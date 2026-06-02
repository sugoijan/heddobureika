//! Geometry-aware z-order ("fitting depth") reordering.
//!
//! Both interactive z-order gestures — *bring forward* on drag-start and *send
//! to fitting depth* on shake — share one invariant: within a set of
//! overlapping groups, a group that is (near-)completely hidden behind a larger
//! one is lifted above it, so nothing disappears. Partial overlap of
//! similar-size pieces is left alone.
//!
//! Occlusion is judged from rectangular axis-aligned bounding boxes in world
//! pose-mm space (no SVG outlines), so the computation is deterministic and
//! identical on client and server — the server stays authoritative for z-order
//! without any protocol change. This is best-effort: jigsaw tab/notch shapes
//! and a multi-piece group's concave silhouette are approximated by the union
//! bounding box.

use crate::ids::GroupId;

/// Fraction of a group's bounding box that must be covered by another group's
/// bounding box for it to count as "hidden" behind that group. Tunable.
pub const COVERAGE_THRESHOLD: f32 = 0.90;

/// Absolute slop (pose-mm) for AABB intersection/containment comparisons.
pub const AABB_EPS: f32 = 1.0e-4;

/// Axis-aligned bounding box in world pose-mm space.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Aabb {
    pub min_x: f32,
    pub min_y: f32,
    pub max_x: f32,
    pub max_y: f32,
}

impl Aabb {
    /// An inverted/empty box; `union` with it is the identity.
    pub fn empty() -> Self {
        Self {
            min_x: f32::INFINITY,
            min_y: f32::INFINITY,
            max_x: f32::NEG_INFINITY,
            max_y: f32::NEG_INFINITY,
        }
    }

    pub fn from_center_half(cx: f32, cy: f32, hx: f32, hy: f32) -> Self {
        let hx = hx.abs();
        let hy = hy.abs();
        Self {
            min_x: cx - hx,
            min_y: cy - hy,
            max_x: cx + hx,
            max_y: cy + hy,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.max_x < self.min_x || self.max_y < self.min_y
    }

    pub fn union(self, other: Aabb) -> Aabb {
        if self.is_empty() {
            return other;
        }
        if other.is_empty() {
            return self;
        }
        Aabb {
            min_x: self.min_x.min(other.min_x),
            min_y: self.min_y.min(other.min_y),
            max_x: self.max_x.max(other.max_x),
            max_y: self.max_y.max(other.max_y),
        }
    }

    pub fn width(&self) -> f32 {
        (self.max_x - self.min_x).max(0.0)
    }

    pub fn height(&self) -> f32 {
        (self.max_y - self.min_y).max(0.0)
    }

    pub fn area(&self) -> f32 {
        if self.is_empty() {
            0.0
        } else {
            self.width() * self.height()
        }
    }

    /// True when the boxes overlap (touching within `AABB_EPS` counts as not
    /// overlapping, so abutting pieces aren't treated as occluding).
    pub fn intersects(&self, other: &Aabb) -> bool {
        self.min_x < other.max_x - AABB_EPS
            && self.max_x > other.min_x + AABB_EPS
            && self.min_y < other.max_y - AABB_EPS
            && self.max_y > other.min_y + AABB_EPS
    }

    pub fn intersection_area(&self, other: &Aabb) -> f32 {
        if self.is_empty() || other.is_empty() {
            return 0.0;
        }
        let w = (self.max_x.min(other.max_x) - self.min_x.max(other.min_x)).max(0.0);
        let h = (self.max_y.min(other.max_y) - self.min_y.max(other.min_y)).max(0.0);
        w * h
    }

    /// Fraction of `inner`'s area that lies inside `self` (in `[0, 1]`).
    pub fn coverage_of(&self, inner: &Aabb) -> f32 {
        let area = inner.area();
        if area <= 0.0 {
            0.0
        } else {
            (self.intersection_area(inner) / area).clamp(0.0, 1.0)
        }
    }
}

/// Which extreme the manipulated group is biased toward.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Gesture {
    /// Drag-start: bring the group as far front as the invariant allows.
    BringForward,
    /// Shake: send the group as far back as the invariant allows.
    SendBackward,
}

/// Compute a new full `z_order` that places group `g` at its "fitting depth",
/// honoring the invariant that no group is left (near-)completely hidden behind
/// a larger overlapping one. Returns the new order, or `None` if unchanged.
///
/// - `z_order` is the current order, back-to-front.
/// - `aabb_by_pos` is the world AABB of each group, aligned with `z_order`.
/// - `g_pos` is `g`'s index within `z_order`.
///
/// The two gestures differ in scope (matching the product requirements):
/// - [`Gesture::BringForward`] (drag-start) only ever moves `g` *forward*: as
///   far toward the front as it can go without newly (near-)completely hiding a
///   piece, and never backward. It does not reorder any other group — so a piece
///   `g` is already hiding stays hidden until a shake drops `g` behind it.
/// - [`Gesture::SendBackward`] (shake) is cluster-local: it only reorders groups
///   whose bounding box overlaps `g`'s, sending `g` as far back as possible
///   without hiding it and re-sorting the overlap cluster so nothing smaller
///   stays buried.
///
/// Pure and deterministic: ties broken by `(area, group_id)` via `f32::total_cmp`.
pub(crate) fn reorder_for_fitting_depth(
    z_order: &[GroupId],
    aabb_by_pos: &[Aabb],
    g_pos: usize,
    gesture: Gesture,
) -> Option<Vec<GroupId>> {
    let n = z_order.len();
    if g_pos >= n || aabb_by_pos.len() != n {
        return None;
    }
    match gesture {
        Gesture::BringForward => bring_forward(z_order, aabb_by_pos, g_pos),
        Gesture::SendBackward => send_backward(z_order, aabb_by_pos, g_pos),
    }
}

/// `(area, group_id)` ordering: the smaller-rank group is the one protected
/// (lifted on top) when covered.
fn rank_lt(z_order: &[GroupId], aabb_by_pos: &[Aabb], a: usize, b: usize) -> bool {
    let aa = aabb_by_pos[a].area();
    let ba = aabb_by_pos[b].area();
    match aa.total_cmp(&ba) {
        core::cmp::Ordering::Less => true,
        core::cmp::Ordering::Greater => false,
        core::cmp::Ordering::Equal => z_order[a].as_u32() < z_order[b].as_u32(),
    }
}

/// Drag-start: move `g` forward only — to the front, but stopping just behind
/// the nearest piece in front of it that it would (near-)completely cover, so
/// the move never newly hides anything. `g` is never moved backward and no other
/// group is reordered (a piece `g` already hides stays hidden).
fn bring_forward(z_order: &[GroupId], aabb_by_pos: &[Aabb], g_pos: usize) -> Option<Vec<GroupId>> {
    let n = z_order.len();
    let g = z_order[g_pos];
    let g_aabb = aabb_by_pos[g_pos];

    // The closest piece in front of `g` that `g` would mostly cover (and that
    // ranks below it). `g` must stay behind it; everything between is fair game.
    let blocker = ((g_pos + 1)..n).find(|&i| {
        g_aabb.coverage_of(&aabb_by_pos[i]) >= COVERAGE_THRESHOLD
            && rank_lt(z_order, aabb_by_pos, i, g_pos)
    });
    let blocker_group = blocker.map(|i| z_order[i]);

    // Reinsert `g` just behind the blocker (or at the absolute front if none).
    let mut result: Vec<GroupId> = Vec::with_capacity(n);
    let mut inserted = false;
    for (i, &gid) in z_order.iter().enumerate() {
        if i == g_pos {
            continue;
        }
        if Some(gid) == blocker_group {
            result.push(g);
            inserted = true;
        }
        result.push(gid);
    }
    if !inserted {
        result.push(g);
    }

    if result == z_order {
        None
    } else {
        Some(result)
    }
}

/// Shake: cluster-local reorder. Sends `g` as far back as the invariant allows
/// (just above anything that would hide it) and re-sorts the overlap cluster so
/// no smaller piece stays buried. Non-overlapping groups keep their slots.
fn send_backward(z_order: &[GroupId], aabb_by_pos: &[Aabb], g_pos: usize) -> Option<Vec<GroupId>> {
    let n = z_order.len();
    let g_aabb = aabb_by_pos[g_pos];

    // Overlap cluster (positions into z_order), in back→front order.
    let cluster: Vec<usize> = (0..n)
        .filter(|&i| i == g_pos || aabb_by_pos[i].intersects(&g_aabb))
        .collect();

    // No overlaps: send to the absolute back (preserves the old shake feel).
    if cluster.len() <= 1 {
        if g_pos == 0 {
            return None;
        }
        let g = z_order[g_pos];
        let mut result = Vec::with_capacity(n);
        result.push(g);
        result.extend(z_order.iter().copied().filter(|&x| x != g));
        return Some(result);
    }

    let m = cluster.len();
    let g_local = cluster.iter().position(|&p| p == g_pos).unwrap();

    // Constraint "a must render above b": a is ≥ COVERAGE_THRESHOLD covered by b
    // and ranks below b. Acyclic (every edge strictly decreases rank).
    let must_be_above = |a: usize, b: usize| -> bool {
        if a == b {
            return false;
        }
        let a_box = aabb_by_pos[cluster[a]];
        let b_box = aabb_by_pos[cluster[b]];
        b_box.coverage_of(&a_box) >= COVERAGE_THRESHOLD
            && rank_lt(z_order, aabb_by_pos, cluster[a], cluster[b])
    };

    // Build the DAG over cluster-local indices: emitting back→front, `a` may be
    // emitted only after every `b` it must sit above.
    let mut prereq_count = vec![0u32; m];
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); m];
    for a in 0..m {
        for b in 0..m {
            if must_be_above(a, b) {
                prereq_count[a] += 1;
                dependents[b].push(a);
            }
        }
    }

    // Kahn's algorithm; emit `g` as early (= as far back) as allowed, otherwise
    // preserve existing order (cluster indices are already back→front).
    let mut emitted = vec![false; m];
    let mut order: Vec<usize> = Vec::with_capacity(m);
    for _ in 0..m {
        let candidates: Vec<usize> = (0..m)
            .filter(|&i| !emitted[i] && prereq_count[i] == 0)
            .collect();
        if candidates.is_empty() {
            return None; // unreachable for a DAG
        }
        let pick = if candidates.contains(&g_local) {
            g_local
        } else {
            *candidates.iter().min().unwrap()
        };
        emitted[pick] = true;
        order.push(pick);
        for &dep in &dependents[pick] {
            prereq_count[dep] = prereq_count[dep].saturating_sub(1);
        }
    }

    // Splice the reordered cluster back into its original slots.
    let mut result = z_order.to_vec();
    for (slot, &local) in order.iter().enumerate() {
        result[cluster[slot]] = z_order[cluster[local]];
    }

    if result == z_order {
        None
    } else {
        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn b(min_x: f32, min_y: f32, max_x: f32, max_y: f32) -> Aabb {
        Aabb {
            min_x,
            min_y,
            max_x,
            max_y,
        }
    }

    #[test]
    fn coverage_fully_inside_is_one() {
        let outer = b(0.0, 0.0, 10.0, 10.0);
        let inner = b(2.0, 2.0, 4.0, 4.0);
        assert!((outer.coverage_of(&inner) - 1.0).abs() < 1e-5);
        assert!(outer.intersects(&inner));
    }

    #[test]
    fn coverage_partial_and_disjoint() {
        let a = b(0.0, 0.0, 10.0, 10.0);
        // Half-covered box.
        let half = b(5.0, 0.0, 15.0, 10.0);
        assert!((a.coverage_of(&half) - 0.5).abs() < 1e-5);
        // Disjoint.
        let far = b(100.0, 100.0, 110.0, 110.0);
        assert!(!a.intersects(&far));
        assert_eq!(a.coverage_of(&far), 0.0);
    }

    // z_order indices double as group ids in these pure-ordering tests.
    fn gids(ids: &[u32]) -> Vec<GroupId> {
        ids.iter().map(|&i| GroupId(i)).collect()
    }

    #[test]
    fn bring_forward_does_not_reveal_already_hidden_piece() {
        // z = [small 1 (back), big 0 (front)]: big is already hiding small.
        // Re-grabbing big must NOT reveal small — drag-start only moves forward.
        let z = gids(&[1, 0]);
        let aabbs = vec![b(2.0, 2.0, 4.0, 4.0), b(0.0, 0.0, 10.0, 10.0)];
        assert!(
            reorder_for_fitting_depth(&z, &aabbs, 1, Gesture::BringForward).is_none(),
            "re-grabbing the covering piece leaves the hidden one hidden"
        );
    }

    #[test]
    fn bring_forward_stays_behind_a_covered_piece_in_front() {
        // z = [big 0 (back), far 1, small 2 (front)]: small (covered by big) is
        // in front of big. Bringing big forward moves it past the non-covered
        // `far` group but stops behind small, so small stays visible.
        let z = gids(&[0, 1, 2]);
        let aabbs = vec![
            b(0.0, 0.0, 10.0, 10.0),
            b(100.0, 0.0, 110.0, 10.0),
            b(2.0, 2.0, 4.0, 4.0),
        ];
        let out = reorder_for_fitting_depth(&z, &aabbs, 0, Gesture::BringForward).unwrap();
        let big = out.iter().position(|g| g.as_u32() == 0).unwrap();
        let far = out.iter().position(|g| g.as_u32() == 1).unwrap();
        let small = out.iter().position(|g| g.as_u32() == 2).unwrap();
        assert!(big > far, "big moves in front of the non-covered group");
        assert!(small > big, "but stops behind the small piece it would cover");
    }

    #[test]
    fn send_backward_keeps_group_above_its_container() {
        // Group 0 = big container (front), group 1 = small inside it (back).
        let z = gids(&[1, 0]);
        let aabbs = vec![b(2.0, 2.0, 4.0, 4.0), b(0.0, 0.0, 10.0, 10.0)];
        // Shake the SMALL group (pos 0 in z = group 1) to fitting depth.
        let out = reorder_for_fitting_depth(&z, &aabbs, 0, Gesture::SendBackward).unwrap();
        let small = out.iter().position(|g| g.as_u32() == 1).unwrap();
        let big = out.iter().position(|g| g.as_u32() == 0).unwrap();
        assert!(small > big, "shaken small group must not be hidden by its container");
    }

    #[test]
    fn isolated_group_goes_to_extreme() {
        let z = gids(&[0, 1, 2]);
        // Group 1 overlaps nobody.
        let aabbs = vec![
            b(0.0, 0.0, 5.0, 5.0),
            b(100.0, 100.0, 105.0, 105.0),
            b(1.0, 1.0, 4.0, 4.0),
        ];
        let fwd = reorder_for_fitting_depth(&z, &aabbs, 1, Gesture::BringForward).unwrap();
        assert_eq!(fwd.last().unwrap().as_u32(), 1, "isolated bring-forward → front");
        let back = reorder_for_fitting_depth(&z, &aabbs, 1, Gesture::SendBackward).unwrap();
        assert_eq!(back.first().unwrap().as_u32(), 1, "isolated send-backward → back");
    }

    #[test]
    fn noop_when_already_at_fitting_depth() {
        // Small already above big; bringing big forward still must keep small on
        // top, but big is already at the back, so nothing changes.
        let z = gids(&[0, 1]);
        let aabbs = vec![b(0.0, 0.0, 10.0, 10.0), b(2.0, 2.0, 4.0, 4.0)];
        assert!(reorder_for_fitting_depth(&z, &aabbs, 0, Gesture::BringForward).is_none());
    }

    #[test]
    fn similar_size_partial_overlap_not_reordered() {
        // Two similar boxes overlapping ~30%: neither is "hidden", so a
        // bring-forward of the front one is a no-op (only its slot, unchanged).
        let z = gids(&[0, 1]);
        let aabbs = vec![b(0.0, 0.0, 10.0, 10.0), b(7.0, 0.0, 17.0, 10.0)];
        // Front group (pos 1) brought forward: already frontmost in cluster.
        assert!(reorder_for_fitting_depth(&z, &aabbs, 1, Gesture::BringForward).is_none());
    }

    #[test]
    fn send_backward_keeps_non_cluster_groups_in_place() {
        // z (back→front) = [small 2, far 1, big 0]. Shaking the big group is
        // cluster-local: it must reorder only {big, small} and leave the
        // non-overlapping group 1 in its slot.
        let z = gids(&[2, 1, 0]);
        let aabbs = vec![
            b(2.0, 2.0, 4.0, 4.0),
            b(100.0, 0.0, 110.0, 10.0),
            b(0.0, 0.0, 10.0, 10.0),
        ];
        let out = reorder_for_fitting_depth(&z, &aabbs, 2, Gesture::SendBackward).unwrap();
        let pos1 = out.iter().position(|g| g.as_u32() == 1).unwrap();
        assert_eq!(pos1, 1, "non-overlapping group keeps its slot");
        let big = out.iter().position(|g| g.as_u32() == 0).unwrap();
        let small = out.iter().position(|g| g.as_u32() == 2).unwrap();
        assert!(small > big, "shaken big group drops below the small one it covered");
    }

    #[test]
    fn bring_forward_puts_group_at_absolute_front() {
        // z = [big 0, far 1, small 2]; bring the far (non-overlapping) group 1
        // forward → it must end on top of everything (drag UX: picked piece on
        // top), since it covers nothing.
        let z = gids(&[0, 1, 2]);
        let aabbs = vec![
            b(0.0, 0.0, 10.0, 10.0),
            b(100.0, 0.0, 110.0, 10.0),
            b(2.0, 2.0, 4.0, 4.0),
        ];
        let out = reorder_for_fitting_depth(&z, &aabbs, 1, Gesture::BringForward).unwrap();
        assert_eq!(out.last().unwrap().as_u32(), 1, "picked piece goes to absolute front");
    }
}
