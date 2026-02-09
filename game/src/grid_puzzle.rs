//! Unified grid puzzle definition (topology + shaper + cached geometry).

use std::num::NonZeroU32;

use crate::grid_shape::{GridJigsawShaper, GridShapeBuildError, GridShapeCache, GridShapeSettings};
use crate::shape::TopologyShaper;
use crate::topology::GridTopology;
use crate::units::LengthMm;

#[derive(Clone, Debug, PartialEq)]
pub struct GridPuzzleDefinition {
    pub topology: GridTopology,
    pub shaper: GridJigsawShaper,
    pub shape_settings: GridShapeSettings,
    pub shape_seed: u32,
    pub piece_width: LengthMm,
    pub piece_height: LengthMm,
    pub shape_cache: GridShapeCache,
}

impl GridPuzzleDefinition {
    pub fn new(
        cols: NonZeroU32,
        rows: NonZeroU32,
        piece_width: LengthMm,
        piece_height: LengthMm,
        shape_seed: u32,
        shape_settings: GridShapeSettings,
    ) -> Result<Self, GridShapeBuildError> {
        let topology = GridTopology::new(cols, rows);
        let shaper = GridJigsawShaper;
        let shape_cache = shaper.build_cache(
            &topology,
            piece_width,
            piece_height,
            shape_seed,
            &shape_settings,
        )?;

        Ok(Self {
            topology,
            shaper,
            shape_settings,
            shape_seed,
            piece_width,
            piece_height,
            shape_cache,
        })
    }

    pub fn rebuild_shapes(&mut self) -> Result<(), GridShapeBuildError> {
        self.shape_cache = self.shaper.build_cache(
            &self.topology,
            self.piece_width,
            self.piece_height,
            self.shape_seed,
            &self.shape_settings,
        )?;
        Ok(())
    }
}
