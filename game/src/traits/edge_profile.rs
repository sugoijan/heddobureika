//! Edge profile generation behavior contract.

use crate::edge_compose::ConnectorSeg;
use crate::edge_profile::EdgeProfileInput;

pub trait EdgeProfileStrategy {
    fn build_segments(&self, input: &EdgeProfileInput) -> Vec<ConnectorSeg>;
}
