# Assumptions <br />
There are some assumptions about the data that are inherent to FHD and its mathematical basis. Documented below are these assumptions and their consequences. <br />

## Time-based <br />
PSFs are calculated only once per observation. Sources will move through the beam over the course of a given snapshot, but the assumption is that the sky is stationary. This leads to small discontinuities visible in waterfall plots. Choose snapshots shorter than about 2 minutes to minimize the discontinuity between adjacent snapshots. Also, phase to the zenith at the center of the snapshot.

## W-projection <br />
FHD does not employ any w-projection or w-stacking algorithms. Integration from FHD needs to be performed in image space to avoid decorrelation as the sky moves. In addition, this means that altitude variations in elements are not properly accounted for, and thus the array must be relatively coplanar.

## Source catalog <br />
Sources are currently assumed to be point sources, and extended sources and diffuse emission are assumed to be point-source component models. The spectral slope is assumed to be a power law, with one general spectral slope for all sources in the catalog (future implementations will include gaussian/shapelet models and varied spectral slope).

## Cosmological <br />
Maybe Bryna can illuminate. <br />

