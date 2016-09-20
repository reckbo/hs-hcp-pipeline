# HCP Diffusion Preprocessing Pipeline

## Runtime Requirements

* [FSL](http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FslInstallation)

## Compile

Standard haskell install using cabal or stack:

    # Cabal
    cabal configure
    cabal build

    # Stack
    stack init
    stack setup
    stack build

## Run

   Make `hcp.cfg` (Use `hcp.cfg.example` as a template) and then run
   `hcp`.  This will create the output in `hcp-output`.


## Not implemented

 - GdCoeffs : assumed to be `NONE`
