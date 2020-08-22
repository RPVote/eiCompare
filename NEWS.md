# eiCompare 3.0.0

## General package changes

* Code has been refactored significantly, aligning with modern R packaging (https://r-pkgs.org)
* Roxygen2 now used for documentation (#27, @aridf)
* Update readme, add contributing guide
* Implement Binder instance
* Remove all warnings and notes from `RCMDCHECK()`
* Reduce package dependencies

### New additions:

**Geocoding**:

- Functions for preprocessing addresses.
- Functions for calling Census and Opencage APIs.
- Implemented parallel processing of geocoding API calls.
- Functions for mapping results of geocoding.
- Geocoding vignette/tutorial.

**Bayesian Improved Surname Geocoding (BISG)**

- Functions for manipulating Census geographic area columns.
- Functions for merging Census and precinct data to voter files.
- Wrapper function that cleans and manipulates surnames to maximize the accuracy of BISG.
- BISG vignette/tutorial.

**Ecological Inference (EI)** 

- Functions for preprocessing election data.
- Functions to automate basic descriptive analyses.
- Refactor RxC and Iterative EI functions.
- Summary and plot methods for comparing EI estimates.
- Probability density plots for EI estimates.
- Implemented parallel processing in EI execution.
- Diagnostic plot outputs for Bayesian MCMC estimation of RxC EI.
- Vignettes/tutorials for EI, parallel processing and data vizualizations

**Performance Analysis**

- Performance analysis functions for using data from previous elections to predict turnout for different electoral maps.
- Performance analysis vignette/tutorial.