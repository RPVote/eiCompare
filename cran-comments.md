## R CMD check results

## Test environments
* local OS X install, R 4.0.2
* ubuntu 18.04 (on travis-ci), R 4.0.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

*  NOTE installed size is  5.7Mb sub-directories of 1Mb or more: doc 4.7Mb

	As described in the .readme, the package is built to facilitate work by users with many different levels of programming experience. This requires detailed, extensive documentation. 

* checking for future file timestamps ... NOTE unable to verify current time

	As indicated [here](https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time) this appears to be an issue within `r-cmd-check`
