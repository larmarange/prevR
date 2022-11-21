## Test environments

* local R installation: R 4.2.2
* mac OS (on github actions): R-release
* windows (on github actions): R-release, R 3.6
* ubuntu 18.04 (on github actions): R-devel, R-release, R-oldrel

cf. https://github.com/larmarange/prevR/actions

## R CMD check results

0 errors | 0 warnings | 0 or 1 note

* checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    data   3.6Mb
    
  Due to a dataset who was always embed within prevR
  to simplify use by end-users.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages