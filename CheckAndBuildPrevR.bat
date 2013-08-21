rmdir prevR.Rcheck /S /Q
REM R CMD check prevR   Not do it before building ==> It creates a non standard file at top level of the package
R CMD build prevR --compact-vignettes
R CMD INSTALL --build  prevR

R CMD check --as-cran prevR_2.7.tar.gz