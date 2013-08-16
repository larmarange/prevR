rmdir prevR.Rcheck /S /Q
R CMD check prevR 
R CMD build prevR --compact-vignettes
R CMD build prevR
R CMD INSTALL --build  prevR

REM R CMD check --as-cran prevR_2.3.tar.gz