rmdir prevR.Rcheck /S /Q
R CMD check prevR 
R CMD build prevR --compact-vignettes
REM attention remplacer si nécessaire le numero de version dans le nom du fichier tar.gz
R CMD INSTALL --build  prevR_2.3.tar.gz
R CMD INSTALL --build  prevR
R CMD check --as-cran prevR_2.3.tar.gz
