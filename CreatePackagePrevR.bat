rmdir prevR /S /Q
rmdir prevR.Rcheck /S /Q
RScript skeletonScripts.R
rm prevR/Read-and-delete-me
rmdir prevR\man /S /Q
mkdir prevR\man
xcopy /Y man\*.* prevR\man\
mkdir prevR\data
xcopy /Y data\*.* prevR\data
mkdir prevR\demo
xcopy /Y demo\*.* prevR\demo
mkdir prevR\vignettes
xcopy /Y vignettes\*.* prevR\vignettes
mkdir prevR\po
mkdir prevR\inst
mkdir prevR\inst\po
mkdir prevR\inst\po\fr
mkdir prevR\inst\po\fr\LC_MESSAGES
cp R-prevR.pot prevR/po/R-prevR.pot
cp R-fr.po prevR/po/R-fr.po
cp R-fr.mo prevR/inst/po/fr/LC_MESSAGES/R-prevR.mo
cp DESCRIPTION  prevR/DESCRIPTION
REM cp LICENCE prevR/LICENCE
cp NAMESPACE prevR/NAMESPACE
cp CITATION  prevR/inst/CITATION
