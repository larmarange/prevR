# prevR

<!-- badges: start -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/0.1.0/active.svg)](https://www.repostatus.org/#active) 
[![R build status](https://github.com/larmarange/prevR/workflows/R-CMD-check/badge.svg)](https://github.com/larmarange/prevR/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/prevR)](https://cran.r-project.org/web/packages/prevR) 
[![Rdoc](https://www.rdocumentation.org/badges/version/prevR)](https://www.rdocumentation.org/packages/prevR)
[![Downloads](https://cranlogs.r-pkg.org/badges/prevR)](https://cran.r-project.org/package=prevR)
[![DOI](https://www.zenodo.org/badge/6281387.svg)](https://www.zenodo.org/badge/latestdoi/6281387)
<!-- badges: end -->

This package performs spatial estimation of a prevalence surface
or a relative risks surface, using data from a Demographic and Health
Survey (DHS) or an analog survey.

## Methodological paper

* <https://journals.openedition.org/cybergeo/24606>

## Installation

From CRAN:

```
install.packages("prevR")
```

Latest version from GitHub:

```
devtools::install_github("larmarange/prevR")
```

## Some additional code / functions

<https://github.com/larmarange/prevR-extra>


## Updating PO files

Run the folowing command:

```r
library(tools)
update_pkg_po('.')
```

Note: you need to have `gettext-tools` on your computer. 

See <http://stat.ethz.ch/R-manual/R-devel/library/tools/html/update_pkg_po.html>