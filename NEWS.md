# prevR 4.0.0

**Breaking change:**

* Manual fitting of the variogram is no longer available in `krige()` as
  it was relying on `geoR` package was has been removed from the CRAN (#22)

**Other changes:**

* `kde()` now relies on `KernSmooth::bkde2D()` instead of 
  `GenKern::KernSur()` (#21)

# prevR 3.4.1

* DOI added (#12)
* example data (`fdhs`) has been updated using new `CRS` 
  representation (#18)

# prevR 3.4.0

* Added a `NEWS.md` file to track changes to the package (#11)
* Bug fix regarding namespace import (#10)
* `quick.prevR()` has been fixed (#13)
* vignette added (#14)
* pkgdown web site added (#15)

