
<!-- README.md is generated from README.Rmd. Please edit that file -->
metafiler
---------

Taxonomic profile visualizations.

[![Travis-CI Build Status](https://travis-ci.org/aaronwolen/metafiler.svg?branch=master)](https://travis-ci.org/aaronwolen/metafiler) [![codecov](https://codecov.io/gh/aaronwolen/metafiler/branch/master/graph/badge.svg)](https://codecov.io/gh/aaronwolen/metafiler)

Installation
------------

``` r
# install.packages("devtools")
source("http://www.bioconductor.org/biocLite.R")
biocLite("Biobase")
devtools::install_github("aaronwolen/metafiler", build_vignettes = TRUE)
```

Demo
----

``` r
library(metafiler)
data(profiles)
```

``` r
profiles <- add_max_feature(profiles)
profile_barplot(profiles)
```

![](http://i.imgur.com/ZOWT66X.png)

More information can be found in the vignette:

``` r
vignette("metafiler-intro", package = "metafiler")
```
