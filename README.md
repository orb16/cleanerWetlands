
# cleanerWetlands

<!-- badges: start -->
<!-- badges: end -->

The goal of cleanerWetlands is to document and make accessible the code we use to clean up council data. 

## Installation

You can install cleanerWetlands using the `devtools` package (install first using `r install.packages("devtools")`). 

``` r
# you will probably need this
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
devtools::install_github("orb16/cleanerWetlands", build_opts = c("--no-resave-data", "--no-manual"))
# might as well restart R here, then load package
require(cleanerWetlands)
```



