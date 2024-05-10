<!-- badges: start -->
<!-- badges: end -->

'ballantine' automates the creation of "ballantine" graphs (also referred to as "commonality analysis"; Ozdemir, 2015). These graphs visualize the amount of shared and unique variance explained by independent variable(s) (up to 4) in the dependent variable through venn diagrams. This method utilizes nested OLS multiple regression models. 

## Installation

You can install the development version of ballantine from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SarcylY/ballantine")
```

## Example

The example below shows how to use the function (using the base R dataset mtcars)

``` r
library(ballantine)

ballantine("mtcars", c("cyl", "hp", "disp"))
```

## References

Ozdemir, B. (2015). Partitioning Variance Into Constituents in Multiple Regression Models: Commonality Analysis. In Quantitative Psychology Research (Vol. 89, pp. 395–405). Springer International Publishing. https://doi.org/10.1007/978-3-319-07503-7_25
