
<!-- README.md is generated from README.Rmd. Please edit that file -->

# staroddi

<!-- badges: start -->
<!-- badges: end -->

The goal of {staroddi} is:

-   Primarily to provide some convenient functions to read in
    [Star-Oddi](https://www.star-oddi.com/) data, including metadata
-   Add some conventient function to tabulate and visualize the data

## Installation

You can install the development version of staroddi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("einarhjorleifsson/staroddi")
```

## Example

This is a basic example which shows you how to read a Star-Oddi file
into R:

``` r
library(staroddi)
## basic example code
dst <- read_dst(system.file("demos/1M9380.DAT", package="staroddi"))
dst
#> # A tibble: 29,963 × 7
#>     .rid time                 temp depth dst_id audkenni   utgafa
#>  * <int> <dttm>              <dbl> <dbl> <chr>  <chr>       <int>
#>  1     1 2008-06-25 21:47:00  20.6 -1.27 1M9380 ISL.MILLI.      1
#>  2     2 2008-06-25 22:07:00  NA   -1.27 1M9380 ISL.MILLI.      1
#>  3     3 2008-06-25 22:27:00  NA   -1.27 1M9380 ISL.MILLI.      1
#>  4     4 2008-06-25 22:47:00  18.9 -1.27 1M9380 ISL.MILLI.      1
#>  5     5 2008-06-25 23:07:00  NA   -1.27 1M9380 ISL.MILLI.      1
#>  6     6 2008-06-25 23:27:00  NA   -1.27 1M9380 ISL.MILLI.      1
#>  7     7 2008-06-25 23:47:00  17.9 -1.27 1M9380 ISL.MILLI.      1
#>  8     8 2008-06-26 00:07:00  NA   -1.27 1M9380 ISL.MILLI.      1
#>  9     9 2008-06-26 00:27:00  NA   -1.27 1M9380 ISL.MILLI.      1
#> 10    10 2008-06-26 00:47:00  16.1 -1.04 1M9380 ISL.MILLI.      1
#> # … with 29,953 more rows
attributes(dst)$meta
#> # A tibble: 14 × 3
#>    id    var                val                         
#>    <chr> <chr>              <chr>                       
#>  1 0     Date-time:         15.12.2010 12:01:48         
#>  2 1     Recorder:          1M9380                      
#>  3 2     File type:         1                           
#>  4 3     Columns:           4                           
#>  5 4     Channels:          2                           
#>  6 5     Field separation:  0                           
#>  7 6     Decimal point:     1                           
#>  8 7     Date def.:         0 1                         
#>  9 8     Time def.:         0                           
#> 10 9     Channel 1:         Temperature(°C) Temp(°C) 3 1
#> 11 10    Channel 2:         Depth(m) Depth(m) 2 2       
#> 12 11    Reconvertion:      1                           
#> 13 19    Line color:        1 2 3 4                     
#> 14 30    Trend Type Number: 1
```
