
<!-- README.md is generated from README.Rmd. Please edit that file -->

# staroddi

<!-- badges: start -->
<!-- badges: end -->

The goal of {staroddi} is:

-   Primarily to provide some convenient functions to read in
    [StarOddi](https://www.star-oddi.com/) data, including metadata
-   Add some conventient function to tabulate and visualize the data

## Installation

You can install the development version of staroddi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("einarhjorleifsson/staroddi")
```

## Example

This is a basic example which shows you how to read in a staroddi file
into R:

``` r
library(staroddi)
## basic example code
dst <- read_dst(system.file("demos/1M9380.DAT", package="staroddi"))
dst
#> # A tibble: 29,963 × 7
#>    dst_id  ncol .rid  time              Temperature Depth path                  
#>  * <chr>  <int> <chr> <chr>             <chr>       <chr> <chr>                 
#>  1 1M9380     4 1     25/06/08 21:47:00 20.580      -1.27 /home/haf/einarhj/r/x…
#>  2 1M9380     4 2     25/06/08 22:07:00 ____        -1.27 /home/haf/einarhj/r/x…
#>  3 1M9380     4 3     25/06/08 22:27:00 ____        -1.27 /home/haf/einarhj/r/x…
#>  4 1M9380     4 4     25/06/08 22:47:00 18.911      -1.27 /home/haf/einarhj/r/x…
#>  5 1M9380     4 5     25/06/08 23:07:00 ____        -1.27 /home/haf/einarhj/r/x…
#>  6 1M9380     4 6     25/06/08 23:27:00 ____        -1.27 /home/haf/einarhj/r/x…
#>  7 1M9380     4 7     25/06/08 23:47:00 17.877      -1.27 /home/haf/einarhj/r/x…
#>  8 1M9380     4 8     26/06/08 00:07:00 ____        -1.27 /home/haf/einarhj/r/x…
#>  9 1M9380     4 9     26/06/08 00:27:00 ____        -1.27 /home/haf/einarhj/r/x…
#> 10 1M9380     4 10    26/06/08 00:47:00 16.052      -1.04 /home/haf/einarhj/r/x…
#> # … with 29,953 more rows
attributes(dst)$meta
#> # A tibble: 14 × 7
#>    id    var                val                  dst_id dstid_source path      n
#>    <chr> <chr>              <chr>                <chr>  <chr>        <chr> <int>
#>  1 #0    Date-time:         15.12.2010 12:01:48  1M9380 Recorder     /hom…    14
#>  2 #1    Recorder:          1M9380               1M9380 Recorder     /hom…    14
#>  3 #2    File type:         1                    1M9380 Recorder     /hom…    14
#>  4 #3    Columns:           4                    1M9380 Recorder     /hom…    14
#>  5 #4    Channels:          2                    1M9380 Recorder     /hom…    14
#>  6 #5    Field separation:  0                    1M9380 Recorder     /hom…    14
#>  7 #6    Decimal point:     1                    1M9380 Recorder     /hom…    14
#>  8 #7    Date def.:         0 1                  1M9380 Recorder     /hom…    14
#>  9 #8    Time def.:         0                    1M9380 Recorder     /hom…    14
#> 10 #9    Channel 1:         Temperature(°C) Tem… 1M9380 Recorder     /hom…    14
#> 11 #10   Channel 2:         Depth(m) Depth(m) 2… 1M9380 Recorder     /hom…    14
#> 12 #11   Reconvertion:      1                    1M9380 Recorder     /hom…    14
#> 13 #19   Line color:        1 2 3 4              1M9380 Recorder     /hom…    14
#> 14 #30   Trend Type Number: 1                    1M9380 Recorder     /hom…    14
```
