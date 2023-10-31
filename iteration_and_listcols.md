iteration and listcols
================
Sitian Zhou
2023-10-31

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(12345)
```

``` r
vec_numeric = 1:4
vec_char = c("a", "b", "c", "d")

tibble(
  num = vec_numeric,
  char = vec_char
)
```

    ## # A tibble: 4 × 2
    ##     num char 
    ##   <int> <chr>
    ## 1     1 a    
    ## 2     2 b    
    ## 3     3 c    
    ## 4     4 d

``` r
l = 
  list(
    vec_numeric = 1:5,
    vec_char = LETTERS,
    matrix = matrix(1:10, nrow = 5, ncol = 2),
    summary = summary(rnorm(100))
  )

l$vec_numeric
```

    ## [1] 1 2 3 4 5

``` r
l[[2]]
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.3804 -0.5901  0.4837  0.2452  0.9004  2.4771

``` r
list_norm_samples = 
  list(
    a = rnorm(20, 1, 5),
    b = rnorm(20, 0, 7),
    c = rnorm(20, 20, 1),
    d = rnorm(20, -45, 13)
  )
list_norm_samples$a
```

    ##  [1]  2.1196270 -4.7811167  3.1120926 -5.6237763  1.7054216 -1.6802400
    ##  [7] -0.5580304  8.7805482 -1.2401665  2.6056177 -5.1508612 -5.6202935
    ## [13]  7.3062114  7.5961586  0.5962312 -1.5254490  0.7392320  4.1443031
    ## [19] 11.9000120  0.6549135

mean and sd function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument should be numebrs")
  } else if (length(x) < 2) {
    stop("You need at least 2 numbers to get z scores")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

``` r
mean_and_sd(list_norm_samples$a)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92

``` r
mean_and_sd(list_norm_samples$b)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norm_samples[[i]])
}
```
