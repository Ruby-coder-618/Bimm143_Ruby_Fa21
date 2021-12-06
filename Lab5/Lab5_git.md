Lab5
================

``` r
library(ggplot2)


ggplot(cars) +
  aes(x=speed, y=dist)+
  geom_point()
```

![](Lab5_git_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)
```

    ##         Gene Condition1 Condition2      State
    ## 1      A4GNT -3.6808610 -3.4401355 unchanging
    ## 2       AAAS  4.5479580  4.3864126 unchanging
    ## 3      AASDH  3.7190695  3.4787276 unchanging
    ## 4       AATF  5.0784720  5.0151916 unchanging
    ## 5       AATK  0.4711421  0.5598642 unchanging
    ## 6 AB015752.4 -3.6808610 -3.5921390 unchanging

``` r
ggplot(genes) + 
    aes(x=Condition1, y=Condition2)+
    geom_point()
```

![](Lab5_git_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
p <- ggplot(genes) + 
    aes(x=Condition1, y=Condition2, col=State) +
    geom_point()
```

``` r
p + scale_colour_manual( values=c("blue","gray","red") )
```

![](Lab5_git_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
