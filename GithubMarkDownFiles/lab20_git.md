Class_20_git
================

``` r
library(bio3d)
```

``` r
alignment<-read.fasta("Alignments")
```

``` r
help("bio3d")
```

    ## starting httpd help server ... done

``` r
help("heatmap")
```

``` r
matrix<-seqidentity(alignment, normalize=TRUE, similarity=FALSE, ncore=1, nseg.scale=1)
```

``` r
heatmap(matrix)
```

![](lab20_git_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
