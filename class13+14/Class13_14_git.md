Class13_git
================

``` r
blast<-read.delim("mm-second.x.zebrafish.tsv")
colnames(blast) <-c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")

hist(blast$bitscore)
```

![](Class13_14_git_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#install.packages("ggplot2")
library(ggplot2)
ggplot(blast)+
  aes((pident*(qend-qstart)),bitscore)+
  geom_point(alpha=0.1)+
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](Class13_14_git_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
