Description of Script Framework
================

To illustrate my process, I will use the data from “Nectar accessibility
determines fitness, flower choice and abundance of hoverflies that
provide natural pest control” (Rijn 2016) as an example. The data was
sourced as raw data from datadryad.org, and is summarized in the
following table:

This is the principle plot that will be used to summarize data in this
meta-analysis, in cases where raw data is available . This plot shows
intraspecific variations, as well as the interspecific or overall
variation (shown in black).
![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The R script cvequality was used to do analysis of equality of
variances. It was done both to analyze equality of all variances to each
other (one test statistic) and of each individual species/strain’s
variance to total variance (number of test statistics = number of
species/strains in the study). Below is an example of these
corresponding tests: “all versus all”, followed by a list of test
statistics of the “one versus all” comparisons.

``` r
#all.v.all test
result = asymptotic_test(x_vector, grouping_vector, 1)
```

    ## [1] "All versus All results of cvequality"

    ## List of 2
    ##  $ D_AD   : num 83.3
    ##  $ p_value: num 1.12e-06

``` r
#one.v.all test using a looped system to compare each species to total CV

for (i in 1:(length(unique(data1A$species))))
{
  n_tmp[[1]] <- n_vector[[i]]
  s_tmp[[1]] <- SD_vector[[i]]
  x_tmp[[1]] <- mean_vector[[i]]
  
  one.v.all[[i]] <- asymptotic_test2(2, n = n_tmp,s = s_tmp, x = x_tmp,1)
}
```

The results if the cvequality test are in the form of D’AD test
statistics (by the Feltz and Miller test 1996) and p-values indicating
deviation from equality of variance. I ran a function to count how many
species grousp had a p-value greater than 0.05: these groups did not
vary significantly in their variance from interspecific variance, and
were considered to have “equal varaince” to total. For groups with a
significant difference in variance from total, I also did a direct
comparison of their CV to total CV to see whhether it was greater.
Finally, I counted the proportion of groups in the study that had EITHER
a p-value of greater than 0.05 OR a p-value lower than 0.05 and a CV
greater than total CV. This gave me the proportion of groups that had
intraspecific variation greater than or equal to interspecific variation
(to test my hypothesis). In the output table presented, these groups
would be those that contain either FALSE-TRUE or TRUE-(\*) outputs. In
this example, 25/32 (78%) of groups have variance greater than or equal
to total (Since equality of variance was tested using cvequality to find
SIGNIFICANT deviation from equality, versus absolute deviation).

    ##  [1] 44.79556 57.20865 50.54708 40.96341 63.12342 51.79508 78.20196 75.29285
    ##  [9] 28.17181 39.63389 87.77254 42.62186 15.61619 86.25342 52.81703 59.83029
    ## [17] 55.19701 40.60463 55.60360 75.79361 69.47908 59.08901 90.03976 43.50922
    ## [25] 31.14091 54.69100 78.81485 40.89194 54.30101 38.60765 39.84012 47.24318

    ##    test_statistics    p_values p_over_five CV_greater
    ## 1     4.3374656396 0.037282380       FALSE      FALSE
    ## 2     1.1833585600 0.276673319        TRUE      FALSE
    ## 3     2.5067664218 0.113358317        TRUE      FALSE
    ## 4     5.8257551437 0.015793187       FALSE      FALSE
    ## 5     1.8454853470 0.174309390        TRUE      FALSE
    ## 6     2.1812136484 0.139704257        TRUE      FALSE
    ## 7     0.0006886788 0.979063765        TRUE      FALSE
    ## 8     0.0524653464 0.818827605        TRUE      FALSE
    ## 9     3.3490938863 0.067242018        TRUE      FALSE
    ## 10    3.1021947941 0.078186819        TRUE      FALSE
    ## 11    0.2409667643 0.623508737        TRUE       TRUE
    ## 12    9.7692391333 0.001774559       FALSE      FALSE
    ## 13    5.2554792398 0.021877776       FALSE      FALSE
    ## 14    0.0939941605 0.759159523        TRUE       TRUE
    ## 15    1.5356901507 0.215260459        TRUE      FALSE
    ## 16    3.3746454577 0.066206823        TRUE      FALSE
    ## 17    2.1273818654 0.144687704        TRUE      FALSE
    ## 18    2.5234633107 0.112164001        TRUE      FALSE
    ## 19    0.7619534204 0.382717895        TRUE      FALSE
    ## 20    0.0242231788 0.876318391        TRUE      FALSE
    ## 21    0.2797676787 0.596853529        TRUE      FALSE
    ## 22    0.7103761176 0.399318978        TRUE      FALSE
    ## 23    0.8214137185 0.364767152        TRUE       TRUE
    ## 24    3.7736976311 0.052064418        TRUE      FALSE
    ## 25    5.9776898095 0.014487967       FALSE      FALSE
    ## 26    1.9757382773 0.159840091        TRUE      FALSE
    ## 27    0.0001876165 0.989071464        TRUE       TRUE
    ## 28    9.9119074896 0.001642128       FALSE      FALSE
    ## 29    1.1068191014 0.292774452        TRUE      FALSE
    ## 30    3.0346974166 0.081501744        TRUE      FALSE
    ## 31    3.2901440419 0.069696908        TRUE      FALSE
    ## 32    5.0180061559 0.025085038       FALSE      FALSE
