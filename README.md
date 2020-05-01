Description of Script Framework
================

To illustrate my process, I will use the data from “Nectar accessibility
determines fitness, flower choice and abundance of hoverflies that
provide natural pest control” (Rijn 2016) as an example. The data was
sourced as raw data from datadryad.org, and is summarized in the
following table:

    ##                       species  n mean_time <- mean(time, na.rm = TRUE)
    ## 1              0water control 27                              1.925926
    ## 2        Achillea millefolium 19                              9.815789
    ## 3                  Ammi majus 23                             11.934783
    ## 4          Anthemis tinctoria 29                              4.137931
    ## 5          Borago officinalis 56                             12.428571
    ## 6       Calendula officinalis 22                              2.590909
    ## 7            Centaurea cyanus 51                              9.313725
    ## 8       Chrysanthemum segetum 37                              5.837838
    ## 9           Cichorium intybus 10                              2.800000
    ## 10            Circium arvense 15                              5.833333
    ## 11        Coreopsis tinctoria 21                              7.309524
    ## 12         Coriandrum sativum 52                              7.769231
    ## 13          Cosmos bipinnatus 10                              2.700000
    ## 14          Crepis capillaris 12                              4.291667
    ## 15              Daucus carota 17                             13.029412
    ## 16       Fagopyrum esculentum 69                             11.855072
    ## 17         Foeniculum vulgare 28                              6.875000
    ## 18         Gypsophila elegans 13                             12.038462
    ## 19       Heracleum spondylium 11                             12.318182
    ## 20          Jacobaea vulgaris 24                              6.395833
    ## 21       Leucanthemum vulgare 25                              3.780000
    ## 22         Lotus corniculatus 14                              2.928571
    ## 23        Matricaria recutita 47                              7.021277
    ## 24            Medicago sativa 22                              2.977273
    ## 25           Pastinaca sativa 19                             14.842105
    ## 26     Phacelia tanacetifolia 25                              3.540000
    ## 27           Ranunculus acris 16                              6.593750
    ## 28            sucrose control 48                             10.562500
    ## 29          Tanacetum vulgare 14                              3.892857
    ## 30 Tripleurospermum maritimum 14                              3.642857
    ## 31               Vicia cracca 16                              2.906250
    ## 32              ViciaS sativa 36                             10.305556
    ##    sd_time <- sd(time, na.rm = TRUE) se_error <- sd_time/sqrt(n)
    ## 1                          0.8627294                   0.9369764
    ## 2                          5.6154809                   1.1169500
    ## 3                          6.0326844                   1.0151883
    ## 4                          1.6950377                   0.9040897
    ## 5                          7.8453394                   0.6506037
    ## 6                          1.3419634                   1.0380044
    ## 7                          7.2835162                   0.6817505
    ## 8                          4.3954744                   0.8004048
    ## 9                          0.7888106                   1.5396093
    ## 10                         2.3119771                   1.2570857
    ## 11                         6.4157544                   1.0624314
    ## 12                         3.3113908                   0.6751633
    ## 13                         0.4216370                   1.5396093
    ## 14                         3.7017093                   1.4054646
    ## 15                         6.8817480                   1.1808264
    ## 16                         7.0929239                   0.5861192
    ## 17                         3.7947942                   0.9200925
    ## 18                         4.8881725                   1.3503267
    ## 19                         6.8493530                   1.4679599
    ## 20                         4.8476331                   0.9938135
    ## 21                         2.6263092                   0.9737344
    ## 22                         1.7304640                   1.3012074
    ## 23                         6.3219403                   0.7101688
    ## 24                         1.2953881                   1.0380044
    ## 25                         4.6219664                   1.1169500
    ## 26                         1.9360613                   0.9737344
    ## 27                         5.1968540                   1.2171680
    ## 28                         4.3192112                   0.7027323
    ## 29                         2.1138606                   1.3012074
    ## 30                         1.4064217                   1.3012074
    ## 31                         1.1578536                   1.2171680
    ## 32                         4.8686721                   0.8114454

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

one.v.all
```

    ## [[1]]
    ## [[1]]$D_AD
    ## [1] 4.337466
    ## 
    ## [[1]]$p_value
    ## [1] 0.03728238
    ## 
    ## 
    ## [[2]]
    ## [[2]]$D_AD
    ## [1] 1.183359
    ## 
    ## [[2]]$p_value
    ## [1] 0.2766733
    ## 
    ## 
    ## [[3]]
    ## [[3]]$D_AD
    ## [1] 2.506766
    ## 
    ## [[3]]$p_value
    ## [1] 0.1133583
    ## 
    ## 
    ## [[4]]
    ## [[4]]$D_AD
    ## [1] 5.825755
    ## 
    ## [[4]]$p_value
    ## [1] 0.01579319
    ## 
    ## 
    ## [[5]]
    ## [[5]]$D_AD
    ## [1] 1.845485
    ## 
    ## [[5]]$p_value
    ## [1] 0.1743094
    ## 
    ## 
    ## [[6]]
    ## [[6]]$D_AD
    ## [1] 2.181214
    ## 
    ## [[6]]$p_value
    ## [1] 0.1397043
    ## 
    ## 
    ## [[7]]
    ## [[7]]$D_AD
    ## [1] 0.0006886788
    ## 
    ## [[7]]$p_value
    ## [1] 0.9790638
    ## 
    ## 
    ## [[8]]
    ## [[8]]$D_AD
    ## [1] 0.05246535
    ## 
    ## [[8]]$p_value
    ## [1] 0.8188276
    ## 
    ## 
    ## [[9]]
    ## [[9]]$D_AD
    ## [1] 3.349094
    ## 
    ## [[9]]$p_value
    ## [1] 0.06724202
    ## 
    ## 
    ## [[10]]
    ## [[10]]$D_AD
    ## [1] 3.102195
    ## 
    ## [[10]]$p_value
    ## [1] 0.07818682
    ## 
    ## 
    ## [[11]]
    ## [[11]]$D_AD
    ## [1] 0.2409668
    ## 
    ## [[11]]$p_value
    ## [1] 0.6235087
    ## 
    ## 
    ## [[12]]
    ## [[12]]$D_AD
    ## [1] 9.769239
    ## 
    ## [[12]]$p_value
    ## [1] 0.001774559
    ## 
    ## 
    ## [[13]]
    ## [[13]]$D_AD
    ## [1] 5.255479
    ## 
    ## [[13]]$p_value
    ## [1] 0.02187778
    ## 
    ## 
    ## [[14]]
    ## [[14]]$D_AD
    ## [1] 0.09399416
    ## 
    ## [[14]]$p_value
    ## [1] 0.7591595
    ## 
    ## 
    ## [[15]]
    ## [[15]]$D_AD
    ## [1] 1.53569
    ## 
    ## [[15]]$p_value
    ## [1] 0.2152605
    ## 
    ## 
    ## [[16]]
    ## [[16]]$D_AD
    ## [1] 3.374645
    ## 
    ## [[16]]$p_value
    ## [1] 0.06620682
    ## 
    ## 
    ## [[17]]
    ## [[17]]$D_AD
    ## [1] 2.127382
    ## 
    ## [[17]]$p_value
    ## [1] 0.1446877
    ## 
    ## 
    ## [[18]]
    ## [[18]]$D_AD
    ## [1] 2.523463
    ## 
    ## [[18]]$p_value
    ## [1] 0.112164
    ## 
    ## 
    ## [[19]]
    ## [[19]]$D_AD
    ## [1] 0.7619534
    ## 
    ## [[19]]$p_value
    ## [1] 0.3827179
    ## 
    ## 
    ## [[20]]
    ## [[20]]$D_AD
    ## [1] 0.02422318
    ## 
    ## [[20]]$p_value
    ## [1] 0.8763184
    ## 
    ## 
    ## [[21]]
    ## [[21]]$D_AD
    ## [1] 0.2797677
    ## 
    ## [[21]]$p_value
    ## [1] 0.5968535
    ## 
    ## 
    ## [[22]]
    ## [[22]]$D_AD
    ## [1] 0.7103761
    ## 
    ## [[22]]$p_value
    ## [1] 0.399319
    ## 
    ## 
    ## [[23]]
    ## [[23]]$D_AD
    ## [1] 0.8214137
    ## 
    ## [[23]]$p_value
    ## [1] 0.3647672
    ## 
    ## 
    ## [[24]]
    ## [[24]]$D_AD
    ## [1] 3.773698
    ## 
    ## [[24]]$p_value
    ## [1] 0.05206442
    ## 
    ## 
    ## [[25]]
    ## [[25]]$D_AD
    ## [1] 5.97769
    ## 
    ## [[25]]$p_value
    ## [1] 0.01448797
    ## 
    ## 
    ## [[26]]
    ## [[26]]$D_AD
    ## [1] 1.975738
    ## 
    ## [[26]]$p_value
    ## [1] 0.1598401
    ## 
    ## 
    ## [[27]]
    ## [[27]]$D_AD
    ## [1] 0.0001876165
    ## 
    ## [[27]]$p_value
    ## [1] 0.9890715
    ## 
    ## 
    ## [[28]]
    ## [[28]]$D_AD
    ## [1] 9.911907
    ## 
    ## [[28]]$p_value
    ## [1] 0.001642128
    ## 
    ## 
    ## [[29]]
    ## [[29]]$D_AD
    ## [1] 1.106819
    ## 
    ## [[29]]$p_value
    ## [1] 0.2927745
    ## 
    ## 
    ## [[30]]
    ## [[30]]$D_AD
    ## [1] 3.034697
    ## 
    ## [[30]]$p_value
    ## [1] 0.08150174
    ## 
    ## 
    ## [[31]]
    ## [[31]]$D_AD
    ## [1] 3.290144
    ## 
    ## [[31]]$p_value
    ## [1] 0.06969691
    ## 
    ## 
    ## [[32]]
    ## [[32]]$D_AD
    ## [1] 5.018006
    ## 
    ## [[32]]$p_value
    ## [1] 0.02508504

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
