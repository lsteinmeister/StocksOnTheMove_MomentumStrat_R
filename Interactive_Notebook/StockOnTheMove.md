# Intro

“Stocks on the Move” Clenow (2015) describes a momentum equity strategy,
which we implement in the following. No part of this document should be
viewed as investment advice. Further, this document does not implement a
backtest. See Clenow (2015) for that.

Further, note that this document implements the presented strategy step
by step in a “quick and dirty” way. This means that this implementation
isn’t very robust but it allows for experimentation.

# Setup

We define the date range of historic data to be downloaded.

    end_date = Sys.Date()
    start_date = end_date - years(2)

## Get Data

Using the “yfR” package, we obtain relevant ticker data from Yahoo
Finance. Here, as in Clenow (2015), we define the S&P500 as our trading
universe.

    SP500_data = yf_collection_get("SP500", first_date = start_date, 
                        last_date = end_date, be_quiet = T)%>% data.table

    ## 

    ## ── Fetching price collection for SP500 ─────────────────────────────────────────

    ## ✔ Got SP500 composition with 503 rows

    SP500_index = yf_get("^SP500TR", first_date = start_date, 
                        last_date = end_date, be_quiet = T)%>% data.table
    tail(SP500_data)

    ##    ticker   ref_date price_open price_high price_low price_close  volume
    ##    <char>     <Date>      <num>      <num>     <num>       <num>   <num>
    ## 1:    ZTS 2025-03-17     162.21     165.82    161.78      165.17 1982900
    ## 2:    ZTS 2025-03-18     165.12     165.42    163.24      164.43 1879300
    ## 3:    ZTS 2025-03-19     164.03     164.92    162.84      163.75 1667200
    ## 4:    ZTS 2025-03-20     163.83     164.14    162.25      163.16 2026600
    ## 5:    ZTS 2025-03-21     162.88     163.04    160.91      163.03 2886900
    ## 6:    ZTS 2025-03-24     162.27     165.48    161.70      163.17 1817400
    ##    price_adjusted ret_adjusted_prices ret_closing_prices cumret_adjusted_prices
    ##             <num>               <num>              <num>                  <num>
    ## 1:         165.17        0.0242465819       0.0242465819               1.016478
    ## 2:         164.43       -0.0044802658      -0.0044802658               1.011924
    ## 3:         163.75       -0.0041354540      -0.0041354540               1.007739
    ## 4:         163.16       -0.0036030311      -0.0036030311               1.004108
    ## 5:         163.03       -0.0007967938      -0.0007967938               1.003308
    ## 6:         163.17        0.0008587339       0.0008587339               1.004170

    tail(SP500_index)

    ##      ticker   ref_date price_open price_high price_low price_close volume
    ##      <char>     <Date>      <num>      <num>     <num>       <num>  <num>
    ## 1: ^SP500TR 2025-03-17   12407.81   12557.36  12397.94    12494.82      0
    ## 2: ^SP500TR 2025-03-18   12449.94   12449.94  12324.96    12362.16      0
    ## 3: ^SP500TR 2025-03-19   12401.25   12583.90  12378.85    12495.74      0
    ## 4: ^SP500TR 2025-03-20   12434.79   12575.93  12404.03    12469.97      0
    ## 5: ^SP500TR 2025-03-21   12399.48   12487.21  12338.81    12480.59      0
    ## 6: ^SP500TR 2025-03-24   12591.94   12717.59  12591.94    12700.91      0
    ##    price_adjusted ret_adjusted_prices ret_closing_prices cumret_adjusted_prices
    ##             <num>               <num>              <num>                  <num>
    ## 1:       12494.82        0.0064876930       0.0064876930               1.468676
    ## 2:       12362.16       -0.0106172120      -0.0106172120               1.453083
    ## 3:       12495.74        0.0108055612       0.0108055612               1.468784
    ## 4:       12469.97       -0.0020623434      -0.0020623434               1.465755
    ## 5:       12480.59        0.0008516554       0.0008516554               1.467003
    ## 6:       12700.91        0.0176530369       0.0176530369               1.492900

# Identifying Momentum

We identify momentum calculating the slope of an exponential regression
on the daily prices (this equals a linear regression on the log.
prices). The slope is adjusted by the coefficient of determination,
favoring steady increases over eradic price movements.

    # compute log prices
    SP500_data[,log_price_close := log(price_adjusted)]
    # transform into wide format with tickers as columns and the log closing prices as entries
    SP500_logPrice = dcast(SP500_data, ref_date ~ ticker, value.var = "log_price_close")
    # change variable type for date from string to Date
    SP500_logPrice[,ref_date := as.Date(ref_date)]

    # length parameter to estimate the 
    B = 90
    # regression quantities
    X = cbind(rep(1, B), seq(from= -(B-1)/2, to =(B-1)/2, length.out = B))
    XTX = crossprod(X)
    # get R2-adjusted slope
    getAdjSlope = function(y){
      #print("next step")
      #print(y)
      beta = as.vector(solve(a = XTX, b = crossprod(X,y)))
      
      #print(beta)
      
      y_hat = as.vector(crossprod(t(X), beta))
      #print(y_hat)

      
      R2 = 1-crossprod(y_hat-mean(y_hat))/crossprod(y-mean(y))
      #print(R2)
      
      #print((exp(beta[2]*250)-1)*R2)
      (exp(beta[2]*250)-1)*R2
    }
    # reorder date to ascending 
    SP500_logPrice = SP500_logPrice[order(rank(ref_date))]
    # iterate over columns and obtain adjusted slopws
    regr.coef.lst = frollapply(SP500_logPrice[,-1], n = B, FUN = getAdjSlope)
    # convert list into data.table
    SP500_coeff = data.table(SP500_logPrice$ref_date, do.call(cbind, regr.coef.lst))
    # set column names 
    colnames(SP500_coeff) <- colnames(SP500_logPrice)

    tail(SP500_coeff[,1:10]) #let's have a peak

    ##      ref_date          A        AAPL      ABBV         ABNB       ABT
    ##        <Date>      <num>       <num>     <num>        <num>     <num>
    ## 1: 2025-03-17 -0.0911799 -0.02273505 0.3673750  0.006433978 0.2684846
    ## 2: 2025-03-18 -0.1034560 -0.05175210 0.3602892 -0.006708689 0.2681406
    ## 3: 2025-03-19 -0.1150760 -0.07553214 0.3419737 -0.015945553 0.2669174
    ## 4: 2025-03-20 -0.1304224 -0.09818485 0.3140673 -0.020462171 0.2674138
    ## 5: 2025-03-21 -0.1422582 -0.11304961 0.2767970 -0.012584730 0.2697356
    ## 6: 2025-03-24 -0.1542827 -0.12501319 0.2317734 -0.019374611 0.2704500
    ##          ACGL         ACN       ADBE        ADI
    ##         <num>       <num>      <num>      <num>
    ## 1: -0.1144413 -0.01858879 -0.2138691 0.05003845
    ## 2: -0.1127136 -0.04494637 -0.2110934 0.04951672
    ## 3: -0.1117302 -0.06849565 -0.2087442 0.04437226
    ## 4: -0.1119888 -0.09674108 -0.2079590 0.04304429
    ## 5: -0.1120404 -0.11809092 -0.2068558 0.04198265
    ## 6: -0.1125499 -0.13789278 -0.2050643 0.04668867

After obtaining the slopes, let’s rank them. A rank of 1 here refers to
the best.

    # rank the adjusted exponential slopes
    SP500_ranks = t(apply(na.omit(SP500_coeff[,-1]), MARGIN = 1, FUN = function(x) rank(-x, ties.method = "min")))
    SP500_ranks = data.table(ref_date = na.omit(SP500_coeff)$ref_date,SP500_ranks)

    #rownames(SP500_ranks) = as.character(na.omit(SP500_coeff)$ref_date)

    head(SP500_ranks)

    ##      ref_date     A  AAPL  ABBV  ABNB   ABT  ACGL   ACN  ADBE   ADI   ADM   ADP
    ##        <Date> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1: 2023-09-12   406   214   134    14   379   168   123    12   344    86   258
    ## 2: 2023-09-13   399   248   120    13   378   160   127    12   350    79   250
    ## 3: 2023-09-14   393   269   115    12   379   152   132    11   352    78   249
    ## 4: 2023-09-15   388   285   106    12   377   142   140     9   359    78   239
    ## 5: 2023-09-18   387   299   103    10   373   129   153     9   364    76   229
    ## 6: 2023-09-19   387   307   100    10   372   122   158     9   365    71   218
    ##     ADSK   AEE   AEP   AES   AFL   AIG   AIZ   AJG  AKAM   ALB  ALGN   ALL
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   182   444   435   484   284   199   139   255   162   480    32   437
    ## 2:   184   437   433   484   283   205   137   251   166   488    29   434
    ## 3:   177   426   432   486   285   198   136   255   168   489    27   431
    ## 4:   173   419   431   486   284   192   137   253   171   489    24   428
    ## 5:   171   415   427   486   285   183   136   256   174   491    23   422
    ## 6:   170   404   424   486   281   181   134   257   173   493    20   416
    ##     ALLE  AMAT  AMCR   AMD   AME  AMGN   AMP   AMT  AMZN  ANET  ANSS   AON
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   304    62   401   337   235    87   158   432   152    41   338   313
    ## 2:   319    63   402   356   221    86   150   428   157    42   341   310
    ## 3:   330    63   401   391   203    93   149   428   166    41   345   307
    ## 4:   341    61   398   429   201    98   147   426   175    40   352   299
    ## 5:   348    68   397   461   199   105   137   424   178    39   361   281
    ## 6:   361    70   399   473   204   116   133   422   182    37   368   264
    ##      AOS   APA   APD   APH   APO  APTV   ARE   ATO   AVB  AVGO   AVY   AWK
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   191    70   209   249   114    64   329   335   271    10   227   399
    ## 2:   190    62   197   241   126    66   326   334   280    11   219   397
    ## 3:   186    62   194   226   129    71   325   331   281    10   215   396
    ## 4:   193    63   191   202   126    74   325   333   289    11   216   393
    ## 5:   200    60   181   185   126    78   327   334   291    11   212   392
    ## 6:   216    52   178   176   126    79   325   332   295    13   210   391
    ##     AXON   AXP   AZO    BA   BAC  BALL   BAX   BBY   BDX   BEN  BF-B    BG
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   345   257   388   107   121   369   405   106   215    58   169   252
    ## 2:   329   286   374   100   134   369   413   128   199    59   162   244
    ## 3:   311   301   362    94   133   370   422   143   185    59   156   241
    ## 4:   265   312   351    91   139   375   425   169   174    67   154   230
    ## 5:   207   320   342    92   146   376   441   210   163    70   154   221
    ## 6:   193   331   333    94   156   384   453   241   154    75   153   207
    ##     BIIB    BK  BKNG   BKR  BLDR   BLK   BMY    BR   BRO   BSX    BX   BXP
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   365   203   135   219    34   143   375   299   291   351   101    46
    ## 2:   363   195   131   200    31   145   376   300   292   353    97    41
    ## 3:   363   189   125   190    30   142   375   303   294   351    92    38
    ## 4:   362   182   123   177    29   150   376   302   293   346    90    37
    ## 5:   358   173   120   165    26   155   372   296   290   343    89    34
    ## 6:   359   169   119   152    24   164   367   297   289   341    85    31
    ##        C   CAG   CAH  CARR   CAT    CB  CBOE  CBRE   CCI   CCL  CDNS   CDW
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   470   362   161    55   172   245   260   144   440     2    97   318
    ## 2:   468   360   167    46   155   239   263   118   439     2    96   314
    ## 3:   466   361   179    40   151   218   263   106   434     2    96   316
    ## 4:   465   360   184    36   135   203   259    94   433     2    96   315
    ## 5:   464   355   198    30   124   190   258    85   426     2   100   311
    ## 6:   464   353   214    27   117   185   255    84   425     2   106   305
    ##      CEG    CF   CFG   CHD  CHRW  CHTR    CI  CINF    CL   CLX CMCSA   CME
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   273    59    38   342   457    92   125   202   409   431   261   213
    ## 2:   272    60    40   340   455    99   125   188   407   425   256   215
    ## 3:   268    68    39   338   453   109   123   178   406   424   259   219
    ## 4:   260    71    42   337   449   122   117   165   403   423   254   225
    ## 5:   257    67    44   339   447   121   113   159   403   423   245   228
    ## 6:   246    67    47   337   446   129   110   149   400   426   235   231
    ##      CMG   CMI   CMS   CNC   CNP   COF   COO   COP   COR  COST  CPAY   CPB
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   464    54   433   395   387    43   377   269   164   244   286   396
    ## 2:   459    54   427   389   382    70   388   269   179   243   285   400
    ## 3:   459    53   425   381   377   101   394   267   193   235   280   405
    ## 4:   457    54   424   371   369   133   405   262   218   227   276   407
    ## 5:   456    55   420   366   365   179   417   261   238   222   272   408
    ## 6:   451    63   418   357   355   266   419   253   245   217   270   408
    ##     CPRT   CPT   CRL   CRM  CRWD  CSCO  CSGP   CSX  CTAS  CTRA  CTSH  CTVA
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   193   361   233   201    47   320   170   429   294   198   296   455
    ## 2:   204   365   238   218    57   316   192   435   295   189   296   450
    ## 3:   217   369   237   227    60   317   228   439   293   192   298   451
    ## 4:   234   374   243   244    69   317   264   441   291   186   295   454
    ## 5:   248   374   244   249    69   315   295   439   284   180   287   452
    ## 6:   262   375   251   247    69   312   315   439   279   175   280   448
    ##      CVS   CVX   CZR     D   DAL  DASH   DAY    DD    DE  DECK  DELL   DFS
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   330   285    20   465     8    28   222   228    48    88    40   491
    ## 2:   325   281    18   462     8    28   220   206    47    80    35   494
    ## 3:   323   277    16   460     9    28   222   191    46    77    32   496
    ## 4:   324   278    16   459    10    28   215   178    45    77    32   498
    ## 5:   321   277    15   457    12    27   211   167    45    72    29   498
    ## 6:   317   274    14   455    15    23   189   159    42    68    28   497
    ##       DG   DGX   DHI   DHR   DIS   DLR  DLTR   DOC   DOV   DOW   DPZ   DRI
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   490   360    72   234   424   167   496   312   283   254    25   340
    ## 2:   490   362    74   236   421   154   495   311   284   247    27   346
    ## 3:   490   367    76   239   421   148   495   310   275   252    26   354
    ## 4:   490   378    80   240   411   145   496   305   267   249    26   370
    ## 5:   490   384    87   235   405   131   496   300   264   246    24   386
    ## 6:   491   393    96   229   390   124   496   296   265   236    22   412
    ##      DTE   DUK   DVA   DVN  DXCM    EA  EBAY   ECL    ED   EFX    EG   EIX
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   439   382   145   176   494   408   359   210   416   476   353   319
    ## 2:   438   367   153   171   492   414   358   207   416   478   347   312
    ## 3:   433   359   175   169   493   423   358   199   414   481   335   300
    ## 4:   432   343   195   166   493   427   355   196   413   483   322   282
    ## 5:   425   340   186   161   495   434   350   188   410   485   301   263
    ## 6:   423   327   184   155   495   437   347   191   407   485   259   242
    ##       EL   ELV   EMN   EMR  ENPH   EOG  EPAM  EQIX   EQR   EQT  ERIE    ES
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   459   336   208   300   497   132   117   178   206   115     6   452
    ## 2:   453   335   229   299   497   133    82   177   216   132     6   449
    ## 3:   454   332   230   299   492   134    72   174   220   128     6   449
    ## 4:   455   334   242   301   492   136    60   168   226   125     5   450
    ## 5:   449   335   252   297   489   134    52   166   239   117     5   448
    ## 6:   444   336   269   292   489   130    49   165   244   120     5   445
    ##      ESS   ETN   ETR  EVRG    EW   EXC   EXE  EXPD  EXPE   EXR     F  FANG
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   112   308   430   438   487   341   288   263    23   467   325   183
    ## 2:   108   302   423   441   487   337   288   261    25   465   336   181
    ## 3:    99   295   419   436   487   327   282   257    29   464   346   183
    ## 4:    93   269   409   434   487   320   272   255    31   462   353   185
    ## 5:    91   251   401   428   487   313   266   253    35   462   370   195
    ## 6:    89   227   386   431   487   300   252   254    40   463   380   205
    ##     FAST   FCX   FDS   FDX    FE  FFIV    FI  FICO   FIS  FITB   FOX  FOXA
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   251    42   221   111   442   268   204   307   205    80   211   246
    ## 2:   249    43   210   107   442   273   198   306   226    84   240   257
    ## 3:   250    42   207    97   438   270   195   309   216    83   246   266
    ## 4:   250    41   206    88   435   261   194   309   213    85   258   266
    ## 5:   254    42   202    83   429   260   189   302   208    88   273   274
    ## 6:   256    39   201    76   429   250   195   298   197    91   293   288
    ##      FRT  FSLR  FTNT   FTV    GD  GDDY    GE  GEHC   GEN  GILD   GIS    GL
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   105   461   495   272   297   334   293   479   192   352   354   267
    ## 2:   109   473   496   252   294   328   293   479   163   352   354   266
    ## 3:   105   476   498   236   292   329   290   478   141   349   353   261
    ## 4:   100   477   497   217   290   332   286   478   121   344   356   252
    ## 5:    93   481   497   192   283   333   279   476    99   344   351   247
    ## 6:    90   481   498   174   276   335   275   474    87   343   348   240
    ##      GLW    GM  GNRC  GOOG GOOGL   GPC   GPN  GRMN    GS   GWW   HAL   HAS
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   243   315   298   124   127   458    50   331   259   181   122   218
    ## 2:   259   331   324   139   141   456    49   327   258   208   119   213
    ## 3:   274   340   341   147   150   455    55   326   248   231   121   206
    ## 4:   296   345   363   155   156   453    59   326   236   251   118   198
    ## 5:   306   360   388   160   162   446    71   322   227   267   116   176
    ## 6:   314   371   421   160   163   441    74   320   219   287   111   172
    ##     HBAN   HCA    HD   HES   HIG   HII   HLT  HOLX   HON   HPE   HPQ   HRL
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:    36   363   309   179   287    79   229   400   436    52   189   371
    ## 2:    36   366   305   178   289    81   222   401   440    48   212   375
    ## 3:    37   376   305   182   287    80   214   398   437    48   244   374
    ## 4:    39   385   297   189   281    87   220   396   436    46   271   379
    ## 5:    43   394   275   205   276    95   218   396   433    46   303   377
    ## 6:    45   414   248   213   271   104   212   398   433    43   321   377
    ##     HSIC   HST   HSY  HUBB   HUM   HWM   IBM   ICE  IDXX   IEX   IFF  INCY
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   349   453   356    45   481   155   301   289   130   275   493   326
    ## 2:   349   452   355    45   477   140   298   287   148   270   491   315
    ## 3:   350   450   355    47   477   126   297   283   161   264   491   314
    ## 4:   347   448   358    49   474   119   294   275   180   256   491   311
    ## 5:   353   445   352    50   467   111   288   270   220   241   492   310
    ## 6:   360   442   349    58   460   109   282   267   238   225   492   309
    ##     INTC  INTU  INVH    IP   IPG   IQV    IR   IRM  ISRG    IT   ITW   IVZ
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:    56   220   306   113   489   109   281   226   385   151   142   264
    ## 2:    52   214   309   113   486   112   267   227   395   159   138   279
    ## 3:    50   209   308   114   483   108   251   225   407   170   138   276
    ## 4:    48   212   304   113   482   102   229   222   420   181   141   273
    ## 5:    48   209   293   109   479   101   216   223   436   206   142   286
    ## 6:    46   203   291   107   476   105   192   220   443   224   145   290
    ##        J  JBHT   JBL   JCI  JKHY   JNJ  JNPR   JPM     K   KDP   KEY  KEYS
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   232    68    13   450   160   207   449   118   412   238    19   492
    ## 2:   225    65    14   463   175   201   451   115   410   235    20   493
    ## 3:   229    61    14   472   188   197   456   111   410   232    17   494
    ## 4:   228    65    13   475   208   190   458   104   408   232    17   494
    ## 5:   234    62    14   477   230   182   458   102   407   237    17   494
    ## 6:   228    60    12   480   243   179   458    99   406   233    17   494
    ##      KHC   KIM   KKR  KLAC   KMB   KMI   KMX    KO    KR  KVUE     L  LDOS
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   370   156   165    53   384   253    73   394   368   393   302   279
    ## 2:   368   158   156    56   385   245    72   392   372   391   301   271
    ## 3:   372   158   153    56   383   243    74   389   371   388   302   258
    ## 4:   373   164   151    52   383   237    75   387   368   386   303   238
    ## 5:   371   175   148    56   378   233    73   385   368   383   298   217
    ## 6:   366   206   138    62   376   223    73   383   363   381   294   190
    ##      LEN    LH   LHX   LII   LIN   LKQ   LLY   LMT   LNT   LOW  LRCX  LULU
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   102   108   421   212   292   454    37   373   414   194    65   184
    ## 2:   110   102   436   203   291   458    33   381   411   186    67   174
    ## 3:   113    90   447   196   289   458    34   387   409   181    65   165
    ## 4:   120    83   452   183   287   460    35   389   404   163    58   159
    ## 5:   123    81   455   172   280   460    32   390   398   149    65   157
    ## 6:   131    78   456   166   277   459    34   392   389   132    65   148
    ##      LUV   LVS    LW   LYB   LYV    MA   MAA   MAR   MAS   MCD  MCHP   MCK
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:    71   469   460   276   138   266   425   223    78   383    67   216
    ## 2:   105   469   454   276   209   265   426   211    75   383    77   237
    ## 3:   144   471   452   279   245   265   429   204    73   382    88   238
    ## 4:   187   471   451   288   274   263   430   207    70   382   116   241
    ## 5:   255   473   443   289   308   262   430   201    64   379   156   236
    ## 6:   304   471   436   284   324   260   434   186    61   378   199   230
    ##      MCO  MDLZ   MDT   MET  META   MGM   MHK   MKC  MKTX   MLM   MMC   MMM
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   126   404   445   231    29    84   247   451   413   131   310   166
    ## 2:   135   405   446   233    30    93   274   448   412   124   308   161
    ## 3:   135   403   445   240    31   103   291   448   412   122   313   154
    ## 4:   138   400   439   246    30   105   307   447   410   115   313   152
    ## 5:   141   399   435   243    31   118   317   442   411   114   309   147
    ## 6:   143   396   432   239    32   144   329   440   410   114   306   137
    ##     MNST    MO   MOH   MOS   MPC  MPWR   MRK  MRNA    MS  MSCI  MSFT   MSI
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   386   366   197    44   141    17   420   475   236   153   274   367
    ## 2:   387   361   191    44   136    17   417   475   232   143   282   364
    ## 3:   384   356   200    45   131    22   415   474   213   140   286   365
    ## 4:   384   354   205    47   132    27   412   476   209   130   298   366
    ## 5:   381   346   214    47   140    37   409   475   197   128   307   367
    ## 6:   379   346   215    44   151    53   405   475   194   127   311   362
    ##      MTB  MTCH   MTD    MU  NCLH  NDAQ  NDSN   NEE   NEM  NFLX    NI   NKE
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:    60    11   419   217     7   463   119   398   473    18   389   466
    ## 2:    64     9   418   224    10   460   116   396   471    19   384   466
    ## 3:    67     8   416   212    13   457   112   397   468    21   378   462
    ## 4:    73     7   417   221    15   456   107   395   469    23   367   461
    ## 5:    74     7   416   225    20   453   110   393   466    28   363   459
    ## 6:    81     7   415   232    29   447   115   394   465    30   350   457
    ##      NOC   NOW   NRG   NSC  NTAP  NTRS   NUE  NVDA   NVR   NWS  NWSA  NXPI
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   391    33   174   355    96   163    76     5   175   256   242    22
    ## 2:   393    39   173   370    91   149    61     5   168   253   242    22
    ## 3:   392    43   173   386    84   146    54     5   162   253   233    23
    ## 4:   391    50   170   397    84   143    44     6   157   245   223    21
    ## 5:   389    63   170   418    84   135    38     6   152   240   219    21
    ## 6:   388    72   167   430    83   136    33     6   142   237   222    21
    ##        O  ODFL   OKE   OMC    ON  ORCL  ORLY  OTIS   OXY  PANW  PARA  PAYC
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   441    85   110   478    35    39   324   314   239    16   477   159
    ## 2:   443    78   114   476    34    37   321   318   234    16   480   196
    ## 3:   443    75   124   475    35    36   319   322   234    18   479   260
    ## 4:   442    68   131   472    33    38   318   331   231    20   479   308
    ## 5:   440    53   150   472    33    41   316   338   232    22   480   331
    ## 6:   438    48   161   467    35    41   310   339   226    25   482   344
    ##     PAYX  PCAR   PCG   PEG   PEP   PFE   PFG    PG   PGR    PH   PHM   PKG
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   190    77   347   364   410   443    75   270   346   136    66    90
    ## 2:   176    71   345   359   408   444    69   264   343   111    51    87
    ## 3:   160    70   348   357   404   442    66   256   336    95    49    89
    ## 4:   144    64   340   357   401   440    66   248   328    81    43    95
    ## 5:   122    59   341   349   400   437    61   242   314    75    40    96
    ## 6:   112    56   334   345   395   435    59   234   286    64    36    97
    ##      PLD  PLTR    PM   PNC   PNR   PNW  PODD  POOL   PPG   PPL   PRU   PSA
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   343     3   250   154   195   357   482   104   323   447   230   411
    ## 2:   344     3   246   164   172   357   481   106   330   445   217   409
    ## 3:   339     3   242   167   157   360   480   104   337   444   208   408
    ## 4:   339     3   235   172   148   361   480    97   342   438   210   406
    ## 5:   337     4   231   177   132   357   478    98   345   432   203   406
    ## 6:   330     4   221   209   118   356   478    98   354   427   200   402
    ##      PSX   PTC   PWR  PYPL  QCOM   RCL   REG  REGN    RF   RJF    RL   RMD
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   157   240   277   392   148    15   120    94    31    63    89   483
    ## 2:   147   228   262   380   165    15   117    83    32    58   101   482
    ## 3:   145   211   247   364   176    15   116    79    33    57   110   482
    ## 4:   149   204   224   348   188    14   114    79    34    56   124   481
    ## 5:   143   204   194   347   224    13   108    80    36    54   145   482
    ## 6:   141   202   168   352   249    11   102    77    38    54   171   479
    ##      ROK   ROL   ROP  ROST   RSG   RTX  RVTY  SBAC  SBUX  SCHW   SHW   SJM
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:    61   456   311   316   305   468   381   418   426    21   196   427
    ## 2:    76   461   307   313   304   467   373   419   422    21   183   431
    ## 3:    85   463   312   315   304   465   368   417   420    19   164   441
    ## 4:   110   466   310   316   300   464   365   418   415    19   146   445
    ## 5:   139   469   305   312   292   463   359   419   412    19   119   450
    ## 6:   180   470   302   303   285   462   358   417   409    19   108   450
    ##      SLB  SMCI   SNA  SNPS    SO   SPG  SPGI   SRE   STE  STLD   STT   STX
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:    98     1   248    51   403    57   116   407   177   146   317    95
    ## 2:    94     1   260    55   404    53   123   403   180   142   320    90
    ## 3:    91     1   271    58   400    51   127   395   180   137   320    87
    ## 4:    92     1   283    72   394    51   129   390   179   134   321    89
    ## 5:    94     1   294    77   391    49   138   382   184   133   325    90
    ## 6:    92     1   308    82   385    51   140   373   196   139   326    88
    ##      STZ   SWK  SWKS   SYF   SYK   SYY     T   TAP   TDG   TDY  TECH   TEL
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   187    26   171    30   339   390   428   327   149   350   415    83
    ## 2:   182    26   193    38   332   390   429   322   144   348   424    92
    ## 3:   172    25   210    44   328   385   440   324   139   342   430   102
    ## 4:   162    25   233    55   327   381   446   329   128   338   444   109
    ## 5:   158    25   265    79   326   375   451   328   125   332   454   127
    ## 6:   146    26   299    95   319   374   454   328   121   323   461   150
    ##      TER   TFC   TGT   TJX   TKO   TMO  TMUS   TPL   TPR  TRGP  TRMB  TROW
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:    69   185   474   322   186   265   358     9   498   282   147   150
    ## 2:    88   255   474   317   187   268   351     7   498   277   129   151
    ## 3:   117   288   473   318   187   262   343     7   497   278   120   155
    ## 4:   160   314   470   319   197   257   336     8   495   277   111   161
    ## 5:   226   330   470   318   187   259   329     8   493   278   106   169
    ## 6:   283   342   468   316   187   258   318     8   490   273   103   177
    ##      TRV  TSCO  TSLA   TSN    TT  TTWO   TXN   TXT   TYL   UAL  UBER   UDR
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   376   348     4   137   225    82   333   128   378   103    91   422
    ## 2:   377   342     4   146   223    89   338   130   386   152    85   430
    ## 3:   380   333     4   202   205    98   344   130   390   221    82   435
    ## 4:   380   330     4   199   200   101   350   127   392   279    82   443
    ## 5:   380   323     3   191   193   112   362   130   395   319    82   444
    ## 6:   382   313     3   198   188   125   369   128   397   340    80   449
    ##      UHS  ULTA   UNH   UNP   UPS   URI   USB     V  VICI   VLO   VMC  VRSK
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   462   486   295   140   332    24    27   280   374   180   133   328
    ## 2:   464   485   297   121   339    23    24   278   371   169   122   323
    ## 3:   467   485   296   118   347    20    24   273   366   159   119   321
    ## 4:   473   485   292   103   349    18    22   268   364   158   112   323
    ## 5:   474   484   282    97   356    16    18   268   354   151   107   324
    ## 6:   477   484   278    93   370    16    18   268   351   147   101   322
    ##     VRSN  VRTX   VST   VTR  VTRS    VZ   WAB   WAT   WBA   WBD  WDAY   WDC
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   380   290   262   434   129   446    81   200   485   321    99    49
    ## 2:   379   290   254   432   104   447    73   185   483   333    98    50
    ## 3:   373   284   254   427    81   446    69   163   484   334   100    52
    ## 4:   372   280   247   421    76   437    57   153   484   335    99    53
    ## 5:   369   269   250   421    66   431    51   144   483   336   104    57
    ## 6:   364   261   263   420    55   428    50   135   483   338   113    66
    ##      WEC  WELL   WFC    WM   WMB   WMT   WRB   WSM   WST   WTW    WY  WYNN
    ##    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1:   423   237    93   417   241   303   278   173   188   472    74   471
    ## 2:   420   230   103   415   231   303   275   170   194   470    68   472
    ## 3:   418   223   107   413   224   306   272   171   201   469    64   470
    ## 4:   416   219   108   414   214   306   270   167   211   468    62   467
    ## 5:   414   215   115   413   196   304   271   164   213   468    58   465
    ## 6:   411   208   123   413   183   301   272   157   211   469    57   466
    ##      XEL   XOM   XYL   YUM   ZBH  ZBRA   ZTS
    ##    <int> <int> <int> <int> <int> <int> <int>
    ## 1:   397   224   448   402   488   372   100
    ## 2:   398   202   457   406   489   394    95
    ## 3:   399   184   461   402   488   411    86
    ## 4:   399   176   463   402   488   422    86
    ## 5:   402   168   471   404   488   438    86
    ## 6:   401   162   472   403   488   452    86

# Filters

To avoid investing in a bear market (correlations tend to increase and
we’d like to stay out of the market during such a time) and to ensure
that we invest in momentum stocks, we apply a few filters.

## Stock MA Filter

We ensure that the stock is trading avove the moving average

    # obtain wide data.table with tickers as columns and adjusted prices as values, ascending by date
    SP500_Prices = dcast(SP500_data, ref_date ~ ticker, value.var = "price_adjusted")[order(rank(ref_date))]

    # make sure that the index is trading above the MA
    # number of days for the moving average
    n_ma = 100

    # calculate moving averages with moving windows of defined length
    SP.MA.lst = frollapply(SP500_Prices[,-1], n = n_ma, FUN = mean)
    # convert to data.table
    SP500_MA = data.table(SP500_Prices$ref_date, do.call(cbind, SP.MA.lst))
    colnames(SP500_MA) <- colnames(SP500_Prices)
    print(tail(SP500_MA[,1:10]))

    ##      ref_date        A     AAPL     ABBV     ABNB      ABT     ACGL      ACN
    ##        <Date>    <num>    <num>    <num>    <num>    <num>    <num>    <num>
    ## 1: 2025-03-17 136.3384 235.6723 185.4685 135.5124 120.9990 93.95821 360.2759
    ## 2: 2025-03-18 136.2044 235.4396 185.7590 135.3861 121.0982 93.85948 359.7563
    ## 3: 2025-03-19 136.0943 235.2386 186.0115 135.2902 121.2172 93.76641 359.2831
    ## 4: 2025-03-20 135.9693 235.0771 186.2699 135.2325 121.3210 93.67345 358.5859
    ## 5: 2025-03-21 135.8723 234.9591 186.4912 135.1930 121.4251 93.57541 357.9968
    ## 6: 2025-03-24 135.7925 234.8574 186.7219 135.1454 121.5610 93.51421 357.4606
    ##        ADBE      ADI
    ##       <num>    <num>
    ## 1: 464.9272 217.1602
    ## 2: 463.8638 217.0144
    ## 3: 462.8116 216.8870
    ## 4: 461.8574 216.6969
    ## 5: 460.9013 216.4637
    ## 6: 460.0088 216.3050

    # define data.table with Boolean values indicating whether the filter is passed
    filter_SP500_MA_above = data.table(SP500_Prices[,-1] > SP500_MA[,-1])
    # add date column
    filter_SP500_MA_above = cbind(SP500_Prices[,.(ref_date)], filter_SP500_MA_above)
    tail(filter_SP500_MA_above[,1:10])

    ## Key: <ref_date>
    ##      ref_date      A   AAPL   ABBV   ABNB    ABT   ACGL    ACN   ADBE    ADI
    ##        <Date> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
    ## 1: 2025-03-17  FALSE  FALSE   TRUE  FALSE   TRUE   TRUE  FALSE  FALSE  FALSE
    ## 2: 2025-03-18  FALSE  FALSE   TRUE  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE
    ## 3: 2025-03-19  FALSE  FALSE   TRUE  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE
    ## 4: 2025-03-20  FALSE  FALSE   TRUE  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE
    ## 5: 2025-03-21  FALSE  FALSE   TRUE  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE
    ## 6: 2025-03-24  FALSE  FALSE   TRUE  FALSE   TRUE   TRUE  FALSE  FALSE  FALSE

## Stock Jump Flter

Some of the rankings might still be inflated by stocks with extreme
price jumps. This is not what we mean with momentum. We desire stocks
that steadily move up. Therefore, we exclude stocks with substantial
jumps in the last 90 days.

    # obtain wide data.table with tickers as columns and adjusted returns as values, ascending by date
    SP500_returns = dcast(SP500_data, ref_date ~ ticker, value.var = "ret_adjusted_prices")[order(rank(ref_date))]

    # identify any stock that has jumped more than 15% in the last 90 days.
    # time window in which jumps are not permitted
    n_jmp = 90
    # cutoff for daily returns to be considered a jump 
    jmp_frac = .15

    SP.jmp.lst = frollapply(SP500_returns[,-1], n = n_jmp, FUN = function(x) !any(abs(x)>jmp_frac))
    filter_SP500_jmp = data.table(SP500_returns$ref_date, do.call(cbind, SP.jmp.lst))
    colnames(filter_SP500_jmp) <- colnames(SP500_returns)
    print(tail(filter_SP500_jmp[,1:10]))

    ##      ref_date     A  AAPL  ABBV  ABNB   ABT  ACGL   ACN  ADBE   ADI
    ##        <Date> <num> <num> <num> <num> <num> <num> <num> <num> <num>
    ## 1: 2025-03-17     1     1     1     1     1     1     1     1     1
    ## 2: 2025-03-18     1     1     1     1     1     1     1     1     1
    ## 3: 2025-03-19     1     1     1     1     1     1     1     1     1
    ## 4: 2025-03-20     1     1     1     1     1     1     1     1     1
    ## 5: 2025-03-21     1     1     1     1     1     1     1     1     1
    ## 6: 2025-03-24     1     1     1     1     1     1     1     1     1

## Market Regime Filter

We don’t hold stocks in bear markets. We consider the S&P500 trading
below its 200-day MA to be an indicator of a bear market.

    # obtain wide data.table with index prices  
    SP500_index_prices = dcast(SP500_index, ref_date ~ ticker, value.var = "price_adjusted")
    # ascending by date
    SP500_index_prices=SP500_index_prices[order(rank(ref_date))]

    # check bear market filter
    # window size for moving average
    n_ma = 200

    # compute moving average
    SP.I.MA.lst = frollapply(SP500_index_prices[,-1], n = n_ma, FUN = mean)
    SP500_I_MA = data.table(SP500_index_prices$ref_date, do.call(cbind, SP.I.MA.lst))
    colnames(SP500_I_MA) <- colnames(SP500_index_prices)
    print(tail(SP500_I_MA))

    ##      ref_date ^SP500TR
    ##        <Date>    <num>
    ## 1: 2025-03-17 12573.21
    ## 2: 2025-03-18 12577.67
    ## 3: 2025-03-19 12583.14
    ## 4: 2025-03-20 12588.02
    ## 5: 2025-03-21 12592.88
    ## 6: 2025-03-24 12598.75

    # are we trading above the moving average?
    filter_SP500_I_MA_above = data.table(SP500_index_prices[,-1] > SP500_I_MA[,-1])
    filter_SP500_I_MA_above = cbind(SP500_index_prices[,.(ref_date)], filter_SP500_I_MA_above)
    tail(filter_SP500_I_MA_above)

    ## Key: <ref_date>
    ##      ref_date ^SP500TR
    ##        <Date>   <lgcl>
    ## 1: 2025-03-17    FALSE
    ## 2: 2025-03-18    FALSE
    ## 3: 2025-03-19    FALSE
    ## 4: 2025-03-20    FALSE
    ## 5: 2025-03-21    FALSE
    ## 6: 2025-03-24     TRUE

## combine filetrs

Finally, we combine the results of all the filters into a single
data.table.

    # use only dates for which all filters are available
    filter_dates = as.Date(intersect(na.omit(filter_SP500_MA_above)$ref_date,
                             intersect(na.omit(filter_SP500_jmp)$ref_date,
                             na.omit(filter_SP500_I_MA_above)$ref_date)))
    # make sure the MA filter and the jump filter both apply
    filters_dt = data.table(filter_SP500_MA_above[ref_date %in% filter_dates,2:ncol(filter_SP500_MA_above)] & 
      filter_SP500_jmp[ref_date %in% filter_dates,2:ncol(filter_SP500_jmp)])
    marget_regime_filter_vals = unlist(filter_SP500_I_MA_above[ref_date %in% filter_dates, 2])
    # ensure the market regime filter applies in each row
    filters_dt = data.table(filter_dates, apply(filters_dt, MARGIN = 2, function(c_col){
      c_col & marget_regime_filter_vals}))
    colnames(filters_dt) = colnames(filter_SP500_MA_above)
    tail(filters_dt[,1:10])

    ##      ref_date      A   AAPL   ABBV   ABNB    ABT   ACGL    ACN   ADBE    ADI
    ##        <Date> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
    ## 1: 2025-03-17  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
    ## 2: 2025-03-18  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
    ## 3: 2025-03-19  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
    ## 4: 2025-03-20  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
    ## 5: 2025-03-21  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
    ## 6: 2025-03-24  FALSE  FALSE   TRUE  FALSE   TRUE   TRUE  FALSE  FALSE  FALSE

# Determine Allocations

We size the positions based on risk. To diversify our portfolio, we want
to take similar amounts of risk in an array of momentum stocks. We
measure the risk as Average True Range (ATR), which is defined as
*T**R*<sub>*t*</sub> = *m**a**x*{high<sub>*t*</sub> − low<sub>*t*</sub>, |high<sub>*t*</sub>−close<sub>*t* − 1</sub>|, |close<sub>*t* − 1</sub>−high<sub>*t*</sub>|},
$$ATR^{(n)}\_t = \frac{1}{n} \sum\_{i = 1}^n TR\_{t-i}.$$

    get_ordered_tickers = function(at_date, rankings, filters)
    {
      filters = filters[ref_date == at_date,]
      
      
      # identify permissible tickers
      tickers_perm = colnames(filters[,-1])[sapply(filters[,-1], function(c_col) all(c_col))]
      

      if(length(tickers_perm) == 0) return(c())
      
      rankings = rankings[ref_date == at_date,c(..tickers_perm)]
      
      rankings = rank(rankings, ties.method = "min")
      
      return(names(rankings)[order(rankings)])
    }
    o_ticks = get_ordered_tickers(end_date-1, SP500_ranks, filters_dt)

## Position Sizing

Let’s define the parameters for position sizing:

    risk_factor = 0.005 # 50 bpts per day expected fluctuation
    account_value = 10000
    ATR_n = 20

Time to calculate the average true range:

    Tickers_close = SP500_logPrice = dcast(SP500_data, ref_date ~ ticker, value.var = "price_close")
    Tickers_high = SP500_logPrice = dcast(SP500_data, ref_date ~ ticker, value.var = "price_high")
    Tickers_low = SP500_logPrice = dcast(SP500_data, ref_date ~ ticker, value.var = "price_close")
    date_vec = Tickers_close$ref_date

    Tickers_close = rbind(NA, as.matrix(Tickers_close[,-1]))
    Tickers_close = Tickers_close[-nrow(Tickers_close),]

    Tickers_high = as.matrix(Tickers_high[,-1])
    Tickers_low = as.matrix(Tickers_low[,-1])

    TR = pmax(Tickers_high-Tickers_low, abs(Tickers_high - Tickers_close), abs(Tickers_close - Tickers_low))

    ATR.lst = frollmean(data.table(TR), ATR_n)
    names(ATR.lst) = colnames(TR)
    ATR = cbind(ref_date = date_vec, data.table(do.call(cbind, ATR.lst)))

Based on the ATR, we can now compute position sizes.

    calc_positions = function(at_date, tickers, ATR, account_val, risk_factor = 0.001){
      
      ATR_vec = unlist(ATR[ref_date == at_date, 
                           c(..tickers)])
      
      rel_pos = floor(account_val * risk_factor / ATR_vec)

      
      rel_pos
    }

    calc_positions(end_date-1, o_ticks , ATR, account_value)

    ##  CVS  TPR   PM   RL  TKO  JBL  YUM UBER TMUS FFIV   GE   BA  EQT AMGN  NEM WELL 
    ##    7    4    4    1    2    2    3    4    2    1    2    1    6    1    9    3 
    ## KLAC NFLX GILD  IBM  PGR CRWD  LLY  ABT    C DASH  AJG   DE VRTX  VTR   ED  HWM 
    ##    0    0    5    1    2    0    0    5    5    1    1    0    1    8    5    2 
    ##  BAX CTRA    T FTNT  BSX ABBV  BRO  ICE TTWO  LKQ LRCX  MDT  JNJ  DRI  DGX  NRG 
    ##   14   14   19    4    4    3    6    4    2   12    3    7    5    2    3    2 
    ##  EXC   BK FOXA  AEP  FOX  AIG  MMM   KO  RSG  COR  WEC   FI  ECL PARA CTVA  MCK 
    ##   15    5   10    5   10    6    3   10    3    2    6    2    2   38   10    1 
    ## APTV  ROP  CMS  AON  RTX   VZ  EXE  HES    V  DTE  MMC  DUK   WM EVRG  MKC  KMB 
    ##    7    1    9    2    4   11    4    3    1    6    3    6    3   10    6    4 
    ##  ROL  AWK ORLY  AMT   KR  TDY  JPM EBAY  BKR  AEE   NI CBOE   MA  ELV   SO  ETR 
    ##   14    3    0    2    6    1    1    4    9    6   16    2    1    1    7    6 
    ##   GL  DPZ  STE  CAH CSCO  MAA   MO  LIN  PPL  TDG  LNT  MCD PAYX  ATO  CNP VICI 
    ##    3    0    2    5   10    3   10    1   21    0   11    1    3    4   21   18 
    ##  BMY  WTW  PNW  WRB GRMN   BR VRSK  GLW  PFG  CME  AZO  PFE  KDP  WMB   ES    O 
    ##    8    2    7   10    2    2    2    8    6    2    0   24   15    8    8   11 
    ##  BDX  WFC CSGP  MOH  MOS TRGP   DG  CVX  ADP MDLZ    L OTIS VRSN  XEL  GPC   CB 
    ##    3    5    5    1   12    1    3    3    2    7    8    6    3    9    4    2 
    ##  HAS  HIG   EW  CHD PODD  ESS  ALL  PSX    K  CPT  REG  JCI  TRV INVH  HCA KVUE 
    ##    7    5    8    5    1    2    2    3   45    4    8    5    2   15    1   24 
    ## SBAC MNST  KMI  TEL  MET  AFL   CI  UDR SCHW  VLO EXPD  SLB  TPL  ZBH  TAP TRMB 
    ##    2   10   20    3    5    6    1   12    5    2    4   10    0    4    8    5 
    ##  EOG  NOC  SJM   CL  DVN  TSN  MPC  XOM ACGL  CCI  HSY   GM   EG  OKE  LUV CINF 
    ##    3    0    3    5   11   11    2    4    6    4    2    6    1    4   11    3 
    ## FAST PAYC CHTR  COP    F  HUM  FCX  AES 
    ##    6    1    1    4   38    1    6   24

These are the whole list of momentum stocks with the number of stocks to
purchase. We would start investing from the first to the last, until our
cash runs out. Generally, we probably don’t want to consider all of
these but only the top 20-50.

# References

Clenow, Andreas. 2015. *<span class="nocase">Stocks on the Move: Beating
the Market with Hedge Fund Momentum Strategies</span>*. CreateSpace
Independent Publishing Platform.
