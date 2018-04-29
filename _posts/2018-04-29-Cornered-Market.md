---
layout: post
subtitle: An Example of Incentives for Collaboration Without Sharing Payoff 
tags: [Strictly domonant strategy, Commodity exchange, Cornered market, R]
comments: true
---




Incentive
---------

Time to time, I give advice to my friends for the bid they would put on
Tehran Commodity Exchange (IME). The auctions are structured as follows:
there is a minimum price, fixed supply and maximum price change of 10
percent. Then the ones who put the highest price will get the commodity
at the price they demand till supply meets demand. So on average lowest
winning bit is lower than highest price.

The models I wrote for this matter are generally based on nth price
common value auctions and I use some model averaging on them. They
normally predict lowest bid or weighted adjusted mean bid with RMSE of
.5 percent. So on average there is a 1-2% gain in that transaction.

Last week, given the data, I ran the models. To my surprise for LLD209
and LD0075 shown estimates that were more than 10 percent of maximum
allowed price. The result of competition among bidders, supported the
results. Observing this, it seemed to me that the market was cornered.
Pretty beautiful thing to be witnessed. I was excited :))

Given the fact that it seemed that the market is cornered, I thought
about the conditioned that it could happen. My thought is considering
this thing as a game theoretic question.

Here are the situation that yield to market being cornered:

-   Following Feb IRRUSD rate was surging. The price of petrochemicals
    in Tehran have very high correlation to their international prices
    in USD terms.

-   Commodity traders and consumers have bought considerable amounts of
    LD and LLD. They tried to first enjoy the rise in the price, second
    avoid buying at higher expected prices.

-   During third week of March to 1st week of April, consumers of LLD
    and LD are at national new year holidays and subsequently demand for
    LD and LLD reduces to lowest amount. So most of the inventory of LLD
    and LD remain the same.

-   And the last and most crucial one. While till third week of April
    IRRUSD was around 49000, the government banned the transactions and
    reduced the price to 42000. As a result most of the traders of LD
    and LLD faced substantial losses.

set up
------

Since this is just for my curiosity, I try to simplify the solutions and
try to be concise.

As I observed, there is an strictly dominant strategy for players. They
are N players, of which k are doing speculative trades. Due to lack of
productions in previous weeks of auctions I simplify and assume that N
is equal to k. Each player has three choices, do not increase the bid
from base price, bid at average increase rate, and bid the maximum
available amount. ( Actually as I said before I consider bidding as nth
price common value auction, I try to not make things more complicated
than necessary).Based on market price and demand of bidders the player
decides on his/her choice. Generally, putting the difference of market
price and auction price aside, In case of bidding at base price, demand
is less than supply. If the average winning bid increases at less than
10 percent, then demand amount of winning bid meet supply. If 10 percent
increase in average bids occur, then demand needs to be at least equal
to supply.

Let see summary of bids in the cases that they have increased from base
price, and let see the tail of data. The data has been subset to the
date after 2016-08-01. I saw small changes in structure around that
time, thought I have not check it with structural break tests.

    library(XLConnect)

    ## Loading required package: XLConnectJars

    ## XLConnect 0.2-12 by Mirai Solutions GmbH [aut],
    ##   Martin Studer [cre],
    ##   The Apache Software Foundation [ctb, cph] (Apache POI, Apache Commons
    ##     Codec),
    ##   Stephen Colebourne [ctb, cph] (Joda-Time Java library),
    ##   Graph Builder [ctb, cph] (Curvesapi Java library)

    ## http://www.mirai-solutions.com ,
    ## http://miraisolutions.wordpress.com

    library(xts)

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    library(leaps)
    library(MuMIn)
    library(openxlsx)

    ## 
    ## Attaching package: 'openxlsx'

    ## The following objects are masked from 'package:XLConnect':
    ## 
    ##     loadWorkbook, mergeCells, saveWorkbook

    path_LLD_PS<- "C:/Users/msdeb/OneDrive/data/petrochemicals/IME_LLDPE_clean.xlsx"

    data.IME<- readWorksheetFromFile(path_LLD_PS, 
                                     sheet=1)
    data.IME<-data.IME[, c(2:9,12)]
    data.IME$DATE<- as.Date(data.IME$DATE,  "%Y/%m/%d")
    data.IME<- as.xts( data.IME[,-9], order.by = data.IME$DATE)
    data.IME<- data.IME[ -which(data.IME$supply == 0),]
    data.IME<- data.IME[ -which(data.IME$HIGH == data.IME$BASEp),]
    #data.IME<- data.IME[ -which(is.na(data.IME$HIGH)),]
    # 2017/08/29
    subIME<- data.IME["2016-08-01::"]


    path<- "C:/Users/msdeb/OneDrive/data/petrochemicals"
    #path<- "C:/Users/msdeb/OneDrive/data/petrochemicals"
    setwd(path)
    data.IME0075<-read.xlsx("IME_LDPE_clean.xlsx", sheet = 3, colNames = TRUE)
    data.IME0075<- data.IME0075[, c(2:9,12)]
    data.IME0075[,8]<- as.numeric(data.IME0075[,8])
    data.IME0075$DATE<- as.Date(data.IME0075$DATE,  "%Y/%m/%d")
    data.IME0075<- as.xts( data.IME0075[,-9], order.by = data.IME0075$DATE)
    if ( sum(data.IME0075$supply == 0, na.rm = TRUE)>0){
      data.IME0075<- data.IME0075[ -which(data.IME0075$supply == 0),]  
    }
    if ( sum(data.IME0075$demand/data.IME0075$supply <= 1, na.rm = TRUE)>0){
      data.IME0075<- data.IME0075[ -which(data.IME0075$demand/data.IME0075$supply <= 1),]  
    }
    if ( sum(data.IME0075$HIGH == data.IME0075$BASEp, na.rm = TRUE)>0){
      data.IME0075<- data.IME0075[ -which(data.IME0075$HIGH == data.IME0075$BASEp),]
    }

    subIME0075<- data.IME0075["2016-08-01::"]

    print("LLD209")

    ## [1] "LLD209"

    tail(subIME[,1:6])

    ##              LOW adjCLOSE  HIGH supply BASEp demand
    ## 2018-02-06 51529    51729 52909   4994 50214   7964
    ## 2018-02-27 51893    52029 52666   1496 50488   2684
    ## 2018-03-06 51353    51419 52159   4400 51353   4598
    ## 2018-03-07 52290    52453 53699   3300 50214   5126
    ## 2018-04-17 50328    50328 50328   4400 45753  14674
    ## 2018-04-24 50262    50262 50262   3300 45693  30074

    summary( subIME[,1:6])

    ##      Index                 LOW           adjCLOSE          HIGH      
    ##  Min.   :2016-08-23   Min.   :38171   Min.   :38269   Min.   :38765  
    ##  1st Qu.:2016-12-04   1st Qu.:40167   1st Qu.:40376   1st Qu.:41383  
    ##  Median :2017-05-29   Median :43009   Median :43232   Median :44010  
    ##  Mean   :2017-06-04   Mean   :43664   Mean   :43887   Mean   :44762  
    ##  3rd Qu.:2017-11-22   3rd Qu.:45658   3rd Qu.:45923   3rd Qu.:47127  
    ##  Max.   :2018-04-24   Max.   :52290   Max.   :52453   Max.   :53699  
    ##      supply         BASEp           demand     
    ##  Min.   : 250   Min.   :37766   Min.   : 1390  
    ##  1st Qu.:2200   1st Qu.:39041   1st Qu.: 4534  
    ##  Median :3300   Median :42081   Median : 5786  
    ##  Mean   :3158   Mean   :42147   Mean   : 6361  
    ##  3rd Qu.:4400   3rd Qu.:43531   3rd Qu.: 7359  
    ##  Max.   :5500   Max.   :51353   Max.   :30074

    incr.bid209<- subIME$HIGH / subIME$BASEp
    colnames(incr.bid209)<- "incr.bid.ratio"
    summary(incr.bid209[ " ::2018-04-01"])

    ##      Index            incr.bid.ratio 
    ##  Min.   :2016-08-23   Min.   :1.005  
    ##  1st Qu.:2016-11-30   1st Qu.:1.042  
    ##  Median :2017-04-28   Median :1.067  
    ##  Mean   :2017-05-24   Mean   :1.061  
    ##  3rd Qu.:2017-11-12   3rd Qu.:1.083  
    ##  Max.   :2018-03-07   Max.   :1.100

    mean.incr.bid209<- mean(incr.bid209[ " ::2018-04-01"])
    print("mean increase bid till holidays-209")

    ## [1] "mean increase bid till holidays-209"

    mean.incr.bid209

    ## [1] 1.060584

    print("LD0075")

    ## [1] "LD0075"

    tail(subIME0075[,1:6])

    ##              LOW adjCLOSE  HIGH supply BASEp demand
    ## 2018-02-13 53800    53919 54189   1000 52865   1540
    ## 2018-03-06 52535    52603 52739   1500 52534   1750
    ## 2018-03-13 54369    54810 57248   1000 52044   3460
    ## 2018-03-27 55031    55228 55899   1000 54134   1590
    ## 2018-04-04 55388    55789 57279   1000 54134   1900
    ## 2018-04-17 51735    51735 51735   1500 47032   7330

    summary( subIME0075[,1:6])

    ##      Index                 LOW           adjCLOSE          HIGH      
    ##  Min.   :2016-08-16   Min.   :40275   Min.   :40909   Min.   :41419  
    ##  1st Qu.:2017-01-13   1st Qu.:43455   1st Qu.:43807   1st Qu.:44494  
    ##  Median :2017-07-04   Median :44909   Median :45042   Median :45377  
    ##  Mean   :2017-06-16   Mean   :46029   Mean   :46226   Mean   :46804  
    ##  3rd Qu.:2017-10-27   3rd Qu.:47823   3rd Qu.:47980   3rd Qu.:48849  
    ##  Max.   :2018-04-17   Max.   :55388   Max.   :55789   Max.   :57279  
    ##      supply         BASEp           demand     
    ##  Min.   : 500   Min.   :38308   Min.   : 1090  
    ##  1st Qu.:1000   1st Qu.:41194   1st Qu.: 1840  
    ##  Median :1000   Median :43971   Median : 2510  
    ##  Mean   :1149   Mean   :44497   Mean   : 2958  
    ##  3rd Qu.:1500   3rd Qu.:47158   3rd Qu.: 3225  
    ##  Max.   :2000   Max.   :54134   Max.   :11210

    incr.bid0075<- subIME0075$HIGH / subIME0075$BASEp
    colnames(incr.bid0075)<- "incr.bid.ratio"
    summary(incr.bid0075[ " ::2018-04-01"])

    ##      Index            incr.bid.ratio 
    ##  Min.   :2016-08-16   Min.   :1.004  
    ##  1st Qu.:2016-12-27   1st Qu.:1.025  
    ##  Median :2017-06-25   Median :1.050  
    ##  Mean   :2017-06-07   Mean   :1.053  
    ##  3rd Qu.:2017-10-17   3rd Qu.:1.080  
    ##  Max.   :2018-03-27   Max.   :1.100

    mean.incr.bid0075<- mean(incr.bid0075[ " ::2018-04-01"])
    print("mean increase bid till holidays-0075")

    ## [1] "mean increase bid till holidays-0075"

    mean.incr.bid0075

    ## [1] 1.052724

Price of the commodity in market is loosely equal to average bid price
times 9.262 percent VAT and omissions, plus the amount they pay to the
people who has the right to buy at exchange( bidding is limited to the
ones who has a production facility, they get a 500IRR per kilo to let
others use their right- this is not an absolutely legal act.), plus the
amount they pay for transportation from petrochemical facilities to
Tehran which is 950 IRR in this case and finally a mark-up they add per
kilo which is loosely 1000IRR. So the prices for LLD209 and LD0075 could
be computed as follows:

    # LLD209
    mrkup = 1000
    # Base price for bidding
    baseP = subIME$BASEp[ dim(subIME)[1]]
    colnames(baseP)<- "baseP"
    baseP

    ##            baseP
    ## 2018-04-24 45693

    # Tax and commisions
    tax.com.m = 1.09262
    # commision to owner of the right to buy commodity
    tax.com.c = 500
    # Transportation costs
    trnsp = 950
    # Base price of previous auction
    baseP.prv.auc = subIME$BASEp[ dim(subIME)[1]-3]
    colnames(baseP.prv.auc)<- "baseP.prv.auc"
    baseP.prv.auc

    ##            baseP.prv.auc
    ## 2018-03-06         51353

    # market price based on bidding at base price
    Pbc = baseP*tax.com.m + tax.com.c + trnsp + mrkup
    colnames(Pbc)<- "Pbc"
    Pbc

    ##                 Pbc
    ## 2018-04-24 52375.09

    # Market price on which the inventory been acquired
    P.prv.auc = baseP.prv.auc*tax.com.m + tax.com.c + trnsp + mrkup
    colnames(P.prv.auc)<- "P.prv.auc"
    P.prv.auc

    ##            P.prv.auc
    ## 2018-03-06  58559.31

    # market price based on average increase in bids
    Pc = (baseP*mean.incr.bid209)*tax.com.m + tax.com.c + trnsp + mrkup
    colnames(Pc)<- "Pc"
    Pc

    ##                  Pc
    ## 2018-04-24 55399.77

    # Market price based on maximum bid available
    Pcor = (baseP*1.1)*tax.com.m + tax.com.c + trnsp + mrkup
    colnames(Pcor)<- "Pcor"
    Pcor

    ##                Pcor
    ## 2018-04-24 57367.59

    # LD0075

    baseP0075 = subIME0075$BASEp[ dim(subIME0075)[1]]
    colnames(baseP0075)<- "baseP0075"
    baseP0075 

    ##            baseP0075
    ## 2018-04-17     47032

    # Base price of previous auction
    baseP.prv.auc0075  = subIME0075$BASEp[ dim(subIME0075)[1]-3]
    colnames(baseP.prv.auc0075)<- "baseP.prv.auc0075"
    baseP.prv.auc0075 

    ##            baseP.prv.auc0075
    ## 2018-03-13             52044

    # market price based on bidding at base price
    Pbc0075  = baseP0075 *tax.com.m + tax.com.c + trnsp + mrkup
    colnames(Pbc0075)<- "Pbc0075"
    Pbc0075 

    ##            Pbc0075
    ## 2018-04-17 53838.1

    # Market price on which the inventory been acquired
    P.prv.auc0075  = baseP.prv.auc0075 *tax.com.m + tax.com.c + trnsp + mrkup
    colnames(P.prv.auc0075)<- "P.prv.auc0075"
    P.prv.auc0075 

    ##            P.prv.auc0075
    ## 2018-03-13      59314.32

    # market price based on average increase in bids
    Pc0075  = (baseP0075 *mean.incr.bid0075 )*tax.com.m + tax.com.c + trnsp + mrkup
    colnames(Pc0075)<- "Pc0075"
    Pc0075 

    ##              Pc0075
    ## 2018-04-17 56547.48

    # Market price based on maximum bid available
    Pcor0075  = (baseP0075 *1.1)*tax.com.m + tax.com.c + trnsp + mrkup
    colnames(Pcor0075)<- "Pcor0075"
    Pcor0075 

    ##            Pcor0075
    ## 2018-04-17 58976.91

Simplified payoff that I consider here is consisted of two part. Here I
assumed that players are identical and the amount they buy is minimum
one truckload, that is 20 tons. We also assume that there exist a total
speculative inventory "Inv" in the market and that is equally
distributed among "k" trader. First part is the amount the player pays
for reacquiring commodities, and second part is the amount of change in
value of its inventory:

minimum position = 20000 Kg payoff in case of bidding at base price:

−(*P**b**c* − *m**r**k**u**p*)\*20000 + *P**b**c* \* *I**n**v*/*k* − *P*.*p**r**v*.*a**u**c* \* *I**n**v*/*k*
*s*.*t**k* \* 20 &lt; =*s**u**p**p**l**y*

payoff in case of an increase in bid equal to average increase:
−(*P**c* − *m**r**k**u**p*)\*20000 + *P**c* \* *I**n**v*/*k* − *P*.*p**r**v*.*a**u**c* \* *I**n**v*/*k*
*s*.*t**k* \* 20 = *s**u**p**p**l**y*

payoff in case of bidding at maximum available price:
−(*P**c**o**r* − *m**r**k**u**p*)\*(*s**u**p**p**l**y*/*k* \* 20000)\*20000 + +*P**c**o**r* \* *I**n**v*/*k* − *P*.*p**r**v*.*a**u**c* \* *I**n**v*/*k*

The last payoff, takes the fact that in case of equal bids, the supply
would be divided equally to bidders into account. As we will see this
ratio has a enormous effect on the cost of cornering market.

For having a strictly dominant strategy, the second payoff should be
strictly larger than first one. Also the third term should be strictly
larger than second term.

*I**n**v*/*k* &gt; ((*P**c* − *P**b**c*)\*20000)/(*P**c* − *P**b**c*)

*I**n**v*/*k* &gt; ( − (*P**c* − *m**r**k**u**p*)\*20000 + (*P**c**o**r* − *m**r**k**u**p*)\*(*s**u**p**p**l**y*/20000 \* *k*)\*20000)/(*P**c**o**r* − *P**c*)

The first inequality reduces to Inv/k being bigger than minimum position
which is 20 Tons.

If we neglect the mark-up term, the second inequality would be:

*I**n**v*/*k* &gt; 20000 \* (*P**c**o**r* \* (*s**u**p**p**l**y*/*k* \* 20000)) − *P**c*)/(*P**c**o**r* − *P**c*)

It means if the supply over aggregate demand being low enough, then the
inequality is holding even for very small per player inventory. In other
words, in this design and by these assumption, market will be cornered.

Let see what is the amount of minimum per player inventory given the
data:

    # Minimum per trader position for LLD209 in order that 2nd inequality holds
    supply<- 3300 # Ton
    k = 30074 / 20 # Ton
     min.per.Inv<- ((-(Pc - mrkup) * 20000) + (Pcor - mrkup) *
                      ((supply / (20000* k))*20000))/(Pcor- Pc)
     colnames(min.per.Inv)<- "min.per.Inv"
    min.per.Inv

    ##            min.per.Inv
    ## 2018-04-24   -552829.5

    # Minimum per trader position for LD0075 in order that 2nd inequality holds
     supply<- 1500 # Ton
     k = 7330 / 20 # Ton
     min.per.Inv0075<- ((-(Pc0075 - mrkup) * 20000) + (Pcor0075 - mrkup) *
                          ((supply / (20000* k))*20000))/(Pcor0075- Pc0075)
      colnames(min.per.Inv0075)<- "min.per.Inv0075"
     min.per.Inv0075

    ##            min.per.Inv0075
    ## 2018-04-17       -457190.2

Oh, these are pretty fascinating results. I am so excited :))

Given the data, strictly dominant strategy would be choose.

Conclusion
----------

It seems that under special circumstances that total speculative
inventory is high enough, decision to cornering market is strictly
dominant strategy, regardless of what was the price in market before
holidays.

If inventories of speculative traders meet this criterion there is
incentive to bid at highest available price. So by the time that
inventories get consumed for producing products, cornering the market
could continue. A decrease in prices due to importing of commodities
from gulf countries could change the assumptions, yet due to new
regulations in Forex market, it do not seem to happen.

###### *Please inform me about your feedback, I will be deeply grateful for that :)*

###### For disclaimer please see about page.
