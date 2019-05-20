---
layout: post
subtitle: Predicting Index
tags: [LASSO, Riged, Elastic net, AR, rnn, Recurrent Neural Network, gru, lstm, rwgrwssion Shrinkage, PCA, Random Forest, R, nnetar]
comments: true
---


### Current problem

When I was assessing the CRP portfolio, I found that it's horse power is
variance. Constructed CRP returns suffered extensively from portfolios
Beta. Although it was hedged as I said in the related post, I wanted to
improve it by predicting VLGI. If downturns could be avoided, the amount
of needed hedge portfolio value could be reduced substantially.

*I computed the following in October last year, so the results are out of date.*

### Idea

My idea is this: generally predefined lags are used, yet since monetary variables takes time to show their effects lets
use correlation for finding the lags that should be used.I was thinking
that since rnn based methods could capture non-linear relations, using
"Kendall's tau coefficient" seems agreeable. Further, I saw no need for taking largest
tau, lets see what happens if the number of variables that we take get bigger.

### Methods

For this matter I would use several methods. Some of which would
consider the temporal characteristics of the data and some of them won't
do that. Having studied economics I am pessimistic about the later
group. Yet having practiced judo, I like trying different techniques.

For doing the comparisons the following is used:

-   AR(1) as benchmark
-   Forecast::nnetar function. It is described as Feed-forward neural
    networks with a single hidden layer and lagged inputs for
    forecasting univariate time series.
-   Regression Shrinkage
    -   LASSO
    -   Riged
    -   Elastic net
-   Principal Component regression including AR(1) term
-   Random Forest
-   Recurrent Neural Network
    -   rnn
    -   gru
    -   lstm

### Data

I would use 25 series of data containing inflation, money aggregates,
banking rates like default rate, and finally oil production.

### code

Iranian websites normally provide data in excel format. I need to
annualize monthly data for VLGI and growth rates need to be computed in quarterly, bi-annual,
3-quarter and annual frequencies. Since I am a little lazy I used lead
of the computed annualized variables instead of lagging all the other
variables :))

    path = "C:/Users/msdeb/OneDrive/data"
    setwd(path)
    library(XLConnect)
    MACRO.data<- readWorksheetFromFile("./macro/macro data.xlsx", 
                                       sheet=2)
    MACRO.data<- as.xts( MACRO.data[,-1], order.by = MACRO.data$DATE)
    dlndata<- apply(MACRO.data, 2 , function(x) diff.xts(log(x)))
    dlndata<- apply(dlndata,2,function(x) { x[ x %in% c( -Inf, Inf)] = 0 ; x })
    ind<-seq.Date(as.Date(index(MACRO.data)[1]), as.Date(index(MACRO.data)[dim(MACRO.data)[1]]), by= "month")

    dlndata<- as.xts(dlndata, order.by = ind)
    dlndata$CPI.YoY.Ext<- MACRO.data$CPI.YoY.Ext


    INDX.data<- read.csv(file = "./indexes/VLGI.tot.CSV")
    INDX.data<- as.xts(INDX.data$VLGI, order.by = as.Date(INDX.data$DATE))
    INDX.data<-  to.monthly(INDX.data)[,4]
    g.INDX<- lag.xts( 12* (diff.xts( log( INDX.data))), k = -1)
    g.INDX.3<- lag.xts((12/3)* (diff.xts( log( ( INDX.data)), lag = 3)), k = -3)
    g.INDX.6<- lag.xts((12/6)* (diff.xts( log( ( INDX.data)), lag = 6)), k = -6)
    g.INDX.9<- lag.xts((12/9)* (diff.xts( log( ( INDX.data)), lag = 9)), k = -9)
    g.INDX.12<- lag.xts((12/12)* (diff.xts( log( ( INDX.data)), lag = 12)), k = -12)

Next lags, VLGI, and VLGI lag is added to the data. I saw that "lag"
function inside some functions do not behave as "lag.xts", so it is
accounted for. Last function compute the validation and training set. It
also removes NA columns. (so the original data reaches to 20 series for
the beginning of the data )

    reg.lag<- function(ind = g.INDX, reg = dlndata$M2, max.lag = 20, num.take = 5) {
      x<- matrix ( NA, ncol = max.lag, nrow = nrow( reg))
      for( i in 1 : max.lag){
        x[, i]<- lag.xts( reg, k = i)
      }
      x<- as.xts( x, order.by = index( reg))
      x<- merge.xts(ind, x)
      x1<- x[ complete.cases(x[,1]),]
      x1<- x1[ complete.cases(x1[, dim( x1)[2]]),]
      lags.cor<- order(abs(cor(x1[,1], x1[,-1],
                    use =  "complete.obs", method = "kendall")),
            decreasing = TRUE)[ 1 : num.take]
      #lags.cor<- lags.cor[ lags.cor != 1]
      
      reg.include<- x[, c(lags.cor)]
      names<- paste(colnames(reg), "L", lags.cor, sep = "." )
      colnames(reg.include)<- names
      
      return(reg.include)
    }


    data<- dlndata
    for( i in 1 : dim(dlndata)[2]){
      temp<- reg.lag(ind = g.INDX, reg = dlndata[,i], max.lag = 20, num.take = 5)
        data<- merge.xts(data, temp)
    }
    data<- data[ , 25: dim(data)[2] ]
    data$LINDX<- scale(lag.xts(g.INDX)[index(data)])

    sc.reg<- scale(data)
    sc.reg<- merge.xts(g.INDX[index(sc.reg)], sc.reg)

    sc.reg.sub<- sc.reg["2001-10-01::2017-02-01"]

    vld.tr.fun<- function( data = sc.reg.sub){
      na.col<- NULL
      for( i in 1 : dim(data)[2]){
        x<- NULL
        if( sum(is.na(data[,i])) > 0) x <- i
        na.col<- c( na.col, x)
      }
      
      data<- data[,!c(1 : dim(data)[2]) %in% na.col]
      
      train.d<- data[ - dim(data)[1], ]
      valid.d<- data[ dim(data)[1], ]
      out<- list( train.d = train.d, valid.d = valid.d)
      return(out)
    }

    Next is AR(1) function:

     AR.fun<- function( train.d. = train.d, valid.d. = valid.d){
       fit <- tryCatch(Arima(train.d.[,1],
                             order = c(1,0,0) ,method="CSS-ML"))
       dep.var<- cbind( c( train.d.[ dim( train.d.)[1] ,1],   valid.d.[,1]))
       
       pred<- Arima( dep.var, model = fit)
       pred.pca<- fitted( pred)[ 2]
       return( pred.pca)
     }
     
     
     AR.win<- function( x = sc.reg.sub){
       tr.vl<- vld.tr.fun( data = x)
       train.d<- tr.vl$train.d
       valid.d<- tr.vl$valid.d
       AR.fun( train.d. = train.d, valid.d. = valid.d)
       
     }

the function for feed-forward neural networks with a single hidden layer
and lagged inputs for forecasting univariate time series from beautiful
forecast package:

     nnetar.fun<- function( train.d. = train.d, valid.d. = valid.d){
       ncol.nnetar<- if( dim( train.d.)[2] < 25) 2 : dim( train.d.)[2] else c( 2 : 25 ,
                                                                               dim( train.d.)[2])
       fit<- nnetar(y = as.numeric(train.d.[,1]), repeats=30,
                    xreg = as.data.frame(train.d.[ ,ncol.nnetar]))
       pred.nnetar<- forecast(fit, xreg = as.data.frame(valid.d.[, ncol.nnetar]), h = 14)
       pred.nnetar<- as.xts( pred.nnetar$mean[1], order.by = index(valid.d.))
       return( pred.nnetar)
     }
     
     nnetar.win<- function( x = sc.reg.sub){
       tr.vl<- vld.tr.fun( data = x)
       train.d<- tr.vl$train.d
       valid.d<- tr.vl$valid.d
       nnetar.fun( train.d. = train.d, valid.d. = valid.d)
       
     }

Regression shrinkage using LASSO, ridge, and elastic net. Since the data
is highly correlated I would imagine that LASSO would not be perfect,
yet as the number of lags that for each variable is taken could vary
there could be clusters of highly correlated data. lets see how it would
work.

     shrink.fun<- function( train.d. = train.d, valid.d. = valid.d){

       m.ridge<- tryCatch(cv.glmnet(x = as.matrix(train.d.[,-1]),
                                    y = as.matrix(train.d.[,1]),
                                    type.measure="mse", alpha = 0, 
                                    family="gaussian"),
                         error=function(e){NA} )
       if ( length( m.ridge) < 2) { ridge.pred <- NA} else{
       ridge.pred<- predict.cv.glmnet(m.ridge, newx = as.matrix( valid.d.[,-1]), s = "lambda.1se")
                            
       }
       
       m.lasso<- tryCatch(cv.glmnet(x = as.matrix(train.d.[,-1]),
                                    y = as.matrix(train.d.[,1]),
                                    type.measure="mse", alpha=1, 
                                    family="gaussian"),
                         error=function(e){NA} )
       if ( length( m.lasso) < 2) { lasso.pred <- NA} else{
       lasso.pred<- predict.cv.glmnet(m.lasso, newx = as.matrix( valid.d.[,-1]), s = "lambda.1se")
       }
       
       
       m.enet<-tryCatch(cv.glmnet(x = as.matrix(train.d.[,-1]),
                                  y = as.matrix(train.d.[,1]),
                                  type.measure="mse", alpha= .5, 
                                  family="gaussian"),
                       error=function(e){NA} )
       if ( length( m.enet) < 2) { enet.pred <- NA} else{
       enet.pred<- predict.cv.glmnet(m.enet, newx = as.matrix( valid.d.[,-1]), s = "lambda.1se")
       }
       pred <- cbind( ridge.pred = ridge.pred, lasso.pred = lasso.pred,
                      enet.pred = enet.pred)
       pred<- as.xts( pred, order.by = index(valid.d.))
       return(pred)
     }
     
     
     
     shrink.win<- function( x = sc.reg.sub){
       tr.vl<- vld.tr.fun( data = x)
       train.d<- tr.vl$train.d
       valid.d<- tr.vl$valid.d
       shrink.fun( train.d. = train.d, valid.d. = valid.d)
       
     }

PCA with AR(1) considered rotated variables that explain at least 85
percent of data. Again since we choose different number of lags for each
variable, the number that explains 85 percent could vary substantially.

     pca.fun<- function( train.d. = train.d, valid.d. = valid.d){
       col.pca<- 2 : ( dim( train.d.)[ 2] - 1 )
       
       pca <- prcomp( train.d.[, col.pca], scale. = F, center = F)
       
       ncol.pca<- which( summary( pca)$ importance[ 3,] > .85)[ 1]
       
       pca.valid<- predict( pca, valid.d.[, col.pca])[ 1 : ncol.pca]
       pca.valid<- rbind( pca$x[ dim(pca$x)[1] ,1 : ncol.pca], pca.valid)
       
       fit <- tryCatch(Arima(train.d.[,1], xreg = pca$x[ ,1 : ncol.pca],
                             order = c(1,0,0) ,method="CSS-ML"))
       dep.var<- rbind( c(train.d.[ dim(train.d.)[1] ,1],   valid.d.[,1]))
       
       pred<- Arima(dep.var, xreg = pca.valid,
                    model=fit)
       pred.pca<- fitted(pred)[2]
       return(pred.pca)
     }
     
     
     PCA.win<- function( x = sc.reg.sub){
       tr.vl<- vld.tr.fun( data = x)
       train.d<- tr.vl$train.d
       valid.d<- tr.vl$valid.d
       pca.fun( train.d. = train.d, valid.d. = valid.d)
       
     }

For Random Forest first "mtray" is tuned then it was computed.

     rf.fun<- function( train.d. = train.d, valid.d. = valid.d){
       
       mtry.pot<- tuneRF(x= as.data.frame(train.d.[,-1]), y = as.numeric(train.d.[,1]),
                         ntreeTry=500, stepFactor=2, improve=0.0005,
                         trace=FALSE, plot=FALSE, doBest=FALSE
       )
       mtry.pot<- mtry.pot[ which.min(mtry.pot[, 2]), 1]
       
       mtry.pot<- tuneRF(x= as.data.frame(train.d.[,-1]), y = as.numeric(train.d.[,1]),
                         ntreeTry=500, stepFactor=1.41, improve=0.0005,
                         trace=FALSE, plot=FALSE, doBest=FALSE,
                         mtryStart = mtry.pot)
       
       mtry.pot<- mtry.pot[ which.min(mtry.pot[, 2]), 1]
       
       mtry.pot<- tuneRF(x= as.data.frame(train.d.[,-1]), y = as.numeric(train.d.[,1]),
                         ntreeTry=500, stepFactor=1.19, improve=0.0005,
                         trace=FALSE, plot=FALSE, doBest=FALSE,
                         mtryStart = mtry.pot)
       
       mtry.pot<- mtry.pot[ which.min(mtry.pot[, 2]), 1]
       
       mtry.pot<- tuneRF(x= as.data.frame(train.d.[,-1]), y = as.numeric(train.d.[,1]),
                         ntreeTry=500, stepFactor=1.09, improve=0.0005,
                         trace=FALSE, plot=FALSE, doBest=FALSE,
                         mtryStart = mtry.pot)
       
       mtry.pot<- mtry.pot[ which.min(mtry.pot[, 2]), 1]
       
       
       rf <- randomForest( x = as.data.frame(train.d.[,-1]), y = as.numeric(train.d.[,1]),
                           data = train.d., ntree=5000 ,
                           importance =FALSE, replace = TRUE,
                           mtry = mtry.pot
       )
       pred.rf <- predict(rf, valid.d.)
       return(pred.rf)
     }
     
     rf.win<- function( x = sc.reg.sub){
       tr.vl<- vld.tr.fun( data = x)
       train.d<- tr.vl$train.d
       valid.d<- tr.vl$valid.d
       rf.fun( train.d. = train.d, valid.d. = valid.d)
       
     }

For recurrent neural networks, rnn, lstm and gru methods is used. I have
not been pretty familiar with these methods, and I loved to work with
them because of that :) I used brute force optimization for optimizing
on training set.

The time needed for estimating suggests using parallel
computations.

     rnn.fun<- function(data, net.type = "gru",
                        l.rate = 0.1, l.decay = 1,
                        hid = 2, epch = 10){
       nrow<- dim(data)[1]
       vl.date<- as.Date(index(data)[nrow])
       tr.data<- array( c(data[(1:(nrow - 1)),-1]),
                        dim=c(1, (nrow - 1) ,(dim(data)[2]-1)))
       vl.data<- array( c(data[nrow ,-1]),
                        dim=c(1, 1 ,(dim(data)[2]-1)))
       model <- trainr(Y=t(data[-nrow,1]),
                       X=tr.data,
                       network_type = net.type,
                       learningrate   =  l.rate,
                       learningrate_decay = l.decay,
                       hidden_dim     = hid,
                       batch_size = 1,
                       numepochs = epch
       )
       
       prd.model<- predictr(model, vl.data)
       out<- cbind(date = as.numeric(vl.date), prd = prd.model)
       return(out)
     }
     
     rnn.cv.fun<- function( train.d. = train.d, valid.d. = valid.d, 
                            rnn.fun. = rnn.fun, mdl = "gru",
                            win = ((nrow(train.d.)* (2/3)) + 1)){
       library(plyr)
       library(dplyr)
       library(xts)
       library(data.table)
       library(forecast)
       
       cl = createCluster(11, export = list("rnn.fun"),
                          lib = list( "rnn", "xts", "plyr", "foreach"))
       
       RMSE.in<- NULL
       RMSE.in<- foreach(i=seq(0.001, 0.9, by = 0.05),.combine=rbind) %dopar%{
         RMSE.in<- NULL
         for ( j in 1: 6){
           out1 = rollapply(train.d., width = win, function(x) rnn.fun(x, net.type = mdl,
                                                                       l.rate = i, l.decay = 1,
                                                                       hid = j, epch = 5), 
                            by.column = FALSE , na.pad = FALSE, partial = FALSE
           )
           out1 = out1[,2]
           temp<- sqrt(mean((train.d.[, 1] - (out1))^2))
           temp<- cbind( l.rate = i, hid = j, RMSE = temp)
           RMSE.in<- rbind(RMSE.in, temp)
         }
         RMSE.in
       }
       stopCluster(cl)
       pot.lh<- RMSE.in[ which.min(RMSE.in[,3]),]
       
       
       tr.data<- array( c( train.d.[,-1]),
                        dim = c( 1, nrow( train.d.) ,( dim( train.d.)[2]-1)))
       
       model <- trainr( Y = t( train.d.[ ,1]),
                        X = tr.data,
                        network_type = mdl,
                        learningrate   =  pot.lh[1],
                        learningrate_decay = 1,
                        hidden_dim     = pot.lh[2],
                        batch_size = 1,
                        numepochs = 5
       )
       
       vl.data<- array( c(valid.d.[ ,-1]),
                        dim=c(1, 1 ,(dim(valid.d.)[2]-1)))
       
       prd.model<- predictr(model, vl.data)
       prd.rnn<- as.xts( prd.model, order.by = index( valid.d.))
       return(prd.rnn)
     }
     
     rnn.win<- function( x = sc.reg.sub, model. = "rnn"){
       tr.vl<- vld.tr.fun( data = x)
       train.d<- tr.vl$train.d
       valid.d<- tr.vl$valid.d
       rnn.cv.fun( train.d. = train.d, valid.d. = valid.d, mdl = model.)
       
     }

Lets compute them together:

     out.prd.AR<- rollapply(sc.reg.sub, width = 72,
               FUN = function(x) AR.win(x), by.column = FALSE)
     
     out.prd.nnetar<-rollapply(sc.reg.sub, width = 72,
               FUN = function(x) nnetar.win(x), by.column = FALSE)
     
     out.prd.shrink<- rollapply(sc.reg.sub, width = 72,
               FUN = function(x) shrink.win(x), by.column = FALSE)
     
     out.prd.PCA<- rollapply(sc.reg.sub, width = 72,
               FUN = function(x) PCA.win(x), by.column = FALSE)
     
     out.prd.rf<- rollapply(sc.reg.sub, width = 72,
               FUN = function(x) rf.win(x), by.column = FALSE)
     
     out.prd.rnn<- rollapply(sc.reg.sub, width = 72,
                             FUN = function(x) rnn.win(x, model. = "rnn"), by.column = FALSE)
     
     out.prd.gru<- rollapply(sc.reg.sub, width = 72,
               FUN = function(x) rnn.win(x, model. = "gru"), by.column = FALSE)
     
     out.prd.lstm<- rollapply(sc.reg.sub, width = 72,
                             FUN = function(x) rnn.win(x, model. = "lstm"), by.column = FALSE)

### Results

#### DA DA!!

DA DA! Results  could be pretty good for 1-step ahead forecast
(the results are not shown here) but in reality most of the data that
are used are NA based on their timing of release. I need to put at least
five months before hand for the data to be published. (yes, I know, Iran
central bank publish everything like the most other Iranian public institutions, they
surely take their time for publishing data.)

So lets see 6-step-ahead forecast. Row names stands for the number of
lags that been chosen. The first line "1.1" shows the result by simply
considering one lag.

<table>
<caption>Table continues below</caption>
<colgroup>
<col width="13%" />
<col width="18%" />
<col width="17%" />
<col width="17%" />
<col width="16%" />
<col width="16%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">RMSE.AR</th>
<th align="center">RMSE.nnetar</th>
<th align="center">RMSE.ridge</th>
<th align="center">RMSE.lasso</th>
<th align="center">RMSE.enet</th>
<th align="center">RMSE.PCA</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.179</td>
<td align="center">0.2145</td>
<td align="center">0.3989</td>
<td align="center">0.4059</td>
<td align="center">0.4162</td>
<td align="center">0.1913</td>
</tr>
<tr class="even">
<td align="center">0.179</td>
<td align="center">0.2171</td>
<td align="center">0.3971</td>
<td align="center">0.4017</td>
<td align="center">0.4147</td>
<td align="center">0.1924</td>
</tr>
<tr class="odd">
<td align="center">0.179</td>
<td align="center">0.2115</td>
<td align="center">0.3916</td>
<td align="center">0.3954</td>
<td align="center">0.3936</td>
<td align="center">0.1937</td>
</tr>
<tr class="even">
<td align="center">0.179</td>
<td align="center">0.2174</td>
<td align="center">0.3904</td>
<td align="center">0.4096</td>
<td align="center">0.3966</td>
<td align="center">0.2104</td>
</tr>
<tr class="odd">
<td align="center">0.179</td>
<td align="center">0.2177</td>
<td align="center">0.3825</td>
<td align="center">0.4751</td>
<td align="center">0.4618</td>
<td align="center">0.2252</td>
</tr>
</tbody>
</table>

<table style="width:60%;">
<colgroup>
<col width="13%" />
<col width="15%" />
<col width="15%" />
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">RMSE.rf</th>
<th align="center">RMSE.rnn</th>
<th align="center">RMSE.gru</th>
<th align="center">RMSE.lstm</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.3362</td>
<td align="center">0.3994</td>
<td align="center">0.5153</td>
<td align="center">0.4413</td>
</tr>
<tr class="even">
<td align="center">0.3381</td>
<td align="center">0.4015</td>
<td align="center">0.5042</td>
<td align="center">0.4488</td>
</tr>
<tr class="odd">
<td align="center">0.332</td>
<td align="center">0.3887</td>
<td align="center">0.4386</td>
<td align="center">0.4475</td>
</tr>
<tr class="even">
<td align="center">0.3312</td>
<td align="center">0.4134</td>
<td align="center">0.494</td>
<td align="center">0.4442</td>
</tr>
<tr class="odd">
<td align="center">0.3347</td>
<td align="center">0.3677</td>
<td align="center">0.4633</td>
<td align="center">0.4438</td>
</tr>
</tbody>
</table>

The results based on using lags of predicted variable ( annualized six month
growth) or not have different RMSE.

Since using lag variables of annualized six month growth produce NAs at
the end of data, they just provide 1-step ahead forecast at the tail
of forecast and the rest of forecasts are not computable due to NAs. So
the results are as follows:

-   Methods that uses lags of predicted variable, including AR, nntar,
    and PCA( as the way we used it), are not better than AR(1)

-   Methods that uses lags of predicted variable, including ridge,
    Lasso, enet, RF, rnn, gru, and lstm, are not performing better
    than AR(1). and random forest seems to be better than other methods.

In practice what is important to me is ability to
predicting negative returns, lets see how are the MAE for 10% and 20%
quantiles.

    ## [1] "MAE for value less than 20% quantile"

<table>
<caption>Table continues below</caption>
<colgroup>
<col width="11%" />
<col width="16%" />
<col width="15%" />
<col width="15%" />
<col width="14%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">MAE.AR</th>
<th align="center">MAE.nnetar</th>
<th align="center">MAE.ridge</th>
<th align="center">MAE.lasso</th>
<th align="center">MAE.enet</th>
<th align="center">MAE.PCA</th>
<th align="center">MAE.rf</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.153</td>
<td align="center">0.1936</td>
<td align="center">0.5595</td>
<td align="center">0.5554</td>
<td align="center">0.5539</td>
<td align="center">0.1734</td>
<td align="center">0.4316</td>
</tr>
<tr class="even">
<td align="center">0.153</td>
<td align="center">0.1857</td>
<td align="center">0.5558</td>
<td align="center">0.5589</td>
<td align="center">0.5582</td>
<td align="center">0.1736</td>
<td align="center">0.4589</td>
</tr>
<tr class="odd">
<td align="center">0.153</td>
<td align="center">0.1823</td>
<td align="center">0.5547</td>
<td align="center">0.5595</td>
<td align="center">0.5597</td>
<td align="center">0.1664</td>
<td align="center">0.4989</td>
</tr>
<tr class="even">
<td align="center">0.153</td>
<td align="center">0.188</td>
<td align="center">0.5554</td>
<td align="center">0.5596</td>
<td align="center">0.561</td>
<td align="center">0.2005</td>
<td align="center">0.5048</td>
</tr>
<tr class="odd">
<td align="center">0.153</td>
<td align="center">0.1361</td>
<td align="center">0.5594</td>
<td align="center">0.5612</td>
<td align="center">0.562</td>
<td align="center">0.1993</td>
<td align="center">0.5125</td>
</tr>
</tbody>
</table>

<table style="width:42%;">
<colgroup>
<col width="13%" />
<col width="13%" />
<col width="13%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">MAE.rnn</th>
<th align="center">MAE.gru</th>
<th align="center">MAE.lstm</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.4645</td>
<td align="center">0.7919</td>
<td align="center">0.7307</td>
</tr>
<tr class="even">
<td align="center">0.4732</td>
<td align="center">0.7567</td>
<td align="center">0.7039</td>
</tr>
<tr class="odd">
<td align="center">0.5028</td>
<td align="center">0.6811</td>
<td align="center">0.7442</td>
</tr>
<tr class="even">
<td align="center">0.4959</td>
<td align="center">0.7764</td>
<td align="center">0.7342</td>
</tr>
<tr class="odd">
<td align="center">0.4589</td>
<td align="center">0.7468</td>
<td align="center">0.7136</td>
</tr>
</tbody>
</table>

    ## [1] "MAE for value less than 10% quantile"

<table>
<caption>Table continues below</caption>
<colgroup>
<col width="11%" />
<col width="16%" />
<col width="15%" />
<col width="15%" />
<col width="14%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">MAE.AR</th>
<th align="center">MAE.nnetar</th>
<th align="center">MAE.ridge</th>
<th align="center">MAE.lasso</th>
<th align="center">MAE.enet</th>
<th align="center">MAE.PCA</th>
<th align="center">MAE.rf</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.2324</td>
<td align="center">0.1766</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.2566</td>
<td align="center">0.5944</td>
</tr>
<tr class="even">
<td align="center">0.2324</td>
<td align="center">0.1558</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.2551</td>
<td align="center">0.6219</td>
</tr>
<tr class="odd">
<td align="center">0.2324</td>
<td align="center">0.1778</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.2394</td>
<td align="center">0.6998</td>
</tr>
<tr class="even">
<td align="center">0.2324</td>
<td align="center">0.1789</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.2918</td>
<td align="center">0.7034</td>
</tr>
<tr class="odd">
<td align="center">0.2324</td>
<td align="center">0.2042</td>
<td align="center">0.7092</td>
<td align="center">0.7056</td>
<td align="center">0.7056</td>
<td align="center">0.2996</td>
<td align="center">0.7227</td>
</tr>
</tbody>
</table>

<table style="width:42%;">
<colgroup>
<col width="13%" />
<col width="13%" />
<col width="13%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">MAE.rnn</th>
<th align="center">MAE.gru</th>
<th align="center">MAE.lstm</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">0.6106</td>
<td align="center">0.9475</td>
<td align="center">0.8364</td>
</tr>
<tr class="even">
<td align="center">0.6604</td>
<td align="center">0.9486</td>
<td align="center">0.7948</td>
</tr>
<tr class="odd">
<td align="center">0.5915</td>
<td align="center">0.7594</td>
<td align="center">0.8987</td>
</tr>
<tr class="even">
<td align="center">0.6828</td>
<td align="center">0.8388</td>
<td align="center">0.8575</td>
</tr>
<tr class="odd">
<td align="center">0.5293</td>
<td align="center">0.8486</td>
<td align="center">0.8887</td>
</tr>
</tbody>
</table>

The results are the same as above. Except that in some parts rnn work
better than random forest.

Conclusion
----------

Although methods that use lagged predicted variable show better
results, the forecast is not available due to construction of annualized
six month growth. Also considering RMSE for the rest of methods, they
are useless in action.

So the methods failed to assimilate Cassandra :))

###### *Please inform me about your feedback, I will be deeply grateful for that :)*

###### For disclaimer please see about page.
