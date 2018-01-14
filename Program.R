 #Utilize quantmod to load the security symbols
 require(quantmod)
 symbols <- c("AAPL", "QQQ")
 getSymbols(symbols)

 #define training set
 startT  <- "2014-01-01"
 endT    <- "2016-01-01"
 rangeT  <- paste(startT,"::",endT,sep ="")
 tAAPL   <- AAPL[,6][rangeT]
 tQQQ   <- QQQ[,6][rangeT]
 
 #define out of sample set
 startO  <- "2016-02-01"
 endO    <- "2016-12-01"
 rangeO  <- paste(startO,"::",endO,sep ="")
 oAAPL   <- AAPL[,6][rangeO] #gets column number 6 in the Apple stock info matrice i.e. the closing stock price adjusted for dividends
 oQQQ   <- QQQ[,6][rangeO]

 #compute price differences on in-sample data
 pdtAAPL <- diff(tAAPL)[-1] #the diff function gets the difference between each value in the matrix
 pdtQQQ <- diff(tQQQ)[-1]
 
 #build the model
 model  <- lm(pdtAAPL ~ pdtQQQ - 1) #the lm function performs an ordinary least squares fit (which reduces the Sum of Squared Residuals (i.e. the difference between the expected and actual values) 
 
 #obtains the hedge ratio
 hr <- as.numeric(model$coefficients[1])
 
  #spread price (in-sample). When this spread is passed we make the trades.
 spreadT <- tAAPL - hr * tQQQ
 
 #compute statistics of the spread
 meanT    <- as.numeric(mean(spreadT,na.rm=TRUE))
 sdT      <- as.numeric(sd(spreadT,na.rm=TRUE))
 upperThr <- meanT + 1 * sdT
 lowerThr <- meanT - 1 * sdT
 
 #visualize the in-sample spread + stats
 plot(spreadT, main = "AAPL vs. QQQ spread (in-sample period)")
 abline(h = meanT, col = "red", lwd =2)  #lwd = line width
 abline(h = meanT + 1 * sdT, col = "blue", lwd=2)
 abline(h = meanT - 1 * sdT, col = "blue", lwd=2)
 
 #Generates a histogram which shows the distribution of the spread
 hist(spreadT, col = "blue", breaks = 100, main = "Spread Histogram (AAPL vs. QQQ)")
 abline(v = meanT, col = "red", lwd = 2)
 
 #Determines if the spread has exceeded our uppper treshold (in which case we sell AAPL and buy QQQQ) or exceeded our lower threshold (in which case we buy AAPL and sell QQQQ)
 indSell <- which(spreadT >= meanT + sdT) 
 indBuy  <- which(spreadT <= meanT - sdT) 
  #Determine when to make trades
  spreadL  <- length(spreadT)
 pricesBuy  <- c(rep(NA,spreadL))
 pricesSell  <- c(rep(NA,spreadL))
 sp       <- as.numeric(spreadT)
 
 tradeQty <- 100
 totalP   <- 0
 
 for(i in 1:spreadL) {
     spTemp <- sp[i]
     if(spTemp < lowerThr) {
        if(totalP <= 0){
           totalP     <- totalP + tradeQty
           pricesBuy[i] <- spTemp
        }
     } else if(spTemp > upperThr) {
       if(totalP >= 0){
          totalP <- totalP - tradeQty
          pricesSell[i] <- spTemp
       }
    }
 }
 
 #Visualize the trades via a plot
  plot(spreadT, main = "AAPL vs. QQQ spread (in-sample period)")
 abline(h = meanT, col = "red", lwd =2)
 abline(h = meanT + 1 * sdT, col = "blue", lwd = 2)
 abline(h = meanT - 1 * sdT, col = "blue", lwd = 2)
 points(xts(pricesBuy,index(spreadT)), col="green", cex=1.9, pch=15)
 points(xts(pricesSell,index(spreadT)), col="red", cex=1.9, pch=15)

  #Generates a histogram which shows the distribution of the trades
 hist(spreadT, col = "blue", breaks = 100, main = "Spread Histogram (AAPL vs. QQQ)")
 abline(v = meanT, col = "red", lwd = 2)
 abline(v= xts(pricesBuy,index(spreadT)))

 hist(spreadT, col = "blue", breaks = 100, main = "Spread Histogram (AAPL vs. QQQ)")
 abline(v = meanT, col = "red", lwd = 2)
 abline(v= xts(pricesBuy,index(spreadT)), col="green")