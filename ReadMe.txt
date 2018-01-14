This program makes a simple pairs trade between 2 equities: Microsoft and the Nasdaq 100 Trust. 
This program uses the quantmod package to obtain historical data for the 2 equities and calculates the hedge ratio.
We generate a model using the lm() function, which gets the Ordinary Least Squares (OLS). OLS is a method of linear regression 
which minimizes RSS (Residual Sum of Squares, the Residual being the difference between estimated value and the observed value) 
and so attempts to closely "fit" the function with the data.
Using this model the program determines if and when there is a weakness in the correlation and at that point 
shorts the stock which is determined to be underperforming and goes long on the stock that is overperforming in the belief 
that more value will be gained from one option than will be lost in the other (although it is possible that both would profit).