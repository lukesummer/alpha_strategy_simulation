# Introduction of Alpha Strategy Simulation.
This code is written by R.  
This code simulates the trading process of Chinese alpha strategy funds in 2014.  
The simulation period is from 2010.10.05 to 2014.12.23, 200 weeks data and the later 60 weeks is the trading weeks.  

#basic parameters
*risk_free_rate*: the weekly risk free rate  
*margin_percent*: the proportion of margin and reserve  
*init_wealth*: the inital captial you have before entering the trading process  
*init_reserve*: the initial risk free asset  
*adj_period*: determine how often you renew you alpha and beta  
*z*: the quantile value for the distribution you choose. This is to calculate the VaR  
*reduce_percent*: Determine the remain proportion of the risky asset when the exception occurs  
