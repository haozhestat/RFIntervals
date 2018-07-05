# Random Forest Prediction Intervals

This repository contains R code and 60 datasets to reproduce the simulation studies and data analysis in the paper "Random Forest Prediction Intervals" by Haozhe Zhang, Joshua Zimmerman, Dan Nettleton, and Daniel J. Nordman.

A R package "RFIntervals" is under development. This package will produce out-of-bag prediction intervals, split conformal prediction intervals, and quantile regression forest intervals for random forest predictions, as well as random forest confidence intervals for estimated model mean functions.

Some key R files

1. 'RFOOBInterval.R'
2. 'RFQuanInterval.R'
3. 'sim_data.R'
4. 'sim_marg.R'
5. 'sim_cond.R'
6. 'sim_summary.R'
7. 'DataAnalysis_summary.R'
8. 'TunePara_Concrete.R'
9. 'uci_main.R'
10. 'nipsdata_main.R'

Related references

- "Distribution-Free Predictive Inference for Regression" by Jing Lei, Max G'Sell, Alessandro Rinaldo, Ryan Tibshirani, and  Larry Wasserman,Journal of the American Statistical Association, just-accepted (2017).

- "Quantile regression forests" by Nicolai, Meinshausen, Journal of Machine Learning Research 7, no. Jun (2006), 983-999.

- "Confidence intervals for random forests: The jackknife and the infinitesimal jackknife" by Stefan Wager, Trevor Hastie, and Bradley Efron, The Journal of Machine Learning Research, 15.1 (2014), 1625-1651.

- "Quantifying uncertainty in random forests via confidenceintervals and hypothesis tests" by Lucas Mentch and Giles Hooker,  The Journal of Machine Learning Research, 2016, 17(1), 841-881.
