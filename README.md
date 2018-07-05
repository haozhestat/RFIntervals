# Random Forest Prediction Intervals

This repository contains R code and 60 datasets to reproduce the simulation studies and data analysis in the paper "Random Forest Prediction Intervals" by Haozhe Zhang, Joshua Zimmerman, Dan Nettleton, and Daniel J. Nordman.

An R package "RFIntervals" is under development. This package will produce out-of-bag prediction intervals, split conformal prediction intervals, and quantile regression forest intervals for random forest predictions, as well as random forest confidence intervals for estimated model mean functions.

Some key R files:

1. 'RFOOBInterval.R' is a function that generates out-of-bag prediction intervals for test cases given a training sample;
2. 'RFQuanInterval.R' is a function that generates quantile regression forest intervals for test cases given a training sample;
3. 'sim_data.R' simulates data in the simulation study under a factorial design;
4. 'sim_marg.R' computes Type I and II coverage rates of the three prediction interval methods in the simulation study;
5. 'sim_cond.R' computes Type III and IV coverage rates of the three prediction interval methods in the simulation study;
6. 'sim_summary.R' draws boxplots (by gglot) of the simulation results;
7. 'DataAnalysis_summary.R' draws boxplots (by gglot) of the real data analysis;
8. 'TunePara_Concrete.R' explore the effect of RF tuning parameters on the performance of the threeprediction interval methods for the Concrete Compressive Strength dataset;
9. 'uci_main.R' is used to analyze the 20 datasets from UCI ML Repository;
10. 'nipsdata_main.R' is used to analyze 40 datasets from Chipman et al. (2010).


Related references

- "Distribution-Free Predictive Inference for Regression" by Jing Lei, Max G'Sell, Alessandro Rinaldo, Ryan Tibshirani, and  Larry Wasserman,Journal of the American Statistical Association, just-accepted (2017).

- "Quantile regression forests" by Nicolai, Meinshausen, Journal of Machine Learning Research 7, no. Jun (2006), 983-999.

- "Confidence intervals for random forests: The jackknife and the infinitesimal jackknife" by Stefan Wager, Trevor Hastie, and Bradley Efron, The Journal of Machine Learning Research, 15.1 (2014), 1625-1651.

- "Quantifying uncertainty in random forests via confidenceintervals and hypothesis tests" by Lucas Mentch and Giles Hooker,  The Journal of Machine Learning Research, 2016, 17(1), 841-881.

- "Bart: Bayesian additive regression trees" by Chipman, Hugh A., Edward I. George, and Robert E. McCulloch, The Annals of Applied Statistics, 2010, 4(1), 266-298.
