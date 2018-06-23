April 27, 2007

- Each dataset has its own directory, in data/DATANAME/
- Each directory contains 5 files:
-------- file listing -----
  1. "x.txt", the predictor variables

  2. "y.txt", the response used in the CGM 2007 NIPS paper.  It is
standardized to have mean 0 and variance 1. It may also have had a
nonlinear transformation, before standardization.  See below.

  3. "origy.txt", the untransformed response.  It's unlikely you need this.

  4. "partitions.txt", a 20-column matrix indicating how CGM performed the
NIPS experiment.  In each column, a "0" indicates the test set.  Integers
1-5 indicate cross-validation folds within the training set.  It's a
standard approach: Select optimal model via 5-fold CV, refit optimal model
on all 5 folds, predict on test set.  This whole process was repeated 20
times for the 20 different train/test partitions.

  5. "info.txt" a data file describing the raw data matrices.  Each row
gives the variable type of the corresponding columns of "x.txt", with
c=categorical, n=numeric, d=dependent variable (ie the response).  In fact
we've removed the response column from x.txt, and put it in separate files
(y.txt, origy.txt).
-------- end file listing ---

- Transformations are indicated in the file "runsummary.dat" in this
  directory.  Let origy=the original response variable.  Then response y is
  given by :
   y=log(origy)    if "l"
   y=origy         if "n"
   y=log(origy+1)  if "l1"
   y=sqrt(y)       if "s"
  This is mostly for information.  If you use the y.txt the transformations
  have already been done, and you should get results directly comparable to
  our NIPS paper
   
- For users of R, I have a simple script, "example.R" that reads in each
  file, builds a linear model on all 5 folds, and calculates a test-set
  MSE.  The average of the test set RMSEs (i.e., sqrt(MSE)) should be about
  0.61 if you use the first partition for each dataset only.


---
Hugh Chipman
firstname.lastname@acadiau.ca
