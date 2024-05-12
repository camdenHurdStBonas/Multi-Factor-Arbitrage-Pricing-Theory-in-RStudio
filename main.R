#   Set up

#Cleared the work space
rm(list=ls())
#sets the variable "path" to the name of Working Directory
path <- ""
#Set the Working Directory using the variable "path"
setwd(path)

#   Libraries and Source Code

# https://cran.r-project.org/package=timetk
library(timetk)
# https://cran.r-project.org/package=plotly
library(plotly)
# https://cran.r-project.org/package=tibble
library(tibble)
#
library(e1071)
#
library(MASS)
#
library(sn)
#turn on the WHITESTRAP package; note that this is a user-created command and MUST BE CITED
library(whitestrap)
#turn on lmtest; note that this is a user-created command and MUST BE CITED
library(lmtest)
#turn on car; note that this is a user-created command and MUST BE CITED
library(car)
#import portfolioObject.R
source('portfolioObject.R')
#import stockModel.R; self created commands
source("stockModel.R")

myH0 <- c("MRP =0","SMB =0","HML =0","RMW =0", "CMA =0", "Vol.ROC =0")

# BTC model
BTC.model <- stockModel(head(BTC.df,length(BTC.df$Adj.Close)-1))()
summary(BTC.model)
white_test(BTC.model)
coeftest(BTC.model, vcov=hccm)
linearHypothesis(BTC.model, myH0, vcov=hccm)


# HOOD model
HOOD.model <- stockModel(HOOD.df)()
summary(HOOD.model)
white_test(HOOD.model)
coeftest(BTC.model, vcov=hccm)
linearHypothesis(BTC.model, myH0, vcov=hccm)

# VOO model
VOO.model <- stockModel(VOO.df)()
summary(VOO.model)
white_test(VOO.model)
coeftest(VOO.model, vcov=hccm)
linearHypothesis(VOO.model, myH0, vcov=hccm)

# PGR model
PGR.model <- stockModel(PGR.df)()
summary(PGR.model)
white_test(PGR.model)
coeftest(PGR.model, vcov=hccm)


# IWF model
IWF.model <- stockModel(IWF.df)()
summary(IWF.model)
white_test(IWF.model)
coeftest(IWF.model, vcov=hccm)
linearHypothesis(IWF.model, myH0, vcov=hccm)
