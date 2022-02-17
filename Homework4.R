library(ISLR)
library(dplyr)
library(leaps)

Train <- read.csv(file = 'C:/Users/54208/Google Drive/School/Graduate School/Masters/Spring_2021/MSA8150/HW4TrainData.csv')
Test <- read.csv(file = 'C:/Users/54208/Google Drive/School/Graduate School/Masters/Spring_2021/MSA8150/HW4TestData.csv')
Validation <- read.csv(file = 'C:/Users/54208/Google Drive/School/Graduate School/Masters/Spring_2021/MSA8150/HW4ValidationData.csv')

regfit_full = regsubsets(y~., data = Train, nvmax = 10)
summary(regfit_full)

reg_summary = summary(regfit_full)
names(reg_summary)

min(reg_summary$cp)
cp_list = reg_summary$cp
print(cp_list)
reg_summary$which

# Formula: x5,x7,x9

regfit_fwd = regsubsets(y~., data = Train, nvmax = 10, method = "forward")
summary(regfit_fwd)

reg_summary_fwd = summary(regfit_fwd)
min(reg_summary_fwd$cp)
cp_list = reg_summary_fwd$cp
print(cp_list)
reg_summary_fwd$which

# Formula: x1,x5, x6, x7, x9, x10

regfit_bwd = regsubsets(y~., data = Train, nvmax = 10, method = "backward")
summary(regfit_bwd)

reg_summary_bwd = summary(regfit_bwd)
min(reg_summary_bwd$cp)
cp_list = reg_summary_bwd$cp
print(cp_list)
reg_summary_bwd$which

# Formula: x5, x6, x7, x10