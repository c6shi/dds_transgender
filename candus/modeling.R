setwd("~/projects/dds_transgender/candus")

require(MASS)
require(ggplot2)
require(foreign)
require(Hmisc)
require(reshape2)

healthcare = read.csv("healthcare.csv", header=T)
head(healthcare)

lapply(healthcare[, c("TRANS_CIS", "SEX", "GENDER_IDENTITY", "Q82", "Q84")], "table")

ftable(xtabs(~ TRANS_CIS + Q84, data=healthcare))

Q84_ordered = factor(healthcare$Q84, 
                     ordered=T, 
                     levels=c("Very satisfied", "Mostly satisfied", "Neutral", "Mostly dissatisfied", "Very dissatisfied"))

# need to add covariates and interactions
model = polr(Q84_ordered ~ TRANS_CIS, data=healthcare, Hess=T)
summary(model)
exp(coef(model))


