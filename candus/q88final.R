setwd("~/projects/dds_transgender/candus")

library(car)
library(flexmix)
library(MASS)
library(leaps)
library(glmnet)
library(plotmo)
library(dplyr)
library(ggplot2)
library(effects)

# Load in data
q88 = read.csv("q88.csv", header=T)
q88 = q88[, !names(q88) %in% c("X", "SEX", "GENDER_IDENTITY", "CURRENT_SEX")]

q88$GEDUCATION = factor(q88$GEDUCATION)
q88$Q93 = factor(q88$Q93)
q88$RACE_RECODE_CAT5 = factor(q88$RACE_RECODE_CAT5)
q88$POVERTYCAT_I = factor(q88$POVERTYCAT_I)
q88$HINC_I = factor(q88$HINC_I)
q88$HINC_I_strat = factor(q88$HINC_I_strat)

# remove the redundant income features --> use POVERTYCAT_I
q88sub = q88[, !names(q88) %in% c("HINC_I", "HINC_I_means", "HINC_I_strat")]

# best feature selection: stepwise both directions, BIC
q88logit = glm(Q88 ~ ., data=q88, family="binomial")
summary(q88logit)
step_both_BIC = step(q88logit, direction="both",
                     scope=list(lower=Q88 ~ TRANS_CIS,
                                upper=Q88 ~ .^2), k=log(nrow(q88)))
summary(step_both_BIC)
BIC(step_both_BIC)

# retrain model with step_both_BIC features
### how to only get the L, Q, C terms of an ordinal variable?
### ^ doesn't matter because glm used the same terms, but how did it know which ones to use?
q88logit_stepwise = glm(Q88 ~ TRANS_CIS + AGE + Q93 + POVERTYCAT_I + Q85, data=q88sub, family="binomial")
summary(q88logit_stepwise)
BIC(q88logit_stepwise)

# plot logistic regression curve
### how to plot this given all of the coefficients? confused
### plotting the logistic regression curve constrained to max 2 covariates?
##### some quantitative variable x, and trans vs cis encoded by color
# plot(Q88 ~ TRANS_CIS + AGE + Q93 + POVERTYCAT_I, data=q88sub)
plot(allEffects(q88logit_stepwise))


# test model with null model
pchisq(q88logit_stepwise$null.deviance, q88logit_stepwise$df.null, lower=F)

# test model with saturated model
pchisq(q88logit_stepwise$null.deviance - q88logit_stepwise$deviance, q88logit_stepwise$df.null - q88logit_stepwise$df.residual, lower=F)
