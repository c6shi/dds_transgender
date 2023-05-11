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

q88$GEDUCATION = factor(q88$GEDUCATION, ordered=T)
q88$Q93 = factor(q88$Q93, ordered=T)
q88$RACE_RECODE_CAT5 = factor(q88$RACE_RECODE_CAT5)
q88$POVERTYCAT_I = factor(q88$POVERTYCAT_I, ordered=T)
q88$HINC_I = factor(q88$HINC_I, ordered=T)
q88$HINC_I_strat = factor(q88$HINC_I_strat, ordered=T)

# remove the redundant income features --> use POVERTYCAT_I
q88sub = q88[, !names(q88) %in% c("HINC_I", "HINC_I_means", "HINC_I_strat")]

# all variables
q88logit = glm(Q88 ~ ., data=q88, family="binomial")
summary(q88logit)
BIC(q88logit)

# all variables, q88sub
q88sublogit = glm(Q88 ~ ., data=q88sub, family="binomial")
summary(q88sublogit)
BIC(q88sublogit)

# Forward & Backward Feature Selection, BIC
step_both_BIC = step(q88logit, direction="both",
                     scope=list(lower=Q88 ~ TRANS_CIS,
                                upper=Q88 ~ .^2), k=log(nrow(q88)))
summary(step_both_BIC)
BIC(step_both_BIC)

# Forward & Backward Feature Selection, BIC, q88sub
step_both_BIC_sub = step(q88sublogit, direction="both",
                         scope=list(lower=Q88 ~ TRANS_CIS,
                                    upper=Q88 ~ .^2), k=log(nrow(q88sub)))
summary(step_both_BIC_sub)
BIC(step_both_BIC_sub)

# Forward & Backward Feature Selection, BIC, regsubsets
ffs = regsubsets(Q88 ~ ., data=q88, method="forward")
summary(ffs)
coef(ffs, 6)
bfs = regsubsets(Q88 ~ ., data=q88, method="backward")
summary(bfs)
coef(bfs, 6)

# Forward & Backward Feature Selection, BIC, regsubsets, q88sub
ffs.sub = regsubsets(Q88 ~ ., data=q88sub, method="forward")
summary(ffs.sub)
coef(ffs.sub, 6)
bfs.sub = regsubsets(Q88 ~ ., data=q88sub, method="backward")
summary(bfs.sub)
coef(bfs.sub, 6)

# Feature Selection, Lasso
x = model.matrix(Q88 ~ ., q88)[, -1]
y = q88$Q88
train = sample(1:nrow(q88), nrow(q88) * 0.8)
test = (-train)
grid = 10^seq(10, -2, length = 100)
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
plot_glmnet(lasso.mod, xlim=c(0,-5))

cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
out = glmnet(x, y, alpha=1, lambda=grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:ncol(x)+1,]
lasso.coef
lasso.coef[lasso.coef!=0]

# Feature Selection, Lasso, q88sub
x.sub = model.matrix(Q88 ~ ., q88sub)[, -1]
y.sub = q88sub$Q88
lasso.mod.sub = glmnet(x.sub[train,], y.sub[train], alpha=1, lambda=grid)
plot(lasso.mod.sub)
plot_glmnet(lasso.mod.sub, xlim=c(0,-5))

cv.out.sub = cv.glmnet(x.sub[train,], y[train], alpha=1)
plot(cv.out.sub)
bestlam.sub = cv.out.sub$lambda.min
bestlam
out.sub = glmnet(x.sub, y.sub, alpha=1, lambda=grid)
lasso.coef.sub = predict(out.sub, type="coefficients", s=bestlam.sub)[1:ncol(x.sub)+1,]
lasso.coef.sub
lasso.coef.sub[lasso.coef.sub!=0]

# refit logistic regression model using step_both_BIC (and step_both_BIC_sub)
q88logit_stepwise = glm(Q88 ~ TRANS_CIS + AGE + Q93 + POVERTYCAT_I, data=q88sub, family="binomial")
summary(q88logit_stepwise)
BIC(q88logit_stepwise)

# refit logistic regression model using ffs
q88logit_ffs = glm(Q88 ~ TRANS_CIS + POVERTYCAT_I + AGE + Q93 + HINC_I + RACE_RECODE_CAT5, data=q88, family="binomial")
summary(q88logit_ffs)
BIC(q88logit_ffs)

# refit logistic regression model using bfs
q88logit_bfs = glm(Q88 ~ TRANS_CIS + POVERTYCAT_I + AGE + Q93 + HINC_I + GEDUCATION, data=q88, family="binomial")
summary(q88logit_bfs)
BIC(q88logit_bfs)

# refit logistic regression model using ffs_sub
q88logit_ffs_sub = glm(Q88 ~ TRANS_CIS + POVERTYCAT_I + AGE + Q93 + RACE_RECODE_CAT5, data=q88sub, family="binomial")
summary(q88logit_ffs_sub)
BIC(q88logit_ffs_sub)

# refit logistic regression model using bfs_sub
q88logit_bfs_sub = glm(Q88 ~ TRANS_CIS + POVERTYCAT_I + AGE + Q93 + GEDUCATION, data=q88sub, family="binomial")
summary(q88logit_bfs_sub)
BIC(q88logit_bfs_sub)

# refit logistic regression model using lasso.coef
q88logit_lasso = glm(Q88 ~ TRANS_CIS + HINC_I + AGE + RACE_RECODE_CAT5 + Q93 + POVERTYCAT_I, 
                     data=q88, 
                     family="binomial"
                     )
summary(q88logit_lasso)
BIC(q88logit_lasso)

# refit logistic regression model using lasso.coef.sub
q88logit_lasso.sub = glm(Q88 ~ TRANS_CIS + AGE + RACE_RECODE_CAT5 + Q93 + POVERTYCAT_I, data=q88sub, family="binomial")
summary(q88logit_lasso.sub)
BIC(q88logit_lasso.sub)

# graph q88logit_stepwise
plot(Q88 ~ TRANS_CIS + AGE + Q93 + POVERTYCAT_I, data=q88sub)
plot(allEffects(q88logit_stepwise))


##### SCRATCH

##### v1: G
q88logitv1 = glm(Q88 ~ TRANS_CIS, data=q88, family="binomial")
summary(q88logitv1)

##### v2a: G, I (14 ordinal categories)
q88$HINC_I = factor(q88$HINC_I)
q88logitv2a = glm(Q88 ~ TRANS_CIS + HINC_I, data=q88, family="binomial")
summary(q88logitv2a)

##### v2b: G, I (means of bins standardized, continuous quantitative)
q88$HINC_I_dummy = q88$HINC_I_means
q88logitv2b = glm(Q88 ~ TRANS_CIS + HINC_I_means, data=q88, family="binomial")
summary(q88logitv2b)
vif(q88logitv2b)

##### v2c: G, I (3 ordinal categories)
q88$HINC_I_strat = factor(q88$HINC_I_strat)
q88logitv2c = glm(Q88 ~ TRANS_CIS + HINC_I_strat, data=q88, family="binomial")
summary(q88logitv2c)
vif(q88logitv2c)

##### v2d: G, A (discrete quantitative)
q88logitv2d = glm(Q88 ~ TRANS_CIS + AGE, data=q88, familiy="binomial")
summary(q88logitv2d)
vif(q88logitv2d)
BIC(q88logitv2d)

##### v2e: G, R (5 categories)

##### v2f: G, Q93 (5 ordinal categories)

##### v2d: G, I (means of bin standardized), GI (1 interaction term)
q88logitv2d = glm(Q88 ~ TRANS_CIS + HINC_I_means + TRANS_CIS:HINC_I_means, data=q88, family="binomial")
summary(q88logitv2d)


##### v3a: G, A
q88logitv3a = glm(Q88 ~ TRANS_CIS + AGE, data=q88, family="binomial")
summary(q88logitv3a)

##### v3b: G, I, A
q88logitv3b = glm(Q88 ~ TRANS_CIS + HINC_I_means + AGE, data=q88, family="binomial")
summary(q88logitv3b)
vif(q88logitv3b)

##### v3c: G, I, A, GA
q88logitv3c = glm(Q88 ~ TRANS_CIS + HINC_I_means + AGE + HINC_I_means:AGE, data=q88, family="binomial")
summary(q88logitv3c)
vif(q88logitv3c, type="terms")

# Forward & Backward Feature Selection, AIC
# step_both_AIC = step(q88logit, direction="both", 
#                      scope=list(lower=Q88 ~ TRANS_CIS,
#                                 upper=Q88 ~ .^2), k=log(2))
# summary(step_both_AIC)

