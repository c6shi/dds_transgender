setwd("~/projects/dds_transgender/candus")

library(car)
library(flexmix)

# Load in data
q88 = read.csv("q88.csv", header=T)

##### v1: G
q88logitv1 = glm(Q88 ~ TRANS_CIS, data=q88, family="binomial")
summary(q88logitv1)

##### v2a: G, I (14 ordinal categories)
q88$HINC_I = factor(q88$HINC_I)
q88logitv2a = glm(Q88 ~ TRANS_CIS + HINC_I, data=q88, family="binomial")
summary(q88logitv2a)

##### v2b: G, I (means of bins standardized, quantitative)
q88$HINC_I_dummy = q88$HINC_I_means
q88logitv2b = glm(Q88 ~ TRANS_CIS + HINC_I_means, data=q88, family="binomial")
summary(q88logitv2b)
vif(q88logitv2b)

##### v2c: G, I (3 ordinal categories)
q88$HINC_I_strat = factor(q88$HINC_I_strat)
q88logitv2c = glm(Q88 ~ TRANS_CIS + HINC_I_strat, data=q88, family="binomial")
summary(q88logitv2c)
vif(q88logitv2c)

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
