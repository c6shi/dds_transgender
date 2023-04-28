setwd("~/projects/dds_transgender/candus")

require(coin)
require(MASS)
require(ggplot2)
require(foreign)
require(Hmisc)
require(reshape2)

# Mann Whitney U Test
q84 = read.csv("q84.csv", header=T)74
q84.trans = q84$X1
q84.cis = q84$X2
wilcox.test(q84.trans, q84.cis)
X_T = 74
X_C = 119
n = 269
m = 1133

p_T = X_T / n
p_C = X_C / m
p_e = (X_T + X_C) / (n + m)
Z = (p_T - p_C) / sqrt((p_e * (1 - p_e) / n) + (p_e * (1 - p_e) / m))
pnorm(Z, lower=F)

# Fisher's Test
q88 = read.csv("q88.csv", header=T)
q88 = q88[, c("X1", "X2")]
chisq.test(q88)
fisher.test(q88)



# Ordinal Logistic Regression
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


