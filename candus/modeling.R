setwd("~/projects/dds_transgender/candus")

require(coin)
require(MASS)
require(ggplot2)
require(foreign)
require(Hmisc)
require(reshape2)

# Two-Proportion Z-Test
two_prop_z = function(X_T, X_C, n, m) {
  p_T = X_T / n
  p_C = X_C / m
  p_e = (X_T + X_C) / (n + m)
  Z = (p_T - p_C) / sqrt((p_e * (1 - p_e) / n) + (p_e * (1 - p_e) / m))
  p_value = pnorm(Z, lower=F)
  return (p_value)
}

# Mann Whitney U Test
q84 = read.csv("q84.csv", header=T)74
q84.trans = q84$X1
q84.cis = q84$X2
wilcox.test(q84.trans, q84.cis)


# Q85
X_T_85 = 86
X_C_85 = 206
n_85 = 273
m_85 = 1138
p_e = (X_T + X_C) / (n + m)
Z = ((X_T / n) - (X_C / m)) / sqrt(((p_e * (1 - p_e)) / n) + (p_e * (1 - p_e) / m))
pnorm(Z, lower=F)
two_prop_z(X_T_85, X_C_85, n_85, m_85)

# Q85 + stratified income
# high
prop.test(x=c(14, 50), n=c(46, 350), alternative='greater')
# mid
prop.test(x=c(18, 66), n=c(70, 370), alternative='greater')
# low
prop.test(x=c(54, 90), n=c(157, 418), alternative='greater')

# Q88
X_T_88 = 74
X_C_88 = 119
n_88 = 269
m_88 = 1133
two_prop_z(X_T_88, X_C_88, n_88, m_88)
p_T = X_T / n
p_C = X_C / m
p_e = (X_T + X_C) / (n + m)
Z = (p_T - p_C) / sqrt((p_e * (1 - p_e) / n) + (p_e * (1 - p_e) / m))
pnorm(Z, lower=F)

prop.test(x=c(74, 119), n=c(269, 1133), alternative='greater', correct=F)

# Q88 + stratified income
# high
prop.test(x=c(5, 13), n=c(45, 346), alternative='greater', correct=T)
# middle
prop.test(x=c(14, 30), n=c(70, 368), alternative='greater', correct=T)
# low
prop.test(x=c(55, 76), n=c(99, 419), alternative='greater', correct=T)


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


