NACC <- read.csv("shao07272018.csv") # 20041 obs

# select unique ID
NACC.uniqueID <- NACC[!duplicated(NACC$NACCID),] # 10881 obs

attach(NACC.uniqueID)
summary(NACCAGE)
table(SEX) # 2126 M, 2893 F
table(RACE)
table(NACCNORM) # Normal cognition at all visits to date; cross-sectional
table(NACCUDSD) # Cognitive status at UDS visit; longitudinal
table(EDUC) # Years of education; 99 means unknown
detach(NACC.uniqueID)

# --------------------------------------------
# Normal Subgroup

NACC.NORM <- NACC.uniqueID[(NACC.uniqueID$NACCNORM == 1)
    & (NACC.uniqueID$CDRGLOB == 0),] # 5190
hist(NACC.NORM$NACCAGE)
# How to filter Age? Use 45 as the minimum Age?
NACC.NORM <- NACC.NORM[NACC.NORM$NACCAGE >= 45,] # 5109
summary(NACC.NORM$NACCAGE)
table(NACC.NORM$SEX)
table(NACC.NORM$RACE)
table(NACC.NORM$EDUC) # 35 with EDUC==99
NACC.NORM <- NACC.NORM[NACC.NORM$EDUC != 99,] # 5074

attach(NACC.NORM)

# Relationship between SEX, NACCAGE and EDUC
cor(NACCAGE, EDUC) # -0.0288 not correlated
by(NACC.NORM[c('NACCAGE', 'EDUC')], SEX, summary)
by(NACC.NORM$NACCAGE, SEX, hist)
by(NACC.NORM$EDUC, SEX, hist)

# MOCA total scores MOCATOTS
# not available when MOCATOTS == 88 | MOCATOTS == -4
MOCATOTS.index <- (MOCATOTS >= 0) & (MOCATOTS <= 30) # 4474
summary(MOCATOTS[MOCATOTS.index])
sd(MOCATOTS[MOCATOTS.index])
MOCATOTS.NORM <- hist(MOCATOTS[MOCATOTS.index], 20, xlim = c(0, 30)) #skewed


# Multivariate Linear Regression
# MOCATOTS
by(MOCATOTS[MOCATOTS.index], SEX[MOCATOTS.index], hist)
plot(sort(unique(NACCAGE[MOCATOTS.index])), by(MOCATOTS[MOCATOTS.index], NACCAGE[MOCATOTS.index], mean)) # the shape looks like decreasing and concave
plot(sort(unique(EDUC[MOCATOTS.index])), by(MOCATOTS[MOCATOTS.index], EDUC[MOCATOTS.index], mean)) # the shape looks like increasing and concave
summary(MOCATOTS.lm <- lm(MOCATOTS ~ SEX + NACCAGE + EDUC, data = NACC.NORM, subset = MOCATOTS.index)) # SEX, AGE, EDUC all significant

# Diagnosis of residuals of the linear model
car::residualPlots(MOCATOTS.lm) # curvature tests rejected: the relationships between Y and EDUC and AGE are not linear. So linear model is not good enough.

# CRAFTVRS
by(CRAFTVRS[CRAFTVRS.index], SEX[CRAFTVRS.index], hist)
plot(sort(unique(NACCAGE[CRAFTVRS.index])), by(CRAFTVRS[CRAFTVRS.index], NACCAGE[CRAFTVRS.index], mean)) # the shape looks like decreasing and concave
plot(sort(unique(EDUC[CRAFTVRS.index])), by(CRAFTVRS[CRAFTVRS.index], EDUC[CRAFTVRS.index], mean)) # the shape looks like increasing
summary(CRAFTVRS.lm <- lm(CRAFTVRS ~ SEX + NACCAGE + EDUC, data = NACC.NORM, subset = CRAFTVRS.index)) # SEX, AGE, EDUC all significant

# Diagnosis
car::residualPlots(CRAFTVRS.lm) # curvature tests

# ordinary GAM (without shape constraint)
require(scam)
summary(MOCATOTS.gam <- gam(MOCATOTS ~ SEX + s(NACCAGE) + s(EDUC), data = NACC.NORM, subset = MOCATOTS.index)) # GCV=5.8794, aic=20624.09, deviance=26104.58
plot.gam(MOCATOTS.gam)
# SCAM by Simon N. Wood 
summary(MOCATOTS.scam <- scam(MOCATOTS ~ SEX + s(NACCAGE, bs = "mpd", m = 2) + s(EDUC, bs = "mpi", m = 2), data = NACC.NORM[MOCATOTS.index,])) # GCV=5.8815, aic=20625.75, deviance=26190.94
plot.scam(MOCATOTS.scam)
summary(MOCATOTS.scam <- scam(MOCATOTS ~ SEX + s(NACCAGE, bs = "mdcv", m = 2) + s(EDUC, bs = "mpi", m = 2), data = NACC.NORM[MOCATOTS.index,])) # GCV=5.8925, aic=20634.09, deviance=26284.95
plot.scam(MOCATOTS.scam)
# SCMLE by J. Samworth
require(scar)
plot(MOCATOTS.scar <- scar(x = as.matrix(NACC.NORM[MOCATOTS.index, c('SEX', 'NACCAGE', 'EDUC')]), y = NACC.NORM$MOCATOTS[MOCATOTS.index], shape = c("l", "de", "in")))
MOCATOTS.scar$deviance # 27201.81
plot(MOCATOTS.scar <- scar(x = as.matrix(NACC.NORM[MOCATOTS.index, c('SEX', 'NACCAGE', 'EDUC')]), y = NACC.NORM$MOCATOTS[MOCATOTS.index], shape = c("l", "ccvde", "ccvin")))
MOCATOTS.scar$deviance # 26239.2



# -----------------------
# Test set results
require(caret)
NACC.NORM.valid <- NACC.NORM[MOCATOTS.index,]
set.seed(3)
trainingRows <- createDataPartition(NACC.NORM.valid$MOCATOTS, p = .70)[[1]] # 3133
cat("training set:", length(trainingRows), "\n")
cat("test set:", nrow(NACC.NORM.valid) - length(trainingRows), "\n")
# OLS
model.lm <- lm(MOCATOTS ~ SEX + NACCAGE + EDUC, data = NACC.NORM.valid, subset = trainingRows)
pred.lm <- predict(model.lm, newdata = NACC.NORM.valid[-trainingRows,])
# gam
model.gam <- gam(MOCATOTS ~ SEX + s(NACCAGE) + s(EDUC), data = NACC.NORM.valid, subset = trainingRows)
pred.gam <- predict(model.gam, newdata = NACC.NORM.valid[-trainingRows,])
# scam
model.scam.1 <- scam(MOCATOTS ~ SEX + s(NACCAGE, bs = "mpd", m = 2) + s(EDUC, bs = "mpi", m = 2), data = NACC.NORM.valid[trainingRows,])
pred.scam.1 <- predict(model.scam.1, newdata = NACC.NORM.valid[-trainingRows,])
model.scam.2 <- scam(MOCATOTS ~ SEX + s(NACCAGE, bs = "mdcv", m = 2) + s(EDUC, bs = "mpi", m = 2), data = NACC.NORM.valid[trainingRows,])
pred.scam.2 <- predict(model.scam.2, newdata = NACC.NORM.valid[-trainingRows,])
# scmle
model.scar <- scar(x = as.matrix(NACC.NORM.valid[trainingRows, c('SEX', 'NACCAGE', 'EDUC')]), y = NACC.NORM.valid$MOCATOTS[trainingRows], shape = c("l", "ccvde", "ccvin"))
pred.scar <- predict(model.scar, newdata = as.matrix(NACC.NORM.valid[-trainingRows, c('SEX', 'NACCAGE', 'EDUC')]))

print(MSE.lm <- mean((NACC.NORM.valid$MOCATOTS[-trainingRows] - pred.lm) ^ 2))
print(MSE.gam <- mean((NACC.NORM.valid$MOCATOTS[-trainingRows] - pred.gam) ^ 2))
print(MSE.scam.1 <- mean((NACC.NORM.valid$MOCATOTS[-trainingRows] - pred.scam.1) ^ 2))
print(MSE.scam.2 <- mean((NACC.NORM.valid$MOCATOTS[-trainingRows] - pred.scam.2) ^ 2))
print(MSE.scar <- mean((NACC.NORM.valid$MOCATOTS[-trainingRows] - pred.scar) ^ 2))