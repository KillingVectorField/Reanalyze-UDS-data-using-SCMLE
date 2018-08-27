NACC <- read.csv("shao07272018.csv") # 20041 obs

# select unique ID
NACC.uniqueID <- NACC[!duplicated(NACC$NACCID),] # 10881 obs

attach(NACC.uniqueID)
summary(NACCAGE)
table(SEX) # 2126 M, 2893 F
table(RACE)
table(NACCNORM) # Normal cognition at all visits to date; cross-sectional
table(CDRGLOB)
table(NACCUDSD) # Cognitive status at UDS visit; longitudinal
table(EDUC) # Years of education; 99 means unknown
detach(NACC.uniqueID)

# --------------------------------------------
# Normal Subgroup

NACC.NORM <- NACC.uniqueID[(NACC.uniqueID$NACCNORM == 1) & (NACC.uniqueID$CDRGLOB == 0),] # 5190
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
# range from 0 to 30, not available when MOCATOTS == 88 | MOCATOTS == -4
#outcome <- "MOCATOTS"
#lower.outcome <- 0
#upper.outcome <- 30

## CRAFTVRS
outcome <- "CRAFTVRS"
lower.outcome <- 0
upper.outcome <- 44

outcome.index <- (NACC.NORM[outcome] >= lower.outcome) & (NACC.NORM[outcome] <= upper.outcome) # 4474
summary(NACC.NORM[outcome.index, outcome])
sd(NACC.NORM[outcome.index, outcome])
outcome.NORM <- hist(NACC.NORM[outcome.index, outcome], 20, xlim = c(lower.outcome, upper.outcome)) #skewed


# Multivariate Linear Regression
by(NACC.NORM[outcome.index, outcome], NACC.NORM[outcome.index, 'SEX'], hist)
plot(sort(unique(NACC.NORM[outcome.index, 'NACCAGE'])), by(NACC.NORM[outcome.index, outcome], NACC.NORM[outcome.index, 'NACCAGE'], mean)) # the shape looks like decreasing and concave
plot(sort(unique(NACC.NORM[outcome.index, 'EDUC'])), by(NACC.NORM[outcome.index, outcome], NACC.NORM[outcome.index, 'EDUC'], mean)) # the shape looks like increasing and concave
summary(outcome.lm <- lm(as.formula(paste(outcome, "~ SEX + NACCAGE + EDUC")), data = NACC.NORM, subset = outcome.index)) # SEX, AGE, EDUC all significant

# Diagnosis of residuals
car::residualPlots(outcome.lm) # curvature tests rejected: the relationships between Y and EDUC and AGE are not linear. So linear model is not good enough.
plot(fitted.values(outcome.lm), residuals(outcome.lm) ^ 2)

# ordinary GAM (without shape constraint)
require(scam)
summary(outcome.gam <- gam(as.formula(paste(outcome, "~ SEX + s(NACCAGE) + s(EDUC)")), data = NACC.NORM, subset = outcome.index)) # GCV=5.8794, aic=20624.09, deviance=26104.58
plot(outcome.gam)
hist(residuals(outcome.gam))
car::residualPlot(outcome.gam)
plot(outcome.gam$fitted.values, outcome.gam$residuals ^ 2)
plot(outcome.gam$model$NACCAGE, outcome.gam$residuals ^ 2)
plot(outcome.gam$model$EDUC, outcome.gam$residuals ^ 2)

# SCAM by Simon N. Wood
summary(outcome.scam.1 <- scam(as.formula(paste(outcome, ' ~ SEX + s(NACCAGE, bs = "mpd", m = 2) + s(EDUC, bs = "mpi", m = 2)')), data = NACC.NORM[outcome.index,])) # GCV=5.8815, aic=20625.75, deviance=26190.94
plot(outcome.scam.1)
hist(residuals(outcome.scam.1))
car::residualPlot(outcome.scam.1)
plot(fitted.values(outcome.scam.1), residuals(outcome.scam.1) ^ 2)

summary(outcome.scam.2 <- scam(as.formula(paste(outcome, '~ SEX + s(NACCAGE, bs = "mdcv", m = 2) + s(EDUC, bs = "mpi", m = 2)')), data = NACC.NORM[outcome.index,])) # GCV=5.8925, aic=20634.09, deviance=26284.95
plot(outcome.scam.2)
hist(residuals(outcome.scam.2))
car::residualPlot(outcome.scam.2)
plot(fitted.values(outcome.scam.2), residuals(outcome.scam.2) ^ 2)

# SCMLE by J. Samworth
require(scar)
plot(outcome.scar.1 <- scar(x = as.matrix(NACC.NORM[outcome.index, c('SEX', 'NACCAGE', 'EDUC')]), y = NACC.NORM[outcome.index, outcome], shape = c("l", "de", "in")))
outcome.scar.1$deviance # 27201.81
fitted.scar.1 <- predict(outcome.scar.1, newdata = as.matrix(NACC.NORM[outcome.index, c('SEX', 'NACCAGE', 'EDUC')]))
residuals.scar.1 <- NACC.NORM[outcome.index, outcome] - fitted.scar.1
hist(residuals.scar.1)
plot(fitted.scar.1, residuals.scar.1)
plot(fitted.scar.1, residuals.scar.1 ^ 2)

plot(outcome.scar.2 <- scar(x = as.matrix(NACC.NORM[outcome.index, c('SEX', 'NACCAGE', 'EDUC')]), y = NACC.NORM[outcome.index, outcome], shape = c("l", "ccvde", "ccvin")))
outcome.scar.2$deviance # 26239.2
fitted.scar.2 <- predict(outcome.scar.2, newdata = as.matrix(NACC.NORM[outcome.index, c('SEX', 'NACCAGE', 'EDUC')]))
residuals.scar.2 <- NACC.NORM[outcome.index, outcome] - fitted.scar.2
hist(residuals.scar.2)
plot(fitted.scar.2, residuals.scar.2)
plot(fitted.scar.2, residuals.scar.2 ^ 2)

##------include interatction?-------------
#summary(outcome.gam.ia <- gam(as.formula(paste(outcome, '~ SEX + s(NACCAGE)+ s(NACCAGE, by = SEX) + s(EDUC)+ s(EDUC, by = SEX)')), data = NACC.NORM, subset = outcome.index)) # GCV=5.8794, aic=20624.09, deviance=26104.58
#plot(outcome.gam.ia)
#hist(residuals(outcome.gam))
#residualPlot(outcome.gam)

# -----------------------
# Test set results
test <- function(outcome = outcome, valid.index = outcome.index, k = 10) {
    NACC.NORM.valid <- NACC.NORM[valid.index,]
    set.seed(3)
    #trainingRows <- createDataPartition(NACC.NORM.valid$MOCATOTS, p = .70)[[1]] # 3133
    #cat("training set:", length(trainingRows), "\n")
    #cat("test set:", nrow(NACC.NORM.valid) - length(trainingRows), "\n")
    cvSplits <- caret::createFolds(NACC.NORM.valid[, outcome], k = k, returnTrain = TRUE)
    pred.lm <- pred.gam <- pred.scam.1 <- pred.scam.2 <- pred.scar.1 <- pred.scar.2 <- rep(0, length(NACC.NORM.valid[, outcome]))
    for (trainingRows in cvSplits) {
        # OLS
        formu <- as.formula(paste(outcome, "~ SEX + NACCAGE + EDUC"))
        model.lm <- lm(formu, data = NACC.NORM.valid, subset = trainingRows)
        pred.lm[-trainingRows] <- predict(model.lm, newdata = NACC.NORM.valid[-trainingRows,])
        # gam
        formu <- as.formula(paste(outcome, "~ SEX + s(NACCAGE) + s(EDUC)"))
        model.gam <- gam(formu, data = NACC.NORM.valid, subset = trainingRows)
        pred.gam[-trainingRows] <- predict(model.gam, newdata = NACC.NORM.valid[-trainingRows,])
        # scam
        formu <- as.formula(paste(outcome, '~ SEX + s(NACCAGE, bs = "mpd", m = 2) + s(EDUC, bs = "mpi", m = 2)'))
        model.scam.1 <- scam(formu, data = NACC.NORM.valid[trainingRows,])
        pred.scam.1[-trainingRows] <- predict(model.scam.1, newdata = NACC.NORM.valid[-trainingRows,])
        formu <- as.formula(paste(outcome, '~ SEX + s(NACCAGE, bs = "mdcv", m = 2) + s(EDUC, bs = "mpi", m = 2)'))
        model.scam.2 <- scam(formu, data = NACC.NORM.valid[trainingRows,])
        pred.scam.2[-trainingRows] <- predict(model.scam.2, newdata = NACC.NORM.valid[-trainingRows,])
        # scmle
        #model.scar.1 <- scar(x = as.matrix(NACC.NORM.valid[trainingRows, c('SEX', 'NACCAGE', 'EDUC')]), y = NACC.NORM.valid$MOCATOTS[trainingRows], shape = c("l", "de", "in"))
        #pred.scar.1[-trainingRows] <- predict(model.scar.1, newdata = as.matrix(NACC.NORM.valid[-trainingRows, c('SEX', 'NACCAGE', 'EDUC')]))
        model.scar.2 <- scar(x = as.matrix(NACC.NORM.valid[trainingRows, c('SEX', 'NACCAGE', 'EDUC')]), y = NACC.NORM.valid[trainingRows, outcome], shape = c("l", "ccvde", "ccvin"))
        pred.scar.2[-trainingRows] <- predict(model.scar.2, newdata = as.matrix(NACC.NORM.valid[-trainingRows, c('SEX', 'NACCAGE', 'EDUC')]))
    }


    MSE.lm <- mean((NACC.NORM.valid[, outcome] - pred.lm) ^ 2)
    MSE.gam <- mean((NACC.NORM.valid[, outcome] - pred.gam) ^ 2)
    MSE.scam.1 <- mean((NACC.NORM.valid[, outcome] - pred.scam.1) ^ 2)
    MSE.scam.2 <- mean((NACC.NORM.valid[, outcome] - pred.scam.2) ^ 2)
    #MSE.scar.1 <- mean((NACC.NORM.valid[,outcome] - pred.scar.1) ^ 2)
    MSE.scar.2 <- mean((NACC.NORM.valid[, outcome] - pred.scar.2) ^ 2)

    return(c(MSE.lm = MSE.lm, MSE.gam = MSE.gam, MSE.scam.1 = MSE.scam.1, MSE.scam.2 = MSE.scam.2, MSE.scar.2 = MSE.scar.2))
}

#test(outcome, outcome.index)

#-------------Z-score comparison-----------------------
Z_score.comparison <- function(outcome.model = outcome.scam.1, outcome = outcome, need.plot = TRUE, need.SD.model = FALSE) {
    if (need.SD.model) {
        #-------------model of SD (heterogeneous SD)---------
        #summary(SD.model <- gam(res.abs ~ SEX + s(NACCAGE) + s(EDUC), data = cbind(NACC.NORM[outcome.index,], res.abs = abs(residuals(outcome.model)))))
        summary(SD.model <- gam(res.squared ~ SEX + s(NACCAGE) + s(EDUC), data = cbind(NACC.NORM[outcome.index,], res.squared = residuals(outcome.model) ^ 2))) # or squared residuals?
        # when we use residuals from lm, the R-sq(adj) is 0.00917
        # when we use residuals from scam.1, the R-sq(adj) is 0.0046


        # SD model test
        cat("Naive (homogenenous) SD:", SD.naive <- sqrt(mean(residuals(outcome.model) ^ 2)), '\n')
        sqrt(predict(SD.model, newdata = data.frame(SEX = c(1, 0, 0), EDUC = c(30, 9, 20), NACCAGE = c(70, 65, 90))))

        #----------Z-score----------------
        # use SD model.
        Z_score <- function(outcome.model = outcome.model, outcome = outcome, newdata) {
            return((newdata[, outcome] - predict(outcome.model, newdata[, c("SEX", "NACCAGE", "EDUC")])) / sqrt(predict(SD.model, newdata[, c("SEX", "NACCAGE", "EDUC")])))
        }
    }

    else { #homogeneous SD is used
        cat("Naive (homogenenous) SD:", SD.naive <- sqrt(mean(residuals(outcome.model) ^ 2)), '\n')
        #----------Z-score----------------
        # use homogeneous SD.
        Z_score <- function(outcome.model = outcome.model, outcome = outcome, newdata) {
            return((newdata[, outcome] - predict(outcome.model, newdata[, c("SEX", "NACCAGE", "EDUC")])) / SD.naive)
        }
    }

    #------------NORM-------------
    NORM.Z_score <- Z_score(outcome.model = outcome.model, outcome = outcome, newdata = NACC.NORM[outcome.index,]) #4472

    #-------------MCI-------------
    NACC.MCI <- NACC.uniqueID[NACC.uniqueID$NACCUDSD == 3,] # 1992
    NACC.MCI <- with(NACC.MCI, NACC.MCI[(NACCAGEB >= 45) & (EDUC != 99),]) #1960
    MCI.outcome.index <- (NACC.MCI[outcome] >= lower.outcome) & (NACC.MCI[outcome] <= upper.outcome)
    MCI.Z_score <- Z_score(outcome.model = outcome.model, outcome = outcome, newdata = NACC.MCI[MCI.outcome.index,])

    #------------Dementia------------
    NACC.Dementia <- NACC.uniqueID[NACC.uniqueID$NACCUDSD == 4,] # 2898
    NACC.Dementia <- with(NACC.Dementia, NACC.Dementia[(NACCAGEB >= 45) & (EDUC != 99),]) #2832
    Dementia.outcome.index <- (NACC.Dementia[outcome] >= lower.outcome) & (NACC.Dementia[outcome] <= upper.outcome)
    Dementia.Z_score <- Z_score(outcome.model = outcome.model, outcome = outcome, newdata = NACC.Dementia[Dementia.outcome.index,])
    

    #--------------Distribution Comparison---------
    if (need.plot) {
        if (need.SD.model) {plot(SD.model) }
        NORM.Z_score.hist <- hist(NORM.Z_score, 30, freq = FALSE)
        MCI.Z_score.hist <- hist(MCI.Z_score, 30, freq = FALSE)
        Dementia.Z_score.hist <- hist(Dementia.Z_score, 30, freq = FALSE)
        plot(NORM.Z_score.hist, freq = FALSE, col = rgb(1, 0, 0, 1 / 4), xlim = c(-5, 5))
        plot(MCI.Z_score.hist, freq = FALSE, col = rgb(0, 1, 0, 1 / 4), add = TRUE)
        plot(Dementia.Z_score.hist, freq = FALSE, col = rgb(0, 0, 1, 1 / 4), add = TRUE)
    }

    #----------labeled Z-score--------------
    labeled.Z_score <- data.frame(label = c(rep('norm', length(NORM.Z_score)), rep('MCI', length(MCI.Z_score)), rep('dementia', length(Dementia.Z_score))), Z_score = c(NORM.Z_score, MCI.Z_score, Dementia.Z_score))
    return(labeled.Z_score)
}

#-------Binary Classification: ROC---------
ROC <- function(labeled.Z_score.list, pos_label = 'MCI', neg_label = 'norm', leg.txt = c("monotone scam", "lm")) {
    i <- 0
    for (labeled.Z_score in labeled.Z_score.list) {
        i <- i + 1
        thres <- seq(-4, 4, length.out = 50)
        onevsone.index <- (labeled.Z_score['label'] == pos_label) | (labeled.Z_score['label'] == neg_label)
        TPR <- vapply(thres, function(thres) {
            sum((labeled.Z_score[onevsone.index, 'label'] == pos_label) & (labeled.Z_score[onevsone.index, 'Z_score'] < thres)) / sum(labeled.Z_score[onevsone.index, 'label'] == pos_label)
        }, FUN.VALUE = 0)
        FPR <- vapply(thres, function(thres) {
            sum((labeled.Z_score[onevsone.index, 'label'] == neg_label) & (labeled.Z_score[onevsone.index, 'Z_score'] < thres)) / sum(labeled.Z_score[onevsone.index, 'label'] == neg_label)
        }, FUN.VALUE = 0)
        if (i == 1) {
            plot(FPR, TPR, type = "l",col=i, main = paste("pos:", pos_label, "neg:", neg_label))
        }
        else {
            points(FPR, TPR, type = "l", col = i)
        }
    }
    legend("bottomright", leg.txt, col = 1:i, lty = rep(1, i))
}

ROC(list(Z_score.comparison(outcome.scam.1, outcome, need.plot = TRUE, need.SD.model = FALSE),
    Z_score.comparison(outcome.scam.1, outcome, need.plot = FALSE, need.SD.model = TRUE),
    Z_score.comparison(outcome.lm, outcome, need.plot = FALSE, need.SD.model = FALSE),
    Z_score.comparison(outcome.lm, outcome, need.plot = FALSE, need.SD.model = TRUE)),
    pos_label = 'MCI',
    neg_label = 'norm',
    leg.txt = c("monotone scam with naive SD",
                "monotone scam with adjusted SD",
                "lm with naive SD",
                "lm with adjusted SD"))


#ROC(4, 3)