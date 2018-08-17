NACC <- read.csv("shao07272018.csv") # 20041 obs

# select unique ID
NACC.uniqueID <- NACC[!duplicated(NACC$NACCID),] # 10881 obs
attach(NACC.uniqueID)

# select the 1st visit
#NACC.uniqueID <- NACC[NACC$NACCVNUM == 1,] # 5019 obs
#attach(NACC.uniqueID)

summary(NACCAGE)
table(SEX) # 2126 M, 2893 F
table(RACE)
table(NACCNORM) # Normal cognition at all visits to date; cross-sectional
table(NACCUDSD) # Cognitive status at UDS visit; longitudinal
table(EDUC) # Years of education; 99 means unknown

# --------------------------------------------
# Normal Subgroup

NACC.NORM <- NACC.uniqueID[NACC.uniqueID$NACCNORM == 1,]
NACC.NORM <- with(NACC.NORM, NACC.NORM[NACCAGE >= 60,]) # 4946
summary(NACC.NORM$NACCAGE)
table(NACC.NORM$SEX)
table(NACC.NORM$RACE)
table(NACC.NORM$EDUC)
sum(NACC.NORM$VISITYR <= 2016) # '3602 participants' in Weitraub's paper; 3858

NACC.NORM.2016 <- NACC.NORM[(NACC.NORM$VISITYR <= 2016) & (NACC.NORM$EDUC != 99),]
attach(NACC.NORM.2016)
with(NACC.NORM.2016, table(NACC.NORM.2016[(NACCAGE >= 60) & (NACCAGE < 70),]$EDUC))

# MOCA total scores MOCATOTS
MOCATOTS.sel <- MOCATOTS[(MOCATOTS >= 0) & (MOCATOTS <= 30)] # 3244
summary(MOCATOTS.sel)
sd(MOCATOTS.sel)
MOCATOTS.NORM <- hist(MOCATOTS.sel, 20, xlim = c(0, 30))

# CRAFTVRS
CRAFTVRS.sel <- CRAFTVRS[(CRAFTVRS >= 0) & (CRAFTVRS <= 44)] # 3245
summary(CRAFTVRS.sel)
sd(CRAFTVRS.sel)
CRAFTVRS.NORM<-hist(CRAFTVRS.sel, 20)

# MINTTOTS
MINTTOTS.sel <- MINTTOTS[(MINTTOTS >= 0) & (MINTTOTS <= 32)] # 3221
summary(MINTTOTS.sel)
sd(MINTTOTS.sel)
hist(MINTTOTS.sel, 20)

# Mean Neuropsychological Test Scores by Age Group
tmp <- MOCATOTS[(MOCATOTS >= 0) & (MOCATOTS <= 30) & (NACCAGE >= 70) & (NACCAGE < 80)]
summary(tmp)
sd(tmp)

# Multivariate Linear Regression
summary(lm(MOCATOTS ~ SEX + NACCAGE + EDUC, data = NACC.NORM.2016, subset = (MOCATOTS >= 9) & (MOCATOTS <= 30)))

summary(lm(CRAFTVRS ~ SEX + NACCAGE + EDUC, data = NACC.NORM.2016, subset = (CRAFTVRS >= 0) & (CRAFTVRS <= 44)))

summary(lm(MINTTOTS ~ SEX + NACCAGE + EDUC, data = NACC.NORM.2016, subset = (MINTTOTS >= 0) & (MINTTOTS <= 32)))

# GAM
normal.gam <- gam(MOCATOTS ~ s(SEX, bs = "mpi") + s(NACCAGE, bs = "mpi") + s(EDUC, bs = "mpi"), sp=rep(-1,3),data = NACC.NORM.2016, subset = (MOCATOTS >= 9) & (MOCATOTS <= 30))

# ---------------------------------------
# MCI Subgroup

NACC.MCI <- NACC.uniqueID[NACC.uniqueID$NACCUDSD == 3,] # 1992
NACC.MCI <- with(NACC.MCI, NACC.MCI[NACCAGE >= 60,]) # 1860
summary(NACC.MCI$NACCAGE)
table(NACC.MCI$SEX)
table(NACC.MCI$RACE)
table(NACC.MCI$EDUC)

NACC.MCI.2016 <- NACC.MCI[(NACC.MCI$VISITYR <= 2016) & (NACC.MCI$EDUC != 99),] # 1330
attach(NACC.MCI.2016)
with(NACC.MCI.2016, table(NACC.MCI.2016[(NACCAGE >= 60) & (NACCAGE < 70),]$EDUC))

# MOCA total scores MOCATOTS
MOCATOTS.sel <- MOCATOTS[(MOCATOTS >= 0) & (MOCATOTS <= 30)] # 1223
summary(MOCATOTS.sel)
sd(MOCATOTS.sel)
MOCATOTS.MCI <- hist(MOCATOTS.sel, 20, xlim = c(0, 30))

# CRAFTVRS
CRAFTVRS.sel <- CRAFTVRS[(CRAFTVRS >= 0) & (CRAFTVRS <= 44)] # 3245
summary(CRAFTVRS.sel)
sd(CRAFTVRS.sel)
CRAFTVRS.MCI <- hist(CRAFTVRS.sel, 20)

# MINTTOTS
MINTTOTS.sel <- MINTTOTS[(MINTTOTS >= 0) & (MINTTOTS <= 32)] # 3221
summary(MINTTOTS.sel)
sd(MINTTOTS.sel)
hist(MINTTOTS.sel, 20)


# ---------------------------------------
# AD Subgroup

NACC.AD <- NACC.uniqueID[NACC.uniqueID$NACCUDSD == 4,] # 2898
NACC.AD <- with(NACC.AD, NACC.AD[NACCAGE >= 60,]) # 2560
summary(NACC.AD$NACCAGE)
table(NACC.AD$SEX)
table(NACC.AD$RACE)
table(NACC.AD$EDUC)

NACC.AD.2016 <- NACC.AD[(NACC.AD$VISITYR <= 2016) & (NACC.AD$EDUC != 99),] # 1981
attach(NACC.AD.2016)
with(NACC.AD.2016, table(NACC.AD.2016[(NACCAGE >= 60) & (NACCAGE < 70),]$EDUC))

# MOCA total scores MOCATOTS
MOCATOTS.sel <- MOCATOTS[(MOCATOTS >= 0) & (MOCATOTS <= 30)] # 1644
summary(MOCATOTS.sel)
sd(MOCATOTS.sel)
MOCATOTS.AD <- hist(MOCATOTS.sel, 20, xlim = c(0, 30))

# CRAFTVRS
CRAFTVRS.sel <- CRAFTVRS[(CRAFTVRS >= 0) & (CRAFTVRS <= 44)] # 3245
summary(CRAFTVRS.sel)
sd(CRAFTVRS.sel)
CRAFTVRS.AD <- hist(CRAFTVRS.sel, 20)

# MINTTOTS
MINTTOTS.sel <- MINTTOTS[(MINTTOTS >= 0) & (MINTTOTS <= 32)] # 3221
summary(MINTTOTS.sel)
sd(MINTTOTS.sel)
hist(MINTTOTS.sel, 20)

# ---------------------------------
# Cross-group Comparisons

plot(MOCATOTS.NORM, col = rgb(1, 0, 0, 1 / 4), xlim = c(0, 30))
plot(MOCATOTS.MCI, col = rgb(0, 1, 0, 1 / 4), add = TRUE)
plot(MOCATOTS.AD, col = rgb(0, 0, 1, 1 / 4), add = TRUE)

plot(CRAFTVRS.NORM, col = rgb(1, 0, 0, 1 / 4), xlim = c(0, 30))
plot(CRAFTVRS.MCI, col = rgb(0, 1, 0, 1 / 4), add = TRUE)
plot(CRAFTVRS.AD, col = rgb(0, 0, 1, 1 / 4), add = TRUE)