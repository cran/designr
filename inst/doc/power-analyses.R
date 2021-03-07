## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=TRUE, message=FALSE------------------------------------------------
library(lmerTest)
library(afex)
library(ggplot2)
library(dplyr)
library(designr)
library(gridExtra)
library(MASS)

## -----------------------------------------------------------------------------
# a) assume effect size + simulate data
x <- rep(c("x1","x2"), each=30)
y <- rep(c( 200, 220), each=30) + rnorm(60,0,50)
ggplot(data.frame(x,y), aes(x=x,y=y)) + geom_boxplot()
# b) run statistical model & get p-value
t.test(y ~ x)$p.value
# c) do this many times
nsim <- 1000
pVal <- c()
for (i in 1:nsim) {
  x <- rep(c("x1","x2"), each=30)
  y <- rep(c( 200, 220), each=30) + rnorm(60,0,50)
  # b) run statistical model & get p-value
  pVal[i] <- t.test(y ~ x)$p.value
}
# d) check how often it is significant
(power <- mean( pVal < 0.05 ))
# e)
# We could do this for different numbers of subjects and see when power is ~80%
# We could change the difference in means/residual standard deviations

## -----------------------------------------------------------------------------
# previous formulation
x <- rep(c("x1","x2"), each=30)
y <- rep(c( 200, 220), each=30) + rnorm(60,0,50)
# new:
x <- rep(c(-1,+1), each=30) # define predictor term: here, sum contrast coding (-1, +1)
y <- 210 + 10*x + rnorm(60,0,50) # simulate data
# run linear model
lm(y ~ x)
summary(lm(y ~ x))
ggplot(data.frame(x,y), aes(x=factor(x),y=y)) + geom_boxplot()

## -----------------------------------------------------------------------------
library(designr)

# one within-subject factor
# create design
design <-
  fixed.factor("X", levels=c("X1", "X2")) + # fixed effect
  random.factor("Subj", instances=5)        # random effect
dat <- design.codes(design) # create data frame (tibble) from experimental design
length(unique(dat$Subj)) # number of subjets
data.frame(dat) # look at data

# one between-subject factor
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", groups="X", instances=5)
dat <- design.codes(design)
length(unique(dat$Subj))
data.frame(dat)

# 2 (A: within-subjects) x 2 (B: between-subjects) design
design <-
  fixed.factor("A", levels=c("A1", "A2")) +
  fixed.factor("B", levels=c("B1", "B2")) +
  random.factor("Subj", groups="B", instances=5)
dat <- design.codes(design)
length(unique(dat$Subj))
data.frame(dat)

# 1 factor, 2 (crossed) random effects, within-subject, between-item factor
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=5) +
  random.factor("Item", groups="X", instances=2)
dat <- design.codes(design)
length(unique(dat$Subj))
length(unique(dat$Item))
data.frame(dat)

# within-subject, within-item factor
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=5) +
  random.factor("Item", instances=2)
dat <- design.codes(design)
length(unique(dat$Subj))
length(unique(dat$Item))
data.frame(dat)

# within-subject, within-item factor, latin square design
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=5) +
  random.factor("Item", instances=2) +
  random.factor(c("Subj", "Item"), groups=c("X"))
dat <- design.codes(design)
length(unique(dat$Subj))
length(unique(dat$Item))
data.frame(dat)


## -----------------------------------------------------------------------------

design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=30)
dat <- design.codes(design)
length(unique(dat$Subj))
(contrasts(dat$X) <- c(-1, +1)) # define a sum contrast
dat$Xn <- model.matrix(~X, dat)[,2] # convert this it a numeric variable


## -----------------------------------------------------------------------------
# simulate data
fix     <- c(200,10) # set fixed effects
sd_Subj <- c(30, 10) # set random effects standard deviations for subjects
sd_Res  <- 50        # set residual standard deviation
dat$ysim <- simLMM(form = ~ 1 + X + (1 + X | Subj), 
                   data   = dat, 
                   Fixef  = fix, 
                   VC_sd  = list(sd_Subj, sd_Res),
                   CP = 0.3,
                   empirical = TRUE)

## -----------------------------------------------------------------------------
# check group means
dat %>% group_by(X) %>% summarize(M=mean(ysim)) # the group means are EXACTLY 190 and 210. This is due to empirical=TRUE.
# check fixed effects + random effects
(m1 <- lmer(ysim ~ Xn + (Xn || Subj), data=dat, control=lmerControl(calc.derivs=FALSE))) # The fixed effects are EXACTLY as indicated. This is due to empirical=TRUE.
aov_car(ysim ~ 1 + Error(Subj/X), data=dat) # We can also run an ANOVA

## ---- eval=FALSE, warning=FALSE, cache=TRUE-----------------------------------
#  nsubj <- seq(10,100,1) # we vary the number of subjects from 10 to 100 in steps of 1
#  nsim <- length(nsubj) # number of simulations
#  COF <- COFaov <- data.frame()
#  warn <- c()
#  for (i in 1:nsim) { # i <- 1
#    #print(paste0(i,"/",nsim))
#    # create experimental design
#    design <-
#      fixed.factor("X", levels=c("X1", "X2")) +
#      random.factor("Subj", instances=nsubj[i])
#    dat <- design.codes(design)
#    nsj <- length(unique(dat$Subj))
#    # create contrast and store as numeric variable
#    contrasts(dat$X) <- c(-1, +1)
#    dat$Xn <- model.matrix(~X,dat)[,2]
#    # for each number of subjects, we run 10 simulations to obtain more stable results
#    for (j in 1:10) { # j <- 1
#      # simulate data
#      dat$ysim <- simLMM(~ X + (X | Subj), data=dat, Fixef=fix, VC_sd=list(sd_Subj, sd_Res), CP=0.3, empirical=FALSE, verbose=FALSE)
#      # ANOVA
#      AOV <- aov_car(ysim ~ 1 + Error(Subj/X), data=dat)
#      AOVcof <- summary(AOV)$univariate.tests[2,]
#      COFaov <- rbind(COFaov,c(nsj,AOVcof))
#      # LMM
#      #LMM <- lmer(ysim ~ Xn + (Xn || Subj), data=dat,
#      #            REML=FALSE, control=lmerControl(calc.derivs=FALSE))
#      # run lmer and capture any warnings
#      ww <- ""
#      suppressMessages(suppressWarnings(
#        LMM <- withCallingHandlers({
#          lmer(ysim ~ Xn + (Xn || Subj), data=dat, REML=FALSE,
#               control=lmerControl(calc.derivs=FALSE))
#          },
#          warning = function(w) { ww <<- w$message }
#          )
#      ))
#      LMMcof <- coef(summary(LMM))[2,]
#      COF <- rbind(COF,c(nsj,LMMcof,isSingular(LMM)))
#      warn[i] <- ww
#    }
#  }
#  #load(file="data/Power0.rda")
#  # Results for LMMs
#  names(COF) <- c("nsj","Estimate","SE","df","t","p","singular")
#  COF$warning <- warn
#  COF$noWarning <- warn==""
#  COF$sign   <- as.numeric(COF$p < .05 & COF$Estimate>0) # determine significant results
#  COF$nsjF   <- gtools::quantcut(COF$nsj, q=seq(0,1,length=10))
#  COF$nsjFL  <- plyr::ddply(COF,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
#  # Results for ANOVAs
#  names(COFaov) <- c("nsj","SS","numDF","ErrorSS","denDF","F","p")
#  COFaov$sign   <- as.numeric(COFaov$p < .05) # determine significant results
#  COFaov$nsjF   <- gtools::quantcut(COFaov$nsj, q=seq(0,1,length=10))
#  COFaov$nsjFL  <- plyr::ddply(COFaov,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
#  #save(COF, COFaov, file="data/Power0.rda")

## -----------------------------------------------------------------------------
load(file=system.file("power-analyses", "Power0.rda", package = "designr"))

mean(COF$singular)

## -----------------------------------------------------------------------------
mean(COF$noWarning)

## -----------------------------------------------------------------------------
COF$warning[1:20]

## ---- message=FALSE-----------------------------------------------------------
p1 <- ggplot(data=COF)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsjFL, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsjFL, y=sign))+
  geom_line(    stat="summary", aes(x=nsjFL, y=sign))
p2 <- ggplot(data=COFaov)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsjFL, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsjFL, y=sign))+
  geom_line(    stat="summary", aes(x=nsjFL, y=sign))
grid.arrange(p1,p2, nrow=1)

## -----------------------------------------------------------------------------
# determine number of subjects needed for a power of 60%
m0 <- loess(sign ~ nsj, data=COF)
COF$pred <- predict(m0)
idx <- COF$pred>0.6
min(COF$nsj[idx])

## -----------------------------------------------------------------------------

design <-
  fixed.factor("A", levels=c("A1", "A2")) +
  fixed.factor("B", levels=c("B1", "B2", "B3")) +
  random.factor("Subj", instances=60) #+
  #random.factor("Items", instances=5)
dat <- design.codes(design)
nrow(dat)
length(unique(dat$Subj))
head(dat,12)
# set contrasts
(contrasts(dat$A) <- c(-1, +1))
(contrasts(dat$B) <- contr.sdif(3))
# Recommendation: hypr-package for contrasts + Schad et al. (2020) JML
dat$An  <- model.matrix(~A,dat)[,2]
dat[,c("Bn1","Bn2")] <- model.matrix(~B,dat)[,2:3]


## -----------------------------------------------------------------------------
# create one factor
dat$F <- factor(paste0(dat$A, ".", dat$B))
levels(dat$F)
mm <- model.matrix(~ -1 + F, data=dat)
head(mm)

## -----------------------------------------------------------------------------
# simulate data
levels(dat$F)
fix     <- c(190,200,210,210,200,190) # set fixed effects
sd_Subj <- c( 30, 30, 30, 30, 30, 30) # set random effects standard deviations for subjects
sd_Res  <- 50        # set residual standard deviation
form    <- ~ -1 + F + (-1 + F | Subj)
dat$ysim <- simLMM(form, data=dat, Fixef=fix, VC_sd=list(sd_Subj, sd_Res), CP=0.85, empirical=TRUE)
# check group means
dat %>% group_by(A) %>% summarize(M=mean(ysim)) # the data show no main effect of A
dat %>% group_by(B) %>% summarize(M=mean(ysim)) # the data show no main effect of B
(tmp <- dat %>% group_by(A, B) %>% summarize(M=mean(ysim))) # there is an interaction
ggplot(data=tmp, aes(x=B, y=M, group=A, colour=A)) + geom_point() + geom_line()
# check fixed effects + random effects
#summary(lmer(ysim ~ -1 + F + (-1 + F | Subj), data=dat))
summary(lmer(ysim ~ An*(Bn1+Bn2) + (An*(Bn1+Bn2) || Subj), data=dat, control=lmerControl(calc.derivs=FALSE)))
aov_car(ysim ~ 1 + Error(Subj/(A*B)), data=dat)


## ---- eval=FALSE, cache=TRUE, message=FALSE-----------------------------------
#  nsubj <- seq(10,100,1) # again, we run from 10 to 100 subjects
#  nsim <- length(nsubj)
#  COF <- COFaov <- data.frame()
#  for (i in 1:nsim) { # i <- 1
#    #print(paste0(i,"/",nsim))
#    # create experimental design
#    design <-
#      fixed.factor("A", levels=c("A1", "A2")) +
#      fixed.factor("B", levels=c("B1", "B2", "B3")) +
#      random.factor("Subj", instances=nsubj[i]) # set number of subjects
#    dat <- design.codes(design)
#    nsj <- length(unique(dat$Subj))
#    # set contrasts & compute numeric predictor variables
#    contrasts(dat$A) <- c(-1, +1)
#    contrasts(dat$B) <- contr.sdif(3)
#    dat$An  <- model.matrix(~A,dat)[,2]
#    dat[,c("Bn1","Bn2")] <- model.matrix(~B,dat)[,2:3]
#    # compute factor F
#    dat$F <- factor(paste0(dat$A, ".", dat$B))
#    for (j in 1:4) { # j <- 1 # run 4 models for each number of subjects
#      # simulate data
#      dat$ysim <- simLMM(form, data=dat, Fixef=fix, VC_sd=list(sd_Subj, sd_Res), CP=0.85, empirical=FALSE, verbose=FALSE)
#      # ANOVA
#      AOV <- aov_car(ysim ~ 1 + Error(Subj/(A*B)), data=dat)
#      AOVcof <- summary(AOV)$univariate.tests[4,]
#      COFaov <- rbind(COFaov,c(nsj,AOVcof))
#      # LMM: perform model comparison
#      LMM1 <- lmer(ysim ~ An*(Bn1+Bn2) + (An*(Bn1+Bn2) || Subj), data=dat, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
#      LMM0 <- lmer(ysim ~ An+(Bn1+Bn2) + (An*(Bn1+Bn2) || Subj), data=dat, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
#      LMMcof <- c(coef(summary(LMM1))[5:6,5],
#                  as.numeric(data.frame(anova(LMM1,LMM0))[2,6:8]))
#      COF <- rbind(COF,c(nsj,LMMcof,isSingular(LMM1) & isSingular(LMM0)))
#    }
#  }
#  # results from LMMs
#  names(COF) <- c("nsj","p_An:Bn1","p_An:Bn2","Chisq","Df","p","singular")
#  COF$sign  <- as.numeric(COF$p < .05)
#  COF$nsjF  <- gtools::quantcut(COF$nsj, q=seq(0,1,length=10))
#  COF$nsjFL <- plyr::ddply(COF, "nsjF", transform, nsjFL=mean(nsj))$nsjFL
#  # results for ANOVA
#  names(COFaov) <- c("nsj","SS","numDF","ErrorSS","denDF","F","p")
#  COFaov$sign  <- as.numeric(COFaov$p < .05)
#  COFaov$nsjF  <- gtools::quantcut(COFaov$nsj, q=seq(0,1,length=10))
#  COFaov$nsjFL <- plyr::ddply(COFaov,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
#  #save(COF, COFaov, file="data/Power1.rda")

## -----------------------------------------------------------------------------
load(file=system.file("power-analyses", "Power1.rda", package = "designr"))
mean(COF$singular)

## ---- message=FALSE-----------------------------------------------------------
p1 <- ggplot(data=COF)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsjFL, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsjFL, y=sign))+
  geom_line(    stat="summary", aes(x=nsjFL, y=sign))
p2 <- ggplot(data=COFaov)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsjFL, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsjFL, y=sign))+
  geom_line(    stat="summary", aes(x=nsjFL, y=sign))
gridExtra::grid.arrange(p1,p2, nrow=1)

## ---- message=FALSE-----------------------------------------------------------
m0 <- loess(sign ~ nsj, data=COF)
COF$pred <- predict(m0)
idx <- COF$pred>0.8
min(COF$nsj[idx])

## ---- message=FALSE-----------------------------------------------------------
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=15) +
  random.factor("Item", groups="X", instances=2)
dat <- design.codes(design)
(contrasts(dat$X) <- c(-1, +1))
dat$Xn <- model.matrix(~X,dat)[,2]

# simulate data
fix     <- c(200, 5) # set fixed effects
sd_Subj <- c(30, 10) # set random effects standard deviations for subjects
sd_Item <- c(10)     # set random effects standard deviations for items
sd_Res  <- 50        # set residual standard deviation
dat$ysim <- simLMM(form=~ X + (X | Subj) + (1 | Item), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj, sd_Item, sd_Res), CP=0.3,
                   empirical=TRUE)
# check group means
dat %>% group_by(X) %>% summarize(M=mean(ysim))
# check fixed effects + random effects
summary(lmer(ysim ~ Xn + (Xn || Subj) + (1 | Item), data=dat, control=lmerControl(calc.derivs=FALSE)))

## ---- eval=FALSE, warnings=FALSE, cache=TRUE----------------------------------
#  nsubj  <- seq(10,100,4)
#  nsimSj <- length(nsubj)
#  nitem  <- seq(2,30,4)
#  nsimIt <- length(nitem)
#  COF <- data.frame()
#  for (i in 1:nsimSj) { # i <- 1
#  	#print(paste0(i,"/",nsimSj))
#    for (j in 1:nsimIt) { # j <- 1
#  	design <-
#  	  fixed.factor("X", levels=c("X1", "X2")) +
#  	  random.factor("Subj", instances=nsubj[i]) +
#  	  random.factor("Item", groups="X", instances=nitem[j])
#  	dat <- design.codes(design)
#  	nsj <- length(unique(dat$Subj))
#  	nit <- length(unique(dat$Item))
#  	contrasts(dat$X) <- c(-1, +1)
#  	dat$Xn <- model.matrix(~X,dat)[,2]
#  	for (j in 1:5) { # j <- 1
#  		dat$ysim <- simLMM(~ X + (X | Subj) + (1 | Item), data=dat,
#  		                   Fixef=fix, VC_sd=list(sd_Subj, sd_Item, sd_Res), CP=0.3,
#  		                   empirical=FALSE, verbose=FALSE)
#  		LMM <- lmer(ysim ~ Xn + (Xn || Subj) + (1 | Item), data=dat, REML=FALSE,
#  		                            control=lmerControl(calc.derivs=FALSE))
#  		LMMcof <- coef(summary(LMM))[2,]
#  		COF <- rbind(COF,c(nsj,nit,LMMcof, isSingular(LMM)))
#  	}
#  }
#  }
#  # results for LMM
#  names(COF)<- c("nsj","nit","Estimate","SE","df","t","p","singular")
#  COF$sign  <- as.numeric(COF$p < .05 & COF$Estimate>0)
#  COF$nsjF  <- gtools::quantcut(COF$nsj, q=seq(0,1,length=10))
#  COF$nsjFL <- plyr::ddply(COF,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
#  COF$nitF  <- gtools::quantcut(COF$nit, q=seq(0,1,length=5))
#  COF$nitFL <- plyr::ddply(COF,"nitF",transform,nitFL=mean(nsj))$nitFL
#  #save(COF, file="data/Power2.rda")

## ---- message=FALSE-----------------------------------------------------------
load(file=system.file("power-analyses", "Power2.rda", package = "designr"))
ggplot(data=COF) +
	geom_smooth(aes(x=nsj, y=sign, colour=factor(nitF)), se=FALSE)+
	geom_point(   stat="summary", aes(x=nsjFL, y=sign, colour=factor(nitF)))+
	geom_errorbar(stat="summary", aes(x=nsjFL, y=sign, colour=factor(nitF)))+
	geom_line(    stat="summary", aes(x=nsjFL, y=sign, colour=factor(nitF)))

# determine number of subjects needed for a power of 60%
idx <- which(COF$nit>46)
m0 <- loess(sign ~ nsj, data=COF[idx,])
COF$pred <- NA
COF$pred[idx] <- predict(m0)
min(COF$nsj[COF$pred>0.5], na.rm=TRUE)


## ---- message=FALSE-----------------------------------------------------------
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=15) +
  random.factor("Item", groups="X", instances=4)
dat <- design.codes(design)
length(unique(dat$Subj))
length(unique(dat$Item))
(contrasts(dat$X) <- c(-1, +1))
dat$Xn <- model.matrix(~X,dat)[,2]

fix      <- c(200,10)
sd_Subj  <- c(30, 10)
sd_Item  <- c(10)
sd_Res   <- 50
dat$ysim <- simLMM(~ X + (X | Subj) + (1 | Item), data=dat, Fixef=fix, VC_sd=list(sd_Subj, sd_Item, sd_Res), CP=0.3, empirical=TRUE)
dat %>% group_by(X) %>% summarize(M=mean(ysim))
lmer(ysim ~ Xn + (Xn || Subj) + (1 | Item), data=dat, control=lmerControl(calc.derivs=FALSE))

## ---- eval=FALSE, message=FALSE, cache=TRUE-----------------------------------
#  nsubj <- seq(10,100,1)
#  nsim  <- length(nsubj)
#  COF   <- data.frame()
#  for (i in 1:nsim) {
#  	#print(paste0(i,"/",nsim))
#  	design <-
#  	  fixed.factor("X", levels=c("X1", "X2")) +
#  	  random.factor("Subj", instances=nsubj[i]) +
#  	  random.factor("Item", groups="X", instances=4)
#  	dat <- design.codes(design)
#  	nsj <- length(unique(dat$Subj))
#  	contrasts(dat$X) <- c(-1, +1)
#  	dat$Xn <- model.matrix(~X,dat)[,2]
#  	for (j in 1:18) {
#  	  # assume three different effect sizes for the fixed effect: 5, 10, or 15
#  		FIX <- c(5,10,15)
#  		fix <- c(200,FIX[(j%%3)+1])
#  		dat$ysim <- simLMM(~ X + (X | Subj) + (1 | Item), data=dat, Fixef=fix, VC_sd=list(sd_Subj, sd_Item, sd_Res), CP=0.3, empirical=FALSE, verbose=FALSE)
#  		LMM <- lmer(ysim ~ Xn + (Xn || Subj) + (1 | Item), data=dat, control=lmerControl(calc.derivs=FALSE))
#  		LMMcof <- coef(summary(LMM))[2,]
#  		COF <- rbind(COF,c(nsj,fix[2],LMMcof)) # store effect size
#  	}
#  }
#  # results for LMM
#  names(COF) <- c("nsj","EffectSize","Estimate","SE","df","t","p")
#  COF$sign <- as.numeric(COF$p < .05 & COF$Estimate>0)
#  COF$nsjF  <- gtools::quantcut(COF$nsj, q=seq(0,1,length=10))
#  COF$nsjFL <- plyr::ddply(COF,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
#  #save(COF, file="data/Power3.rda")

## ---- message=FALSE-----------------------------------------------------------
load(file=system.file("power-analyses", "Power3.rda", package = "designr"))
ggplot(data=COF) +
	geom_smooth(aes(x=nsj, y=sign, colour=factor(EffectSize)))+
	geom_point(stat="summary", aes(x=nsjFL, y=sign, colour=factor(EffectSize)))+
	geom_errorbar(stat="summary", aes(x=nsjFL, y=sign, colour=factor(EffectSize)))+
	geom_line(stat="summary", aes(x=nsjFL, y=sign, colour=factor(EffectSize)))

## -----------------------------------------------------------------------------
m0 <- loess(sign ~ nsj, data=subset(COF,EffectSize==10))
COF$pred <- NA
COF$pred[COF$EffectSize==10] <- predict(m0)
idx <- COF$pred>0.5
min(COF$nsj[idx], na.rm=TRUE) # 70

## -----------------------------------------------------------------------------
# load empirical data
data(gibsonwu2013) # the Gibson & Wu (2013) dataset is included in the designr package
gw1 <- subset(gibsonwu2013, region=="headnoun") # subset critical region
gw1$so <- ifelse(gw1$type %in% c("subj-ext"),-1,1) # sum-coding for predictor
dat2 <- gw1[,c("subj","item","so","type2","rt")] # extract experimental design
datE <- dplyr::arrange(dat2, subj, item)
length(unique(datE$subj)) # 37
length(unique(datE$item)) # 15

# we re-create the empirical design from the experiment using designr
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("subj", instances=18) +
  random.factor("item", instances= 8) +
  random.factor(c("subj", "item"), groups=c("X"))
dat <- dplyr::arrange(design.codes(design), subj, item)
(contrasts(dat$X) <- c(+1, -1))
dat$so <- model.matrix(~X,dat)[,2]
length(unique(dat$subj)) # 36
length(unique(dat$item)) # 16

# compare designs
head(datE,20)
head(data.frame(dat),20)
tail(datE,20)
tail(data.frame(dat),20)

# transform dependent variable
hist(datE$rt)
boxcox(rt ~ so, data=datE)
datE$speed <- 1/datE$rt
hist(datE$speed)
qqnorm(datE$speed); qqline(datE$speed)

# run LMM on speed variable
lmm <- lmer(speed ~ so + (so|subj) + (so|item), data=datE, control=lmerControl(calc.derivs=FALSE))
summary(lmm)
# the random effects correlation for items is estimated as 1, indicating a singular fit
lmm <- lmer(speed ~ so + (so|subj) + (so||item), data=datE, control=lmerControl(calc.derivs=FALSE))
summary(lmm)

## -----------------------------------------------------------------------------
datE$simSpeed <- simLMM(LMM=lmm, empirical=TRUE)
dat$speed     <- simLMM(data=dat, LMM=lmm, empirical=TRUE)
dat %>% group_by(so) %>% summarize(M=mean(speed))
lmer(speed ~ so + (so||subj) + (so||item), data=dat, control=lmerControl(calc.derivs=FALSE))

## ---- eval=FALSE, cache=TRUE, message=FALSE, warnings=FALSE-------------------
#  # perform power analysis based on fitted model
#  nsubj <- seq(10,100,1)
#  nsim <- length(nsubj)
#  COF <- data.frame()
#  for (i in 1:nsim) { # i <- 1
#  	#print(paste0(i,"/",nsim))
#  	design <-
#  	  fixed.factor("X", levels=c("X1", "X2")) +
#  	  random.factor("subj", instances=nsubj[i]) +
#  	  random.factor("item", instances= 8) +
#  	  random.factor(c("subj", "item"), groups=c("X"))
#  	dat <- dplyr::arrange(design.codes(design), subj, item)
#  	contrasts(dat$X) <- c(+1, -1)
#  	dat$so <- model.matrix(~X,dat)[,2]
#  	nsj <- length(unique(dat$subj))
#  	for (j in 1:10) { # j <- 1
#  		dat$speed <- simLMM(data=dat, LMM=lmm, empirical=FALSE, verbose=FALSE)
#  		LMMcof <- coef(summary(lmer(speed ~ so + (so||subj) + (so||item), data=dat, control=lmerControl(calc.derivs=FALSE))))[2,]
#  		COF <- rbind(COF,c(nsj,LMMcof))
#  	}
#  }
#  names(COF) <- c("nsj","Estimate","SE","df","t","p")
#  COF$sign <- as.numeric(COF$p < .05 & COF$Estimate>0)
#  COF$nsjF  <- gtools::quantcut(COF$nsj, q=seq(0,1,length=10))
#  COF$nsjFL <- plyr::ddply(COF,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
#  #save(COF, file="data/Power4.rda")

## ---- message=FALSE-----------------------------------------------------------
load(file=system.file("power-analyses", "Power4.rda", package = "designr"))
ggplot(data=COF) +
	geom_smooth(aes(x=nsj, y=sign))+
	geom_point(stat="summary", aes(x=nsjFL, y=sign))+
	geom_errorbar(stat="summary", aes(x=nsjFL, y=sign))+
	geom_line(stat="summary", aes(x=nsjFL, y=sign))

fixef(lmm)
mean(COF$Estimate)

# determine number of subjects needed for a power of 40%
m0 <- loess(sign ~ nsj, data=COF)
COF$pred <- predict(m0)
idx <- COF$pred>0.4
min(COF$nsj[idx]) # 164

## -----------------------------------------------------------------------------
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=15) +
  random.factor("Item", groups="X", instances=10)
dat <- design.codes(design)
(contrasts(dat$X) <- c(-1, +1))
dat$Xn <- model.matrix(~X,dat)[,2]

## -----------------------------------------------------------------------------
# simulate covariate
dat$age <- round( simLMM(form=~ 1 + (1 | Subj), data=dat,
                                   Fixef=30, VC_sd=list(5),
                                   empirical=TRUE, family="lp") )
#table(dat$Subj,dat$age)
hist(dat$age)
dat$wordFrequency <- round( simLMM(form=~ 1 + (1 | Item), data=dat,
                            Fixef=4, VC_sd=list(1),
                            empirical=TRUE, family="lp") )
#table(dat$Item,dat$wordFrequency)
hist(dat$wordFrequency)
fix     <- c(5) # set fixed effects
sd_Subj <- c(1) # set random effects standard deviations for subjects
sd_Item <- c(1) # set random effects standard deviations for items
sd_Res  <- 1    # set residual standard deviation
dat$rating <- round( simLMM(form=~ 1 + (1 | Subj) + (1 | Item), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj, sd_Item, sd_Res),
                   empirical=TRUE) )
hist(dat$rating)
dat$rating.c <- dat$rating - mean(dat$rating)

## -----------------------------------------------------------------------------
# simulate data
fix     <- c(200,20, 7, 5) # set fixed effects
sd_Subj <- c(30, 10, 5, 5) # set random effects standard deviations for subjects
sd_Item <- c(10)     # set random effects standard deviations for items
sd_Res  <- 50        # set residual standard deviation
dat$ysim <- simLMM(form=~ X*rating.c + (X*rating.c | Subj) + (1 | Item), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj, sd_Item, sd_Res), CP=0.3,
                   empirical=FALSE)
# check group means
dat %>% group_by(X) %>% summarize(M=mean(ysim))
# check fixed effects + random effects
summary(lmer(ysim ~ Xn*rating.c + (Xn*rating.c || Subj) + (1 | Item), data=dat, control=lmerControl(calc.derivs=FALSE)))

## ---- eval=FALSE, cache=TRUE, warning=FALSE-----------------------------------
#  nsubj <- seq(10,100,1)
#  nsim <- length(nsubj)
#  COF <- data.frame()
#  for (i in 1:nsim) { # i <- 1
#    #print(paste0(i,"/",nsim))
#    design <-
#      fixed.factor("X", levels=c("X1", "X2")) +
#      random.factor("Subj", instances=nsubj[i]) +
#      random.factor("Item", groups="X", instances=2)
#    dat <- design.codes(design)
#    nsj <- length(unique(dat$Subj))
#    contrasts(dat$X) <- c(-1, +1)
#    dat$Xn <- model.matrix(~X,dat)[,2]
#    for (j in 1:10) { # j <- 1
#      dat$rating <- round( simLMM(form=~ 1 + (1 | Subj) + (1 | Item), data=dat,
#                                  Fixef=5, VC_sd=list(1, 1, 1), empirical=FALSE, verbose=FALSE) )
#      dat$rating.c <- dat$rating - mean(dat$rating)
#  
#      fix     <- c(200,20, 7, 5) # set fixed effects
#      sd_Subj <- c(30, 10, 5, 5) # set random effects standard deviations for subjects
#      sd_Item <- c(10)     # set random effects standard deviations for items
#      sd_Res  <- 50        # set residual standard deviation
#      dat$ysim <- simLMM(form=~ X*rating.c + (X*rating.c | Subj) + (1 | Item), data=dat,
#                         Fixef=fix, VC_sd=list(sd_Subj, sd_Item, sd_Res), CP=0.3,
#                         empirical=FALSE, verbose=FALSE)
#      LMM <- lmer(ysim ~ Xn*rating.c + (Xn*rating.c || Subj) + (1 | Item), data=dat,
#                                  control=lmerControl(calc.derivs=FALSE))
#      LMMcof <- coef(summary(LMM))[4,] # extract the stats for the interaction
#      COF <- rbind(COF,c(nsj,LMMcof))
#    }
#  }
#  names(COF) <- c("nsj","Estimate","SE","df","t","p")
#  COF$sign <- as.numeric(COF$p < .05 & COF$Estimate>0)
#  COF$nsjF  <- gtools::quantcut(COF$nsj, q=seq(0,1,length=10))
#  COF$nsjFL <- plyr::ddply(COF,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
#  #save(COF, file="data/Power5.rda")

## ---- message=FALSE-----------------------------------------------------------
load(file=system.file("power-analyses", "Power5.rda", package = "designr"))
ggplot(data=COF) +
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsjFL, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsjFL, y=sign))+
  geom_line(    stat="summary", aes(x=nsjFL, y=sign))

# determine number of subjects needed for a power of 50%
m0 <- loess(sign ~ nsj, data=COF)
COF$pred <- predict(m0)
idx <- COF$pred>0.5
min(COF$nsj[idx]) # 84

## -----------------------------------------------------------------------------
design <-
  fixed.factor("X", levels=c("X1", "X2")) +
  random.factor("Subj", instances=50) +
  random.factor("Item", groups="X", instances=10)
dat <- dplyr::arrange(design.codes(design), Subj, Item)[c(3, 1, 2)]
(contrasts(dat$X) <- c(-1, +1))
dat$Xn <- model.matrix(~X,dat)[,2]

fix      <- c(0.5,2) # specify fixed-effects
sd_Subj  <- c(3  ,1) # specify subject random effects (standard deviation)
sd_Item  <- c(1)     # specify item random effects (standard deviation)
dat$ysim <- simLMM(form=~ X + (X | Subj) + (1 | Item), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj, sd_Item), CP=0.3,
                   empirical=FALSE, family="binomial")
dat %>% group_by(X) %>% summarize(M=mean(ysim))
glmer(ysim ~ X + (X | Subj) + (1 | Item), data=dat, family="binomial", control=lmerControl(calc.derivs=FALSE))


## -----------------------------------------------------------------------------
# provide the linear predictor
# i.e., predictions of the LMM before passing through the link function / probability density function (PDF)
# this allows using custom links / PDFs
dat$lp <- simLMM(form=~ X + (X | Subj) + (1 | Item), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj, sd_Item), CP=0.3,
                   empirical=FALSE, family="lp")

