## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(designr)

# Set a "seed" for the random numnber generator
set.seed(12345)  

## ------------------------------------------------------------------------
design1 <- 
  fixed.factor("Speed", levels=c("slow", "medium", "fast")) +
  fixed.factor("Load",  levels=c("yes", "no")) +
  random.factor("Subj", instances=6) +    
  random.factor("Item", instances=6)    


codes1 <- arrange(design.codes(design1), Subj, Item)[c(3, 4, 2, 1)]
codes1
tail(codes1, 10)

#xtabs( ~ Subj + Item + Load + Speed, codes1)
xtabs(~ Load + Speed, codes1)
xtabs(~ Subj + Load + Speed, codes1)
xtabs(~ Item + Load + Speed, codes1)

## ------------------------------------------------------------------------
design2 <- fixed.factor("Speed", levels=c("slow", "medium", "fast")) +
  fixed.factor("Type",  levels=c("simple", "complex")) +
  random.factor("Subj", instances=6) +   
  random.factor("Item", groups="Type", instances=3)

codes2 <- arrange(design.codes(design2), Subj, Item)[c(3, 4, 1, 2)]
codes2

xtabs(~ Item + Type, codes2)
xtabs(~ Subj + Type, codes2)

#xtabs( ~ Subj + Item + Type + Speed, codes2)
#xtabs(~ Type + Speed, codes2)
#xtabs(~ Subj + Type + Speed, codes2)
#xtabs(~ Item + Type + Speed, codes2)

## ------------------------------------------------------------------------
design3 <- 
  fixed.factor("Speed", levels=c("slow", "medium", "fast")) +
  fixed.factor("Age",  levels=c("young", "old")) +
  random.factor("Item", instances=6) +
  random.factor("Subj", groups="Age", instances=3) 

codes3 <- arrange(design.codes(design3), Subj, Item)[c(4, 3, 2, 1)]
codes3

xtabs(~ Subj + Age, codes3)
xtabs(~ Item + Age, codes3)

#xtabs( ~ Subj + Item + Age + Speed, codes3)
#xtabs( ~ Subj + Age + Speed, codes3)

## ------------------------------------------------------------------------
design4 <- 
  fixed.factor("Speed", levels=c("slow", "medium", "fast")) +
  fixed.factor("Age",  levels=c("simple", "complex")) +
  random.factor("Subj", groups=c("Age", "Speed"), instances=10) +
  random.factor("Item", instances=6)   

codes4 <- arrange(design.codes(design4), Subj, Item)[c(3, 4, 2, 1)]
codes4

xtabs( ~ Subj + Age, codes4)
xtabs( ~ Subj + Speed, codes4)
xtabs( ~ Item + Age, codes4)
xtabs( ~ Item + Speed, codes4)

#xtabs( ~ Subj + Item + Age + Speed, codes4)

## ------------------------------------------------------------------------
design5 <- 
  fixed.factor("Speed", levels=c("slow", "medium", "fast")) +
  fixed.factor("Load",  levels=c("simple", "complex")) +
  random.factor("Subj", instances=1) + 
  random.factor("Item", instances=1) +
  random.factor(c("Subj", "Item"), groups=c("Speed", "Load"))


codes5 <- arrange(design.codes(design5), Subj, Item)[c(3, 4, 1, 2)]
codes5

xtabs(~ Subj + Speed + Load, codes5)
xtabs(~ Item + Speed + Load, codes5)

xtabs( ~ Subj + Item + Load + Speed, codes1)

## ------------------------------------------------------------------------
design6 <- 
  fixed.factor("Speed", levels=c("slow", "medium", "fast")) +
  fixed.factor("Load",  levels=c("simple", "complex")) +
  random.factor("Subj", instances=10) + 
  random.factor("Item", instances=4) +
  random.factor(c("Subj", "Item"), groups=c("Speed", "Load"))


codes6 <- arrange(design.codes(design6), Subj, Item)[c(3, 4, 1, 2)]
codes6
length(unique(codes6$Subj))
length(unique(codes6$Item))
length(unique(paste(codes6$Subj, codes6$Item)))

## ------------------------------------------------------------------------
sessionInfo()

