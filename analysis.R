############################################################
## ANALYSIS OF SCALES DATA 
## both original and replication datasets

rm(list=ls())
library(xtable)
source("~/Projects/R/Ranalysis/useful.R")
d.rep <- read.csv("data/scales_rep_data.csv")
d.orig <- read.csv("data/scales_orig_data.csv")


######
# merge the datasets together, do some extra cleanup
# clean up replication
d.rep <- d.rep[,c("SID","age","age.grp","study_name","trial","correct","logical","foil")]
names(d.rep) <- c("id","age","age.grp","condition","trial","correct","logical","foil")
levels(d.rep$condition) <- c("Label","No Label")
d.rep$expt <- "replication"
d.rep <- subset(d.rep,trial != "Cars" & trial != "Fruits")
d.rep$trial <- factor(d.rep$trial)
levels(d.rep$trial) <- tolower(levels(d.rep$trial))

# clean up original
d.orig <- d.orig[,c("id","age","age.grp","condition","trial","correct")]
d.orig$correct <- d.orig$correct == 1
d.orig$age.grp <- floor(d.orig$age * 2) / 2
d.orig$expt <- "original"

# create final 
d <- rbind.fill(d.rep,d.orig)

###### DESCRIPIVES ######
d$age.grp.year <- floor(d$age.grp)

## ages 
subs <- aggregate(correct ~ id + age.grp + age + condition + expt, data=d, mean)
descs <- aggregate(age ~ age.grp.year + condition + expt,data=subs,mean)
descs$n <- aggregate(id ~ age.grp.year + condition + expt,data=subs,n.unique)$id

## means
mss <- aggregate(correct ~ id + age.grp + condition,data=d,mean)
ms <- aggregate(correct ~ age.grp + condition,data=mss,mean)
ms$sd <- aggregate(correct ~ age.grp + condition,data=mss,sd)$correct
ms$n <- aggregate(id ~ age.grp + condition,data=mss,n.unique)$id
xtable(ms)

# now add confidence intervals later
ms$cih <- aggregate(correct ~ age.grp + condition,data=mss,ci.high)$correct
ms$cil <- aggregate(correct ~ age.grp + condition,data=mss,ci.low)$correct

###### ANALYSIS ######

## first plot the above
quartz()
qplot(age.grp, correct, ymax=correct+cih,ymin=correct-cil,
      colour=condition,
      position=position_dodge(width=.1),
      geom=c("line","pointrange"),
      data=ms) +
  ylim(c(0,1)) +
  ylab("Proportion One-Feature Choices") + 
  xlab("Age (Years)") + 
  scale_colour_manual(values=gray(c(.65,.35)))


## aggregate and plot both experiments separately
mss <- aggregate(correct ~ id + age.grp + condition + expt,data=d,mean)
ms <- aggregate(correct ~ age.grp + condition + expt,data=mss,mean)
ms$cih <- aggregate(correct ~ age.grp + condition + expt,data=mss,ci.high)$correct
ms$cil <- aggregate(correct ~ age.grp + condition + expt,data=mss,ci.low)$correct

quartz()
qplot(age.grp, correct, ymax=correct+cih,ymin=correct-cil,
      colour=condition,
      position=position_dodge(width=.1),
      geom=c("line","pointrange"),
      data=ms) +
  ylim(c(0,1)) +
  ylab("Proportion One-Feature Choices") + 
  xlab("Age (Years)") + 
  scale_colour_manual(values=gray(c(.65,.35)))


## by items
mss <- aggregate(correct ~ id + age.grp + condition + trial,data=d,mean)
ms <- aggregate(correct ~ age.grp + condition + trial,data=mss,mean)
ms$cih <- aggregate(correct ~ age.grp + condition + trial,data=mss,ci.high)$correct
ms$cil <- aggregate(correct ~ age.grp + condition + trial,data=mss,ci.low)$correct

qplot(age.grp, correct, ymax=correct+cih,ymin=correct-cil,
      colour=condition,
      position=position_dodge(width=.1),
      geom=c("line","pointrange"),
      data=ms) + 
  facet_wrap(~trial) + 
  geom_hline(yintercept=.5,lty=2)

###### models ######
mod1 <- glmer(correct ~ age * condition + (1 | id) + (1 | trial), 
              family="binomial",data=d)
summary(mod1)
ranef(mod1)


# experiment effect
summary(glmer(correct ~ age * condition * expt + (1 | id) + (1 | trial), 
              family="binomial",data=d))






