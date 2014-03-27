############################################################
## ANALYSIS OF SCALES DATA 
## both original and replication datasets

rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R")
d.rep <- read.csv("~/Projects/R/scales/data/scales_rep.csv")
d.orig <- read.csv("~/Projects/R/scales/data/scales_orig.csv")

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

###### ANALYSIS ######

## aggregate and plot both experiments separately
mss <- aggregate(correct ~ id + age.grp + condition + expt,data=d,mean)
ms <- aggregate(correct ~ age.grp + condition + expt,data=mss,mean)
ms$cih <- aggregate(correct ~ age.grp + condition + expt,data=mss,ci.high)$correct
ms$cil <- aggregate(correct ~ age.grp + condition + expt,data=mss,ci.low)$correct
ms$n <- aggregate(id ~ age.grp + condition + expt,data=mss,n.unique)$id

quartz()
qplot(age.grp, correct, ymax=correct+cih,ymin=correct-cil,
      colour=condition,lty=expt,
      position=position_dodge(width=.1),
      geom=c("line","pointrange"),
      data=ms) + 
  geom_hline(yintercept=.5,lty=2)


## aggregate and plot averaged across experiments
mss <- aggregate(correct ~ id + age.grp + condition,data=d,mean)
ms <- aggregate(correct ~ age.grp + condition,data=mss,mean)
ms$cih <- aggregate(correct ~ age.grp + condition,data=mss,ci.high)$correct
ms$cil <- aggregate(correct ~ age.grp + condition,data=mss,ci.low)$correct

quartz()
qplot(age.grp, correct, ymax=correct+cih,ymin=correct-cil,
      colour=condition,
      position=position_dodge(width=.1),
      geom=c("line","pointrange"),
      data=ms) + 
  geom_hline(yintercept=.5,lty=2)


## now look at response types, only for experiment 2
ms <- aggregate(cbind(correct,logical,foil) ~ condition + age.grp,
                data=subset(d,expt=="replication"),mean)
msd <- melt(ms,id.vars=c("condition","age.grp"),
            measure.vars=c("correct","logical","foil"))

# quartz()
qplot(age.grp, value,group=variable,
      geom="line",colour=variable,
      data=msd) + 
  facet_grid(.~condition) +
  geom_hline(yintercept=.5,lty=2) + 
  ylim(c(0,1)) + 
  ylab("proportion responses")


###### models ######
summary(glmer(correct ~ age.grp * condition + (1 | id) + (1 | trial), 
           family="binomial",data=d))

# experiment effect
summary(glmer(correct ~ age.grp * condition * expt + (1 | id) + (1 | trial), 
              family="binomial",data=d))



## descriptives
subs <- aggregate(correct ~ id + age.grp + age + condition + expt, data=d, mean)
subs$age.grp.year <- floor(subs$age.grp)
descs <- aggregate(age ~ age.grp.year + condition + expt,data=subs,mean)
