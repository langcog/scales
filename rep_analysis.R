############################################################
## ANALYSIS OF SCALES DATA 
## only replication dataset

rm(list=ls())
library(xtable)
source("~/Projects/R/Ranalysis/useful.R")
d <- read.csv("data/scales_rep_data.csv")

##### initial stuff
# get filler data
sum(subset(d,trial.type=="Filler" & study_name=="SCALES_REP")$correct)
# 141
length(subset(d,trial.type=="Filler" & study_name=="SCALES_REP")$correct)
# 144

######
# clean up replication
d <- d[,c("SID","age","age.grp","study_name","trial","correct","logical","foil")]
names(d) <- c("id","age","age.grp","condition","trial","correct","logical","foil")
levels(d$condition) <- c("Label","No Label")
d <- subset(d,trial != "Cars" & trial != "Fruits")
d$trial <- factor(d$trial)
levels(d$trial) <- tolower(levels(d$trial))


##### merge in demographics
demos <- read.csv("data/scales_rep_demographics.csv")

## exclusion info
summary(demos$Inclusion)

d <- merge(d,demos[,c("SID","ethnicity","english","gender","hispanic","parent_ed")],by.x="id",by.y="SID",all.x=TRUE, all.y=FALSE)

##### descriptives (check)
d$age.grp.year <- floor(d$age.grp)

## ages 
subs <- aggregate(correct ~ id + age.grp.year + age + condition, data=d, mean)
descs <- aggregate(age ~ age.grp.year + condition,data=subs,mean)
descs$n <- aggregate(id ~ age.grp.year + condition,data=subs,n.unique)$id


###### MODELS FOR OTHER VARIABLES

# baseline model
summary(glmer(correct ~ age * condition + (1 | id) + (1 | trial), 
              family="binomial",data=d))

# gender effect
summary(glmer(correct ~ age * condition * (gender=="male")  + (1 | id) + (1 | trial), 
              family="binomial",data=d))

# english percentage, nothing here.
summary(glmer(correct ~ age * condition * english  + (1 | id) + (1 | trial), 
              family="binomial",data=d))

# english percentage main effect, also nothing
summary(glmer(correct ~ age * condition + english  + (1 | id) + (1 | trial), 
              family="binomial",data=d))

# parent ed, nothing in the interaction
summary(glmer(correct ~ age * condition * as.numeric(parent_ed)  + (1 | id) + (1 | trial), 
              family="binomial",data=d))

# parent ed, main effect
summary(glmer(correct ~ age * condition + as.numeric(parent_ed)  + (1 | id) + (1 | trial), 
              family="binomial",data=d))



##### RESPONSE ANALYSIS #####
mss <- aggregate(cbind(correct,logical,foil) ~ id + condition + age.grp,
                data=d,mean)
msd <- melt(mss,id.vars=c("id","condition","age.grp"),
            measure.vars=c("correct","logical","foil"))

ms <- aggregate(value ~ variable + age.grp + condition, data=msd, mean)
ms$cih <- aggregate(value ~ variable + age.grp + condition, data=msd, ci.high)$value
ms$cil <- aggregate(value ~ variable + age.grp + condition, data=msd, ci.low)$value

# quartz()
qplot(age.grp, value,group=variable,
      ymax=value+cih, ymin=value-cil,
      position=position_dodge(.1),
      geom=c("pointrange","line"),colour=variable,
      data=ms) + 
  facet_grid(.~condition) +
  geom_hline(yintercept=.5,lty=2) + 
  ylim(c(0,1)) + 
  xlab("Age (Years)") +
  ylab("Proportion Choices") + 
  scale_colour_brewer(palette="Greys")

## item response model
summary(glmer(correct ~ factor(age.grp) + (1 | id) + (1 | trial), 
              family="binomial",data=subset(d,foil==FALSE & condition=="Label")))

summary(glmer(correct ~ factor(age.grp) + (1 | id) + (1 | trial), 
              family="binomial",data=subset(d,foil==FALSE & condition=="No Label")))
