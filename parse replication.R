rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R")
d <- read.csv("data/scales_rep_data_raw.csv")

## get ages
d$age <- as.numeric(difftime(as.Date(d$DOT,format="%m/%d/%Y"),
                  as.Date(d$DOB,format="%m/%d/%Y"))/365)
d$age.grp <- floor(d$age*2)/2

## remove young one and correct 5yo to be in age grp 4s
d <- subset(d,age > 2)
d$age.grp[d$age==5] <- 4


## melt and code data
md <- melt(d,id.vars=c("SID","age","age.grp","study_name"),
           measure.vars=c("Cars","Faces","Houses","Fruits","Pasta","Beds"),
           value.name="response",variable.name="trial")
          
md$trial.type <- revalue(md$trial,
                         c("Cars"="Filler",
                           "Faces"="Inference",
                           "Houses"="Inference",
                           "Fruits"="Filler",
                           "Pasta"="Inference",
                           "Beds"="Inference"))
md$correct <- md$response == "penguin" |
  md$response == "bear" |
  md$response == "sauce" |
  md$response == "meat" |
  md$response == "flower" |
  md$response == "tree" |
  md$response == "hat" |
  md$response == "glasses" |
  grepl("*/Y",md$response)

md$logical <- md$response == "+"
md$foil <- md$response == "-"

write.csv(md,"data/scales_rep_data.csv",row.names=FALSE)

