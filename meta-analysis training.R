
###### Written for training purposes #####
#### Dr. Seckin Arslan ###################
#### University of Groningen #############
#### seckin1984@gmail.com ################



#working dir
# setwd("   ....  ")
# setwd("~/Desktop/Academic Work/UCA Course 2021/07 Meta-analysis")

# install.packages(c("lme4","multcomp","reshape","plyr","dplyr",
#                    "tidyverse","ggplot2","gridExtra","forestplot","lmerTest"))

library(lme4)
library(multcomp)
library(reshape)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(forestplot)
library(lmerTest) 
library(ggstatsplot)
library(ggplot2)
library(metafor)
library(tidyr)
library(patchwork)
library(data.table)
library(remotes)
library(likert)
library(summarytools) 
library(patchwork)





## Quick analysis with BCG data ####

data("dat.bcg", package = "metafor")
print(dat.bcg, row.names = FALSE)



dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos,
              di = cneg, data = dat.bcg, append = TRUE)

print(dat[,-c(4:7)], row.names = FALSE)






###########  OVERALL FUNNEL PLOT and MODEL #######
##################################################

# "PR": The raw proportion is equal to xi/ni.
# "PLO": The logit transformed proportion is equal to the log of xi/(ni - xi).

# This is the pronoun impairment data in aphasia (Arslan et al, under review)
#get data here 


funnel.data <- read.csv("funnel.data.csv", header = TRUE, sep = ",")


#calculate logit tansformed proportions 
fun.data <- escalc(xi = xi, ni = ni,
                   data = funnel.data, measure = "PLO", append = TRUE)





#random effects models

res1 <- rma(yi, vi, data=fun.data)

summary(res1)





# draw funnel plots 

pl1 <-  funnel(res1)





#######################################################
####  ANALYSIS OF OBJECT VS REFLEXIVE   ####


#read data 
reflex_data <- read.csv("reflex_training.csv", header = TRUE, sep = ",")


# calculate effect sizes with escal function (metafor package)

tfun.data <- escalc(m1i = Ref_ACC, m2i = Pro_ACC, sd1i= Ref_sd, sd2i = Pro_sd, n1i = Ref_n, n2i = Pro_n,
                    data = reflex_data, measure = "SMD", append = TRUE)



#random effects model
res <- rma(yi, vi, data=tfun.data)
summary(res)



#forest plot

forest(res, slab = paste(tfun.data$Authors, sep = ", "))




