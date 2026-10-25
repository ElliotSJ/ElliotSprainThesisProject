#Loading Data
load("31622-0009-Data.rda")
load("31622-0001-Data.rda")
year0data <- da31622.0001
year15data <- da31622.0009
da31622.0009$K6F4
library(tidyverse)
library(dplyr)
library(ggplot2)
library(table1)

##########################################
##########################################
#Data cleaning and setup
##########################################
##########################################

#Descriptive Statistics (race, age, etc., refer to thesis document) Using ANOVA
year15data$CK6ETHRACE[year15data$CK6ETHRACE == "(-9) -9 Not in wave"] <- NA #Set not in wave to NA
year15data$CK6ETHRACE[year15data$CK6ETHRACE == "(-3) -3 Missing"] <- NA #Set missing to NA
year15data$CK6ETHRACE[year15data$CK6ETHRACE == "(-1) -1 Refuse"] <- NA #Set refuse to NA
demoethrace <- year15data$CK6ETHRACE
demoage <-year15data$CP6YAGEY
demoage[demoage == -9] <- NA
demoage[demoage == -3] <- NA
demoedu <- year15data$CP6EDU
demosex <- year0data$CM1BSEX


demodat <- cbind.data.frame(demoethrace, demoage, demoedu, demosex)

#Mother's Educational Attainment variable as a measure of SES. 
medu_cont <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$CP6EDU)) #This line replaces the entire string of CP6EDU with only the contents 1-9 after the ( and before the ). Negatives fail this check and are set to NA.
medu_cont_c <- medu_cont-mean(medu_cont, na.rm=TRUE) 
#Mothers education centered on 0. Positive means above mean education, negative means below mean education 
medu_categories <- c("(1) 1 less HS","(2) 2 HS or equiv", "(3) 3 Some coll, tech", "(4) 4 coll or grad")
medu_cat <- medu_categories[medu_cont] #Categorical form of medu_cont_c, with NAs for negatives but 

#Preparing to calculate income to needs
householdsize <- year15data$CP6HHSIZE
#Household size - number of people in the household
householdsize[householdsize<0] <-NA
householdincome <- year15data$P6K57
householdincome[householdincome<0] <-NA
#Household income - dollar amount of income for the household

#load("thresh17.csv") This csv file does not read properly. Will manually import applicable data from this table. Using weighted values from column B
familysize <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,18)
familythresh <- c(16493, 19515, 25094, 29714, 33618, 38173, 42684, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681) #From 2017 US Census data found https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
#Calculating income-to-needs as a measure of SES
itn <- householdincome/familythresh[householdsize]
itn_c <- itn-mean(itn, na.rm=TRUE)
#Ratio of income to poverty line income based on household size. Centered on 0. Positive values are above the mean, negative values are below the mean

##########################################
##########################################
#RQ1 DATA
##########################################
##########################################

#Relationship Involvement - RQ1 dependent variable
yr15involvement <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F7)) 
#Positive values only. 1 is yes, 2 is no. Currently dating at year 15
everinvolvement <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F4)) 
#Positive values only. 1 is yes, 2 is no. Has ever dated at year 15
reverseyr15involvement <- 2-yr15involvement 
#Reverse coded current involvement. 0 is no, 1 is yes
reverseeverinvolvement <- 2-everinvolvement 
#Reverse coded ever dated. 0 is no, 1 is yes

#Computing a composite measure/factor score for mother's education and income to needs
composite <- rowMeans (
  cbind( 
    scale(medu_cont_c), 
    scale(itn_c)
  ),
  na.rm=TRUE
)
#Composite is centered on 0. Negative values are below the mean, positive values are above the mean

rq1data <- cbind.data.frame(reverseyr15involvement, reverseeverinvolvement, composite)

##########################################
##########################################
#RQ2 DATA
##########################################
##########################################

#Intimate Partner Violence- RQ2 dependent variable
ipvqa <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18A))
ipvqb <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18B))
ipvqc <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18C))
ipvqd <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18D))

ipvscore <- ipvqa+ipvqb+ipvqc+ipvqd #Sum of IPV measure scores. Min of 4, max of 12. Min of 4 is highest IPV score, 12 is lowest
reverseipvscore <- 8-(ipvscore-4) 
#Number of IPV behaviors reported. A score of 8 is now the max IPV score. 0 is the min


relationshipquality <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F14))
reverserelationshipquality <- 6-relationshipquality
#Self reported relationship quality from 1-5. 1 is the lowest, 5 is the highest

rq2data <- cbind.data.frame(composite, reverseipvscore, reverserelationshipquality)

##########################################
##########################################
#RQ3 DATA
##########################################
##########################################

pt_together_num <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$CP6PRELB))
togetherstatuslist <- c(1,1,0,0,0,0,0,0)
parenttogether <- togetherstatuslist[pt_together_num]
#Binary variable on parents being together. 1 is together, 0 is not

rq3data <- cbind.data.frame(composite, reverseyr15involvement, reverseeverinvolvement, reverseipvscore, reverserelationshipquality, demoethrace, pt_together_num, parenttogether, demosex)
rq3boys <- subset(rq3data, rq3data$demosex == "(1) 1 boy")
rq3girls <- subset(rq3data, rq3data$demosex == "(2) 2 girl")

##########################################
##########################################
#COVARIATES DATA
##########################################
##########################################
covardata <- cbind.data.frame(composite, yr15involvement, everinvolvement, reverseipvscore, reverserelationshipquality, pt_together_num, parenttogether, demoethrace, demoage, demosex)


#Race categories asfor covariate comparisons. Black as the reference group
covardata$whitecompare <- 0
covardata$whitecompare[covardata$demoethrace == "(1) 1 White only, non-hispanic"] <- 1 #Set not in wave to NA
covardata$whitecompare[is.na(covardata$demoethrace)] <- NA 

covardata$hisplatcompare <- 0
covardata$hisplatcompare[covardata$demoethrace == "(3) 3 Hispanic/Latino"] <- 1 
covardata$hisplatcompare[is.na(covardata$demoethrace)] <- NA 

covardata$othercompare <- 0
covardata$othercompare[covardata$demoethrace == "(4) 4 Other only, non-hispanic"] <- 1
covardata$othercompare[covardata$demoethrace == "(-2) -2 Don't know"] <- 1
covardata$othercompare[is.na(covardata$demoethrace)] <- NA 

covardata$multicompare <- 0
covardata$multicompare[covardata$demoethrace == "(5) 5 Multi-racial, non-hispanic"] <- 1
covardata$multicompare[is.na(covardata$demoethrace)] <- NA 

##########################################
##########################################
#Correlations between independent variables in the composite
##########################################
##########################################


medu_itn_cat <-lm(formula = itn_c ~ medu_cat) 
medu_itn_cont <-lm(formula = itn_c ~ medu_cont_c) 
summary(medu_itn_cont)
comp_medu <-lm(formula = composite ~ medu_cont) 
comp_itn <-lm(formula = composite ~ itn_c) 

summary(comp_medu)
summary(comp_itn)



##########################################
##########################################
#Research Question 3 - How does parent’s marital status moderate these relationships?
##########################################
##########################################

#Moderator regressions

comp_current_cohab <- lm(formula = reverseyr15involvement ~ composite*parenttogether, data = rq3girls)
comp_ever_cohab <- lm(formula = reverseeverinvolvement ~ composite*parenttogether, data = rq3girls)

comp_ipv_cohab <- lm(formula = reverseipvscore ~ composite*parenttogether, data = rq3girls)
comp_qual_cohab <-lm(formula = reverserelationshipquality ~ composite*parenttogether, data = rq3girls)


summary(comp_current_cohab)
summary(comp_ever_cohab)
summary(comp_ipv_cohab)
summary(comp_qual_cohab)

comp_current_cohab_cov <- lm(formula = reverseyr15involvement ~ composite*parenttogether + whitecompare + hisplatcompare + othercompare + multicompare + demosex + demoage, data = covardata)
comp_ever_cohab_cov <- lm(formula = reverseeverinvolvement ~ composite*parenttogether + whitecompare + hisplatcompare + othercompare + multicompare + demosex + demoage, data = covardata)

comp_ipv_cohab_cov <- lm(formula = reverseipvscore ~ composite*parenttogether + whitecompare + hisplatcompare + othercompare + multicompare + demosex + demoage, data = covardata)
comp_qual_cohab_cov <-lm(formula = reverserelationshipquality ~ composite*parenttogether + whitecompare + hisplatcompare + othercompare + multicompare + demosex + demoage, data = covardata)

summary(comp_current_cohab_cov)
summary(comp_ever_cohab_cov)
summary(comp_ipv_cohab_cov)
summary(comp_qual_cohab_cov)

#Supplemental analysis
current_cohab_gender <- lm(formula = reverseyr15involvement ~ parenttogether*demosex, data = rq3data)
ever_cohab_gender <- lm(formula = reverseeverinvolvement ~ parenttogether*demosex, data = rq3data)
summary(current_cohab_gender)
summary(ever_cohab_gender)
