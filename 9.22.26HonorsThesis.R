#Loading Data
load("31622-0009-Data.rda")
load("31622-0001-Data.rda")
year0data <- da31622.0001
year15data <- da31622.0009
da31622.0009$K6F4
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)

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
medu_categories <- c("(1) 1 less HS","(2) 2 HS or equiv", "(3) 3 Some coll, tech", "(4) 4 coll or grad")
medu_cat <- medu_categories[medu_cont] #Categorical form of medu_cont_c, with NAs for negatives but 

#Preparing to calculate income to needs
householdsize <- year15data$CP6HHSIZE
householdsize[householdsize<0] <-NA
householdincome <- year15data$P6K57
householdincome[householdincome<0] <-NA

#load("thresh17.csv") This csv file does not read properly. Will manually import applicable data from this table. Using weighted values from column B
familysize <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,18)
familythresh <- c(16493, 19515, 25094, 29714, 33618, 38173, 42684, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681) #From 2017 US Census data found https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
#Calculating income-to-needs as a measure of SES
itn <- householdincome/familythresh[householdsize]
itn_c <- itn-mean(itn, na.rm=TRUE)

##########################################
##########################################
#RQ1 DATA
##########################################
##########################################

#Relationship Involvement - RQ1 dependent variable
yr15involvement <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F7)) #Positive values only. 1 is yes, 2 is no. Currently dating at year 15
everinvolvement <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F4)) #Positive values only. 1 is yes, 2 is no. Has ever dated at year 15
reverseyr15involvement <- 2-yr15involvement #Reverse coded current involvement. 0 is no, 1 is yes
reverseeverinvolvement <- 2-everinvolvement #Reverse coded ever dating. 0 is no, 1 is yes

#Computing a composite measure/factor score for mother's education and income to needs
composite <- rowMeans (
  cbind( 
    scale(medu_cont_c), 
    scale(itn_c)
  ),
  na.rm=TRUE
)

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
reverseipvscore <- 8-(ipvscore-4) #Adapted scores for reverse coding. A score of 8 is now the highest IPV score

relationshipquality <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F14))
reverserelationshipquality <- 6-relationshipquality

rq2data <- cbind.data.frame(composite, reverseipvscore, reverserelationshipquality)

##########################################
##########################################
#RQ3 DATA
##########################################
##########################################

pt_together_num <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$CP6PRELB))
togetherstatuslist <- c(1,1,0,0,0,0,0,0)
parenttogether <- togetherstatuslist[pt_together_num]

rq3data <- cbind.data.frame(composite, reverseyr15involvement, reverseeverinvolvement, reverseipvscore, reverserelationshipquality, demoethrace, pt_together_num, parenttogether)

##########################################
##########################################
#COVARIATES DATA
##########################################
##########################################
covardata <- cbind.data.frame(composite, yr15involvement, everinvolvement, reverseipvscore, reverserelationshipquality, pt_together_num, parenttogether, demoethrace, demoage, demosex)


#Race categories asfor covariate comparisons. Black as the reference group
covardata$whitecompare <- 0
covardata$whitecompare[covardata$demoethrace == "(1) 1 White only, non-hispanic"] <- 1 #Set not in wave to NA

covardata$hisplatcompare <- 0
covardata$hisplatcompare[covardata$demoethrace == "(3) 3 Hispanic/Latino"] <- 1 

covardata$othercompare <- 0
covardata$othercompare[covardata$demoethrace == "(4) 4 Other only, non-hispanic"] <- 1
covardata$othercompare[covardata$demoethrace == "(-2) -2 Don't know"] <- 1

covardata$multicompare <- 0
covardata$multicompare[covardata$demoethrace == "(5) 5 Multi-racial, non-hispanic"] <- 1

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
#Research Question 1 -  How does SES predict intimate relationship involvement in adolescents?
##########################################
##########################################

#Regression on data
comp_current <- lm(formula = yr15involvement ~ composite, data = rq1data)
comp_ever <- lm(formula = everinvolvement ~ composite, data = rq1data)

summary(comp_current)
summary(comp_ever)

comp_current_cov <- lm(formula = yr15involvement ~ composite + whitecompare + hisplatcompare + othercompare + multicompare + demosex + demoage, data = covardata)
comp_ever_cov <- lm(formula = everinvolvement ~ composite + whitecompare + hisplatcompare + othercompare + multicompare + demosex + demoage, data = covardata)
summary(comp_current_cov)



##########################################
##########################################
#Research Question 2 - Among those who are in intimate relationships, is SES predictive of intimate relationship violence and relationship satisfaction?
##########################################
##########################################


#Regression on data
comp_ipv <- lm(formula = reverseipvscore ~ composite, data = rq2data)
comp_qual <- lm(formula = reverserelationshipquality ~ composite, data = rq2data)
ipv_qual <- lm(formula = reverseipvscore ~ reverserelationshipquality, data = rq2data)


summary(comp_ipv)
summary(comp_qual)
summary(ipv_qual) #Intimate partner violence association with relationship quality


##########################################
##########################################
#Research Question 3 - How does parent’s marital status moderate these relationships?
##########################################
##########################################

#Moderator regressions

comp_current_cohab <- lm(formula = yr15involvement ~ composite*parenttogether, data = rq3data)
comp_ever_cohab <- lm(formula = everinvolvement ~ composite*parenttogether, data = rq3data)

comp_ipv_cohab <- lm(formula = reverseipvscore ~ composite*parenttogether, data = rq3data)
comp_qual_cohab <-lm(formula = relationshipquality ~ composite*parenttogether, data = rq3data)


summary(comp_current_cohab)
summary(comp_ever_cohab)
summary(comp_ipv_cohab)
summary(comp_qual_cohab)




#Simple Slope Analysis 
xx <- c(-0.0233,0.0233)   #  <-- change to alter plot dims
yy <- c(1.1632,1.3239)   #  <-- change to alter plot dims
leg <- c(-0.0233,1.1833)   #  <-- change to alter legend location
x <- c(-0.0233,0.0233)   #  <-- x-coords for lines
y1 <- c(1.19,1.1908)
y2 <- c(1.3214,1.3239)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='X',ylab='Y',main='MLR 2-Way Interaction Plot')
lines(x,y1,lwd=3,lty=1,col=1)
lines(x,y2,lwd=3,lty=5,col=2)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
legend(leg[1],leg[2],legend=c('CVz1(1)','CVz1(2)'),lwd=c(3,3),lty=c(1,5),col=c(1,2))




#Graphs
ggplot(data=rq3data, aes(medu_cont, yr15involvement)) +
  geom_point() +
  geom_smooth(method="lm")
ggplot(data=rq3data, aes(medu_cont, everinvolvement)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Mother's Education Level", y="Ever involved in a relationship", title="Relationship between mother's education level and past relationship involvement")


#Data loading checks
#head(medu_cont_c)
#head(householdsize)
#head(householdincome)
#length(familythresh)
#length(familysize)
#head(itn_c)
#head(cbind.data.frame(householdincome, householdsize, itn_c))
#head(everinvolvement)
#year15data$CP6EDU



#Old Code
#m_current_cont <- lm(formula = yr15involvement ~ medu_cont_c, data = rq1data)
#itn_current <- lm(formula = yr15involvement ~ itn_c, data = rq1data)
#m_ever_cont <- lm(formula = everinvolvement ~ medu_cont_c, data = rq1data)
#itn_ever <- lm(formula = everinvolvement ~ itn_c, data = rq1data)

#Mother's education attainment categorical regressions
#m_current_cat <- lm(formula = yr15involvement ~ medu_cat, data = rq1data)
#m_ever_cat <- lm(formula = everinvolvement ~ medu_cat, data = rq1data) #Triple check all new variables. Negative values converted to NA


#Summaries of Regressions
#summary(m_current_cont) #Positive correlation means LESS relationship involvement
#summary(itn_current) #Positive correlation means LESS relationship involvement
#summary(m_ever_cont) #Positive correlation means LESS relationship involvement
#summary(itn_ever) #Positive correlation means LESS relationship involvement

#summary(m_current_cat) #Positive correlation means LESS relationship involvement
#summary(m_ever_cat) #Positive correlation means LESS relationship involvement

#RQ2

#m_ipv_cont <- lm(formula = reverseipvscore ~ medu_cont_c, data = rq2data)
#itn_ipv <- lm(formula = reverseipvscore ~ itn_c, data = rq2data)
#m_qual_cont <-lm(formula = reverserelationshipquality ~ medu_cont_c, data = rq2data)
#itn_qual <-lm(formula = reverserelationshipquality ~ itn_c, data = rq2data)
#ipv_qual <-lm(formula = reverserelationshipquality ~ reverseipvscore) #Analysis to replicate existing findings that IPV is not consistently associated with lower relationship quality

#Mother's education attainment categorical regressions
#m_ipv_cat <- lm(formula = reverseipvscore ~ medu_cat, data = rq2data)
#m_qual_cat <-lm(formula = reverserelationshipquality ~ medu_cat, data = rq2data)

#Summaries of Regressions
#summary(m_ipv_cont) #Mother's educational attainment association with intimate partner violence
#summary(itn_ipv) #Family income-to-needs association with intimate partner violence
#summary(m_qual_cont) #Mother's educational attainment association with relationship quality
#summary(itn_qual) #Family income-to-needs association with relationship quality. Positive correlation means HIGHER relationship quality



#summary(m_ipv_cat) #Mother's educational attainment (categorical) association with intimate partner violence
#summary(m_qual_cat)

#RQ3


#current_m_ethrace_cat <- lm(formula = yr15involvement ~ medu_cat*ethrace, data = rq3data)
#current_m_cohab_cat <- lm(formula = yr15involvement ~ medu_cat*parenttogether, data = rq3data)

#current_m_ethrace_cont <- lm(formula = yr15involvement ~ medu_cont_c*ethrace, data = rq3data)
#current_m_cohab_cont <- lm(formula = yr15involvement ~ medu_cont_c*parenttogether, data = rq3data)

#current_itn_ethrace <- lm(formula = yr15involvement ~ itn_c*ethrace, data = rq3data)
#current_itn_cohab <- lm(formula = yr15involvement ~ itn_c*parenttogether, data = rq3data)

#ever_m_ethrace_cat <- lm(formula = everinvolvement ~ medu_cat*ethrace, data = rq3data)
#ever_m_cohab_cat <- lm(formula = everinvolvement ~ medu_cat*parenttogether, data = rq3data)

#ever_m_ethrace_cont <- lm(formula = everinvolvement ~ medu_cont_c*ethrace, data = rq3data)
#ever_m_cohab_cont <- lm(formula = everinvolvement ~ medu_cont_c*parenttogether, data = rq3data)

#ever_itn_ethrace <- lm(formula = everinvolvement ~ itn_c*ethrace, data = rq3data)
#ever_itn_cohab <- lm(formula = everinvolvement ~ itn_c*parenttogether, data = rq3data)

#ipv_m_ethrace_cat <- lm(formula = reverseipvscore ~ medu_cat*ethrace, data = rq3data)
#ipv_m_cohab_cat <- lm(formula = reverseipvscore ~ medu_cat*parenttogether, data = rq3data)

#ipv_m_ethrace_cont <- lm(formula = reverseipvscore ~ medu_cont*ethrace, data = rq3data)
#ipv_m_cohab_cont <- lm(formula = reverseipvscore ~ medu_cont*parenttogether, data = rq3data)

#ipv_itn_ethrace <- lm(formula = reverseipvscore ~ itn_c*ethrace, data = rq3data)
#ipv_itn_cohab <- lm(formula = reverseipvscore ~ itn_c*parenttogether, data = rq3data)

#qual_m_ethrace_cat <-lm(formula = relationshipquality ~ medu_cat*ethrace, data = rq3data)
#qual_m_cohab_cat <-lm(formula = relationshipquality ~ medu_cat*parenttogether, data = rq3data)

#qual_m_ethrace_cont <-lm(formula = relationshipquality ~ medu_cont*ethrace, data = rq3data)
#qual_m_cohab_cont <-lm(formula = relationshipquality ~ medu_cont*parenttogether, data = rq3data)

#qual_itn_ethrace <-lm(formula = relationshipquality ~ itn_c*ethrace, data = rq3data)
#qual_itn_cohab <-lm(formula = relationshipquality ~ itn_c*parenttogether, data = rq3data)


#Summary of each moderator analysis
#summary(current_m_ethrace_cont)
#summary(current_m_cohab_cont)
#summary(current_itn_ethrace)
#summary(current_itn_cohab)

#summary(ever_m_ethrace_cont)
#summary(ever_m_cohab_cont)
#summary(ever_itn_ethrace)
#summary(ever_itn_cohab)

#summary(ipv_m_ethrace_cont)
#summary(ipv_m_cohab_cont)
#summary(ipv_itn_ethrace)
#summary(ipv_itn_cohab)

#summary(qual_m_ethrace_cont)
#summary(qual_m_cohab_cont)
#summary(qual_itn_ethrace)
#summary(qual_itn_cohab)


