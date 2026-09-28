#Loading Data
load("31622-0009-Data.rda")
year15data <- da31622.0009
da31622.0009$K6F4

#Data cleaning for SES measures
momeduclear <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$CP6EDU)) #This line replaces the entire string of CP6EDU with only the contents 1-9 after the ( and before the ). Negatives fail this check and are set to NA.
momeducategories <- c("(1) 1 less HS","(2) 2 HS or equiv", "(3) 3 Some coll, tech", "(4) 4 coll or grad")
momeducategorical <- momeducategories[momeduclear] #Categorical form of momeduclear, with NAs for negatives but 


summary(year15dataclear, na.rm=TRUE) #Summary of Mother's Educational Attainment variable as a measure of SES. 
householdsize <- year15data$CP6HHSIZE
householdsize[householdsize<0] <-NA
householdincome <- year15data$P6K57
householdincome[householdincome<0] <-NA

#load("thresh17.csv") This csv file does not read properly. Will manually import applicable data from this table. Using weighted values from column B
familysize <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,18)
familythresh <- c(16493, 19515, 25094, 29714, 33618, 38173, 42684, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681) #From 2017 US Census data found https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
#Calculating income-to-needs as a measure of SES
incometoneeds <- householdincome/familythresh[householdsize]
summary(incometoneeds)


#Research Question 1 -  How does SES predict intimate relationship involvement in adolescents?
#Data Cleaning
yr15involvement <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F7)) #Extracts positive numeric code value for Current Relationship Involvement from string. Incidentally sets negatives to NA. 
summary(yr15involvement)
everinvolvement <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F6)) #Extracts positive numeric code value for Any Previous Relationship Involvement from string. Incidentally sets negatives to NA. 

rq1data <- cbind.data.frame(momeduclear, incometoneeds, yr15involvement, everinvolvement)
head(rq1data)

#Regression on data
currentmom <- lm(formula = yr15involvement ~ momeduclear, data = rq1data)
currentitn <- lm(formula = yr15involvement ~ incometoneeds, data = rq1data)
evermom <- lm(formula = everinvolvement ~ momeduclear, data = rq1data)
everitn <- lm(formula = everinvolvement ~ incometoneeds, data = rq1data)

#Mother's education attainment categorical regressions
currentmomcat <- lm(formula = yr15involvement ~ momeducategorical, data = rq1data)
evermomcat <- lm(formula = everinvolvement ~ momeducategorical, data = rq1data)

#Summaries of Regressions
summary(currentmom)
summary(currentitn)
summary(evermom)
summary(everitn)

summary(currentmomcat)
summary(evermomcat)

#Research Question 2 - Among those who are in intimate relationships, is SES predictive of intimate relationship violence and relationship satisfaction?
#Data Cleaning RQ2 dependent variables
ipvqa <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18A))
ipvqb <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18B))
ipvqc <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18C))
ipvqd <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18D))

ipvscore <- ipvqa+ipvqb+ipvqc+ipvqd #Sum of IPV measure scores. Min of 4, max of 12. Min of 4 is highest IPV score, 12 is lowest
reverseipvscore <- 8-(ipvscore-4) #Adapted scores for reverse coding. A score of 8 is now the highest IPV score

relationshipquality <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F14))
head(relationshipquality, 30)

rq2data <- cbind.data.frame(momeduclear, incometoneeds, ipvscore, reverseipvscore, relationshipquality)

#Regression on data
ipvmom <- lm(formula = reverseipvscore ~ momeduclear, data = rq2data)
ipvitn <- lm(formula = reverseipvscore ~ incometoneeds, data = rq2data)
qualmom <-lm(formula = relationshipquality ~ momeduclear, data = rq2data)
qualitn <-lm(formula = relationshipquality ~ incometoneeds, data = rq2data)
ipvqual <-lm(formula = relationshipquality ~ reverseipvscore) #Analysis to replicate existing findings that IPV is not consistently associated with lower relationship quality

#Mother's education attainment categorical regressions
ipvmomcat <- lm(formula = reverseipvscore ~ momeducategorical, data = rq2data)
qualmomcat <-lm(formula = relationshipquality ~ momeducategorical, data = rq2data)

#Summaries of Regressions
summary(ipvmom) #Mother's educational attainment association with intimate partner violence
summary(ipvitn) #Family income-to-needs association with intimate partner violence
summary(qualmom) #Mother's educational attainment association with relationship quality
summary(qualitn) #Family income-to-needs association with relationship quality
summary(ipvqual) #Intimate partner violence association with relationship quality

summary(ipvmomcat)
summary(qualmomcat)
#Research Question 3 - How do race and parent’s marital status moderate these relationships?
race <- ipvqd <-as.numeric(sub("^\\((-?\\d+)\\).*", "\\1", year15data$CK6ETHRACE))
race[race<=-3] <- NA



#Covariate tests




#Data loading checks
head(momeduclear)
head(householdsize)
head(householdincome)
length(familythresh)
length(familysize)
head(incometoneeds)
head(cbind.data.frame(householdincome, householdsize, incometoneeds))
head(everinvolvement)
head(race,30)
year15data$CP6EDU
