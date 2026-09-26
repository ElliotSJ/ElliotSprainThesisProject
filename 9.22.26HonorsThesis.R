load("31622-0009-Data.rda")
year15data <- da31622.0009

da31622.0009$K6F4


momeduclear <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$CP6EDU)) #This line replaces the entire string of CP6EDU with only the contents 1-9 after the ( and before the ). Negatives fail this check and are set to NA.
head(momeduclear)

summary(year15dataclear, na.rm=TRUE) #Summary of Mother's Educational Attainment variable as a measure of SES. 

householdsize <- year15data$CP6HHSIZE
householdsize[householdsize<0] <-NA
head(householdsize)

householdincome <- year15data$P6K57
householdincome[householdincome<0] <-NA
head(householdincome)
#load("thresh17.csv") This csv file does not read properly. Will manually import applicable data from this table. Using weighted values from column B
familysize <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,18)
familythresh <- c(16493, 19515, 25094, 29714, 33618, 38173, 42684, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681, 50681) #From 2017 US Census data found https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
length(familythresh)
length(familysize)
incometoneeds <- householdincome/familythresh[householdsize]
head(incometoneeds)
summary(incometoneeds)
head(cbind.data.frame(householdincome, householdsize, incometoneeds))

     
#Research Question 1 -  How does SES predict intimate relationship involvement in adolescents?

yr15involvement <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F7)) #Extracts positive numeric code value for Current Relationship Involvement from string. Incidentally sets negatives to NA. 
summary(yr15involvement)
everinvolvement <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F6)) #Extracts positive numeric code value for Any Previous Relationship Involvement from string. Incidentally sets negatives to NA. 
head(everinvolvement)
rq1data <- cbind.data.frame(momeduclear, incometoneeds, yr15involvement, everinvolvement)
head(rq1data)

#Research Question 2 - Among those who are in intimate relationships, is SES predictive of intimate relationship violence and relationship satisfaction?

ipvqa <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18A))
ipvqb <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18B))
ipvqc <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18C))
ipvqd <-as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F18D))

ipvscore <- ipvqa+ipvqb+ipvqc+ipvqd #Sum of IPV measure scores. Min of 4, max of 12. Min of 4 is highest IPV score, 12 is lowest
reverseipvscore <- 8-(ipvscore-4) #Adapted scores for reverse coding. A score of 8 is now the highest IPV score

relationshipquality <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$K6F14))
head(relationshipquality, 30)


#Research Question 3 - How do race and parent’s marital status moderate these relationships?
race <- ipvqd <-as.numeric(sub("^\\((-?\\d+)\\).*", "\\1", year15data$CK6ETHRACE))
race[race<=-3] <- NA
head(race,30)


