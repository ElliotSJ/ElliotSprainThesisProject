load("C:/Users/ellio/Desktop/University/Thesis data/ICPSR_31622-V5/ICPSR_31622/DS0009/31622-0009-Data.rda")
year15data <- da31622.0009

da31622.0009$K6F4


year15dataclear <- as.numeric(sub("^\\((\\d+)\\).*", "\\1", year15data$CP6EDU)) #This line replaces the entire string of CP6EDU with only the contents 1-9 after the ( and before the ). Negatives fail this check and are set to NA.
year15dataclear

summary(year15dataclear, na.rm=TRUE) #Summary of Mother's Educational Attainment variable as a measure of SES. 

#Research Question 1 -  How does SES predict intimate relationship involvement in adolescents?





#Research Question 2 - Among those who are in intimate relationships, is SES predictive of intimate relationship violence and relationship satisfaction?





#Research Question 3 - How do race and parent’s marital status moderate these relationships?