library("caret")         
library("ggplot2")
library("dplyr")
library("haven")
library("stargazer") 
library("readxl")
library("writexl")
library("corrplot")
library("MASS")
library(broom)
library("lavaan")
library(tidyverse)
library(gridExtra)
library(psych)

theme_main <- theme(panel.background = element_rect(fill = "white", color = "#455D5A"), 
                    panel.grid = element_line(color = "black", linetype = "dotted", size = 0.2),
                    axis.text = element_text(color = "black", face = "bold"),
                    axis.title = element_text(face = "bold"),
                    legend.title = element_text(face = "bold"),
                    axis.ticks = element_line(color = "#455D5A"))
colorset <- c("#E57577", "#64864A", "#6C5B7B", "#355C7D")


##insert data##
setwd("C:/Users/juhig/OneDrive/Desktop/BU/dissertation")
data <- read_dta("ses_combined_general%20release.dta")

##data preparation
data <- data %>%
  dplyr::rename(year = dataset)
data <- data %>%
  dplyr::rename(gender = asex)
data <- data %>%
  dplyr::rename(jobsatisfaction = ksatis)
##Reason: we take only employee people
data <- data %>%
  dplyr::filter(bemptype %in% c(1))


##job satisfaction
data1 <- data %>%
  dplyr::filter(jobsatisfaction %in% c(1,2,3,5,6,7))
data1$jobsatisfaction <- recode(data1$jobsatisfaction, `1` = 6, `2` = 5, `3` = 4, `5` = 3, `6` = 2, `7` = 1)

##gender
data1 <- data1 %>%
  dplyr::filter(gender %in% c(1,2))
data1$gender <- factor(data1$gender, levels = c(1, 2),
                       labels = c("male","female"))


##we take only full time and part time

data1 <- data1 %>%
  dplyr::filter(bfultime %in% c(1,2))
data1$jobstatus <- factor(data1$bfultime, levels = c(2, 1),
                         labels = c("parttime","fulltime"))

##other variables

##jobtype

data1 <- data1 %>%
  dplyr::filter(b1soc00%in% c(1,2,3,4,5,6,7,8,9))
data1$jobtype <- factor(data1$b1soc00, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("manager","professional","associate professionals","administrative & secretarial","skilled trades","personal services","sales","plant & machine operatives","elementary"))


##age
# Create five employee age categories
data1$Agecat <- 0
data1$Agecat <- ifelse(data1$aage < 30, 1, data1$Agecat) # Below age 30
data1$Agecat <- ifelse(data1$aage  >= 30 & data1$Agecat < 40, 2, data1$Agecat) # Age 30-39.
data1$Agecat <- ifelse(data1$aage  >= 40 & data1$Agecat < 50, 3, data1$Agecat) # Age 40-49.
data1$Agecat <- ifelse(data1$aage  >= 50 & data1$Agecat < 70, 4, data1$Agecat) # Age 50+.
data1$Agecat <- factor(data1$Agecat, levels = c(1, 2, 3, 4),
                       labels = c("Age 20-29","Age 30-39","Age 40-49","Age 50+"))



##Organization size

data1 <- data1 %>%
  dplyr::filter(bworkcat%in% c(2,3,4,5))
data1$organizationalsize <- factor(data1$bworkcat, levels = c(2,3,4,5),
                                  labels = c("< 25","25-99","100-499","500+"))

##Control variables- Herzberg's factors
##motivators 

## Achievement
##in my current job i have enough opportunity to use knowledge & skills I have
data1 <- data1 %>%
  dplyr::filter(buseskil %in% c(1,2,3,4))
data1$opp_to_use_skill <- recode(data1$buseskil, `1` = 4, `2` = 3, `3` = 2, `4` = 1)


##Recognition
##gbonus1
##whether receive any incentive/bonus/commission linked to own performance

data1 <- data1 %>%
  dplyr::filter(gbonus1%in% c(1,2))
data1$bonus<- factor(data1$gbonus1, levels = c(2,1))


##work itself
##whether management arrange meetings to inform employees what is happening

data1 <- data1 %>%
  dplyr::filter(emanmeet%in% c(1,2))
data1$managermeeting<- factor(data1$emanmeet, levels = c(2,1))


##Responsibility
##my job allows me to take part in making decisions that affect my work
data1 <- data1 %>%
  dplyr::filter(bdecide %in% c(1,2,3,4))
data1$desision_power<- recode(data1$bdecide, `1` = 4, `2` = 3, `3` = 2, `4` = 1)


##Opp for promotion
##jprmprob: chances of promtn in present orgnstn?
##jprmprb1: if no chance promtn, 'cos alrdy highest?, Taken as high chance of promotion since that denote employee is encouraged.

data1$promotion <- ifelse(data1$jprmprob ==5 & data1$jprmprb1==1 ,5, data1$jprmprob)
data1 <- data1 %>%
  dplyr::filter(promotion %in% c(1,2,3,4,5))
data1$promotion <- recode(data1$promotion, `1` = 5, `2` = 4, `3` = 3, `4` = 2,`5`=1)

##Hygiene factors

##Pay
##gross hourly pay for employees

data1$hourlypay<-data1$gpayp
data1<-data1[!is.na(data1$hourlypay),]


##Supervision
##how closely supervised in job

data1 <- data1 %>%
  dplyr::filter(bsuper %in% c(1,2,3,4))
data1$supervision <- recode(data1$bsuper, `1` = 4, `2` = 3, `3` = 2, `4` = 1)

##Company policies
##evalues
##i find that my values and my organisation's values are very similar

data1 <- data1 %>%
  dplyr::filter(evalues %in% c(1,2,3,4))
data1$similar_org_values <- recode(data1$evalues, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
unique(data1$similar_org_values)
##security
##any chance to lose job in next 12 months

data1 <- data1 %>%
  dplyr::filter(blosejob %in% c(1,2))
data1$jobsecurity <-  factor(data1$blosejob, levels = c(1,2))
                                                                                                                                          

##Job orientation variables

data1 <- data1 %>%
  dplyr::filter(forient1 %in% c(1,2,3,4))
data1$promotion <- recode(data1$forient1, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$promotion),]


data1 <- data1 %>%
  dplyr::filter(forient2 %in% c(1,2,3,4))
data1$pay <- recode(data1$forient2, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$pay),]

data1 <- data1 %>%
  dplyr::filter(forient3 %in% c(1,2,3,4))
data1$relation_with_manager <- recode(data1$forient3, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$relation_with_manager),]

data1 <- data1 %>%
  dplyr::filter(forient4 %in% c(1,2,3,4))
data1$job_security <- recode(data1$forient4, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$job_security),]

data1 <- data1 %>%
  dplyr::filter(forient5 %in% c(1,2,3,4))
data1$opportunity_to_use_initiative <- recode(data1$forient5, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$opportunity_to_use_initiative),]

data1 <- data1 %>%
  dplyr::filter(forient6 %in% c(1,2,3,4))
data1$like_doing_job <- recode(data1$forient6, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$like_doing_job),]

data1 <- data1 %>%
  dplyr::filter(forient7 %in% c(1,2,3,4))
data1$convinient_hours <- recode(data1$forient7, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$convinient_hours),]

data1 <- data1 %>%
  dplyr::filter(forient8 %in% c(1,2,3,4))
data1$felxible_hours <- recode(data1$forient8, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$felxible_hours),]

data1 <- data1 %>%
  dplyr::filter(forient9 %in% c(1,2,3,4))
data1$opportunity_to_use_abilities <- recode(data1$forient9, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$opportunity_to_use_abilities),]

data1 <- data1 %>%
  dplyr::filter(forient10 %in% c(1,2,3,4))
data1$fringe_benefits <- recode(data1$forient10, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$fringe_benefits),]

data1 <- data1 %>%
  dplyr::filter(forient11 %in% c(1,2,3,4))
data1$easy_workload <- recode(data1$forient11, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$easy_workload),]

data1 <- data1 %>%
  dplyr::filter(forient12 %in% c(1,2,3,4))
data1$training <- recode(data1$forient12, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$training),]

data1 <- data1 %>%
  dplyr::filter(forient13 %in% c(1,2,3,4))
data1$physical_work_condition <- recode(data1$forient13, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$physical_work_condition),]

data1 <- data1 %>%
  dplyr::filter(forient14 %in% c(1,2,3,4))
data1$variety <- recode(data1$forient14, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$variety),]

data1 <- data1 %>%
  dplyr::filter(forient15 %in% c(1,2,3,4))
data1$friendly_people <- recode(data1$forient15, `1` = 4, `2` = 3, `3` = 2, `4` = 1)
data1<-data1[!is.na(data1$friendly_people),]

##CFA

model<-'
intrinsic=~opportunity_to_use_initiative+like_doing_job+opportunity_to_use_abilities+variety
extrinsic=~pay+physical_work_condition+fringe_benefits
effort=~convinient_hours+felxible_hours+easy_workload
future=~promotion+job_security+training
human=~relation_with_manager+friendly_people'

fit1 <- cfa(model, data = data1)
summary(fit1, fit.measures = TRUE, standardized = TRUE)


##model is made my taking mean of variables in each factors

##intrinsic orientation 5,6,9,14
data1$intrinsic <- rowMeans(data1[ , c("opportunity_to_use_initiative","like_doing_job","opportunity_to_use_abilities","variety")], na.rm=TRUE)
unique(data1$intrinsic)
##Extrinsic 2,13,10
data1$extrinsic <- rowMeans(data1[ , c("pay","physical_work_condition","fringe_benefits")], na.rm=TRUE)
#Effort 7,8,11
data1$effort<- rowMeans(data1[ , c("convinient_hours","felxible_hours","easy_workload")], na.rm=TRUE)
##future 1,4,12
data1$future <- rowMeans(data1[ , c("promotion","job_security","training")], na.rm=TRUE)
##Human 3,15
data1$human <- rowMeans(data1[ , c("relation_with_manager","friendly_people")], na.rm=TRUE)


##mean job orientation between male and female
a<-data1 %>% group_by(gender) %>% summarise(number_of_employees = n(), 
                                            intrinsic = mean(intrinsic), extrinsic = mean(extrinsic),
                                            effort = mean(effort), future = mean(future),
                                            human = mean(human))
##Regression 
##general
form1<-formula(jobsatisfaction~gender)
model1<-lm(form1,data=data1)
summary(model1)

##there exist a gender gap in job satisfaction where female holds higher satisfaction than male.
##Now adding Hertzbergs motivators and hygiene factors

form2<-formula(jobsatisfaction~gender+intrinsic+extrinsic+effort+future+human)
model2<-lm(form2,data=data1)
summary(model2)


form2a<-formula(jobsatisfaction~gender*intrinsic+extrinsic+effort+future+human)
model2a<-lm(form2a,data=data1)
summary(model2a)

graph1 <- augment(model2a)
ggplot(graph1, aes(intrinsic, .fitted, color = gender)) + geom_smooth(method = "lm") + theme_main +
  scale_color_manual(values = colorset) +
  labs(x = "intrinsic_orientation", y = "job satisfaction (Predicted)" , color = "Gender")

##Gap slightly got reduced. But still exists significantly. Bonus, presence of supervisor and manager meetings turns insignificant
##Adding orientation factors to see if the impact justify higher satisfaction of women

form3<-formula(jobsatisfaction~gender+intrinsic+extrinsic+effort+future+human+bonus+supervision+managermeeting+jobsecurity+opp_to_use_skill+desision_power+similar_org_values+hourlypay+promotion)
model3<-lm(form3,data=data1)
summary(model3)


##Only extrinsic and human orientation stays significant and eventhough it reduce the job satisfaction gender gap, the gap stays significant.
##with the advent of orientation factors, union turns insignificant.
##Finally adding organizational and personal attributes to understand its impact

form4<-formula(jobsatisfaction~gender+intrinsic+extrinsic+effort+future+human+bonus+supervision+managermeeting+jobsecurity+opp_to_use_skill+desision_power+similar_org_values+hourlypay+promotion+Agecat+jobtype+jobstatus+organizationalsize)
model4<-lm(form4,data=data1)
summary(model4)


##Final model
##supervisor,intrinsic,effort,future,genderratio,sector,maritalstatus seems insignificant

stargazer(model1,model2,model3,model4, type = "html", report = 'vc*p', out = "C:/Users/juhig/OneDrive/Desktop/BU/dissertation/reg1.htm", star.cutoffs = c(0.05, 0.01, 0.001),align=TRUE)
stargazer(ano1,ano2,ano3,ano4,ano5,ano6,ano7,ano8,ano9,ano10,ano11,ano12,ano13,ano14,ano15, type = "html", report = 'vc*p', out = "C:/Users/juhig/OneDrive/Desktop/BU/dissertation/anova1.htm", star.cutoffs = c(0.05, 0.01, 0.001),align=TRUE)

##################################################################################

b<-ggplot(data1, aes(aage, intrinsic)) + geom_smooth() + theme_main +
  scale_color_manual(values = colorset) +
  labs(x = "Age", y = "Intrinsic orientation")
c<-ggplot(data1, aes(aage, extrinsic)) + geom_smooth() + theme_main +
  scale_color_manual(values = colorset) +
  labs(x = "Age", y = "Extrinsic orientation")
d<-ggplot(data1, aes(aage, effort)) + geom_smooth() + theme_main +
  scale_color_manual(values = colorset) +
  labs(x = "Age", y = "Effort orientation")
e<-ggplot(data1, aes(aage, future)) + geom_smooth() + theme_main +
  scale_color_manual(values = colorset) +
  labs(x = "Age", y = "Future orientation")
f<-ggplot(data1, aes(aage, human)) + geom_smooth() + theme_main +
  scale_color_manual(values = colorset) +
  labs(x = "Age", y = "Human orientation")

grid.arrange(b,c,d,e,f)

##DurbinwatsonTest
library(car)
##this package mask few packages. hence adding here
durbinWatsonTest(model4)

##partial F test

anova(model3, model4)
anova(model2, model3)
anova(model1, model2)


##Correlation matrix
data2<- data1 %>%
  dplyr::select(jobsatisfaction,intrinsic,extrinsic,effort,future,human,supervision,opp_to_use_skill,desision_power,similar_org_values,hourlypay,promotion)
M<-cor(data2)  
head(round(M,2))
corrplot(M, method = 'number', order = 'FPC', type = 'lower', diag = FALSE)
summary(model4)

##Descriptive statistics
data3<- data1 %>%
  dplyr::select(jobsatisfaction,gender,intrinsic,extrinsic,effort,future,human,bonus,supervision,managermeeting,jobsecurity,opp_to_use_skill,desision_power,similar_org_values,hourlypay,promotion,Agecat,jobtype,jobstatus,organizationalsize)
describe(data3)
