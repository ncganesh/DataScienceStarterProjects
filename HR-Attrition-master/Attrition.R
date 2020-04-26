
##########Predicting attrition rates#####################

library("ggplot2")
library("tidyr")
library("dplyr")
library("corrplot")
library("miscset")
library("purrr")
require(gridExtra)

library(caTools)
library(e1071)
library(glmnet)



##########Importing data

emp <- read.csv("C:/Users/Admin/Desktop/Datasets/ORMAE/HR_Employee_Attrition_Data.csv")

##Viewing the structure of the data - 1.Variable Identification
str(emp)
dim(emp)
summary(emp)


##After checking the summary and visualizing,Removing Employee number and std hours and converting 
##some variables to factors which is stored in dataset as integers
head(emp)

########Removing Employee Number,Standard hours#############
emp <- (emp[ ,-c(9,10,27)])


##Some variables such as Education,JobInvolvement..etc  which are factors are 
##stored asintegers ,so Converting these  continuos variables to Categorical data##
WorkLifeBalance


names <- c('WorkLifeBalance' ,'StockOptionLevel','PerformanceRating','JobSatisfaction',
           'RelationshipSatisfaction','JobLevel','JobInvolvement','EnvironmentSatisfaction','Education')
emp[,names] <- lapply(emp[,names] , factor)
str(emp)



##################UNIVARIATE ANALYSIS##############################


###########Visualizing Each Numeric data#################
emp[33:35] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue") 

boxplot(attrition$ï..Age)


########### vISUALIZING EACH categorical data #################

empcat <- names(which(sapply(emp,is.factor)))
empnum <- names(which(sapply(emp,is.numeric)))

empn <- which(sapply(emp,is.numeric))
mean(emp$PercentSalaryHike)

ggplotGrid(ncol = 3,lapply(empcat[1:9],function(col) {
  ggplot(emp, aes_string(col)) + geom_bar(fill = "red") + 
    theme(axis.text.x = element_text(size  = 10,
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1))
        }))

#################BiVariate Analysis##############


################Age and Attrition#################

ggplot(emp,aes(x = Age,fill = Attrition)) +
  geom_bar(position = "fill")

##We can infer that Attrition is more between Age 18 and 33 when compared to employees whose age 
##is more than 33.There is increase in attrition whose Age is greater than 
## 50 as employees usually retire at age of 55

#############converting into groups#################
 emp$Agec <- cut(emp$Age, breaks = c(18,33,60),
                   labels = c("18-33","33-60"), include.lowest = TRUE)
head(emp$Agec)

#######Plotting Age(groups) vs Attrition
p1 <- ggplot(emp,aes(x = Agec,fill = Attrition)) +
  geom_bar(position = "dodge") +
    ggtitle("Age(group) vs Attrition - count")


p2 <- ggplot(emp,aes(x = Agec,fill = Attrition)) +
  geom_bar(position = "fill") +
  ggtitle("Age(groups) vs Attrition - proportion")

grid.arrange(p1, p2, ncol=2)


########Second Variable

co16 <- ggplot(emp,aes(x = DistanceFromHome,fill = Attrition)) + 
  geom_bar(position = "fill")

##We can infer that Attrition rate is more if distance from home is more than 12

emp$dist1 <- cut(emp$DistanceFromHome, breaks = c(0,12,30),
                labels = c("0-12","12-30"), include.lowest = TRUE)
p3 <- ggplot(emp,aes(x = dist1,fill = Attrition)) + 
  geom_bar(position = "fill") 
  
p4 <- ggplot(emp,aes(x = dist1,fill = Attrition)) +
  geom_bar(position = "dodge") 
  
grid.arrange(p3, p4, ncol=2)

######3rd Variable : Education.

##We can infer that employees who's Education doesnot  influence Employee Attrition . 
  
co1 <- ggplot(emp,aes(x = Education,fill = Attrition)) + 
  geom_bar(position = "fill")


#We can infer that employees Attrition is more if  EnvironmentSatisfaction 
##is 1 - need to CATEGORICAL
co2 <- ggplot(emp,aes(x = EnvironmentSatisfaction,fill = Attrition)) + 
  geom_bar(position = "fill")

'WorkLifeBalance' ,'StockOptionLevel','PerformanceRating',
'JobSatisfaction','RelationshipSatisfaction','JobLevel',
'JobInvolvement','EnvironmentSatisfaction','Education'
co3 <- ggplot(emp,aes(x = JobInvolvement,fill = Attrition)) + 
  geom_bar(position = "fill")

co4 <- ggplot(emp,aes(x = JobLevel,fill = Attrition)) + 
  geom_bar(position = "fill")

co5 <- ggplot(emp,aes(x = JobSatisfaction,fill = Attrition)) + 
  geom_bar(position = "fill")

co6 <- ggplot(emp,aes(x = PerformanceRating,fill = Attrition)) + 
  geom_bar(position = "fill")

co7 <- ggplot(emp,aes(x = PercentSalaryHike,fill = Attrition)) + 
  geom_bar(position = "fill")


co9 <- ggplot(emp,aes(x = StockOptionLevel,fill = Attrition)) + 
  geom_bar(position = "fill")

co10 <- ggplot(emp,aes(x = TotalWorkingYears,fill = Attrition)) + 
  geom_bar(position = "fill")

co11 <- ggplot(emp,aes(x = TrainingTimesLastYear,fill = Attrition)) + 
  geom_bar(position = "fill")

co12 <- ggplot(emp,aes(x = YearsAtCompany,fill = Attrition)) + 
  geom_bar(position = "fill")

co13 <- ggplot(emp,aes(x = YearsInCurrentRole,fill = Attrition)) + 
  geom_bar(position = "fill")

co14 <- ggplot(emp,aes(x = YearsSinceLastPromotion,fill = Attrition)) + 
  geom_bar(position = "fill")

co15 <- ggplot(emp,aes(x = YearsWithCurrManager,fill = Attrition)) + 
  geom_bar(position = "fill")

grid.arrange(co1,co2,co3,co4,co5,co6,co7,co9,ncol=4)
grid.arrange(co10,co12,co13,co14,co15,co11,ncol=2)



emp$totwyears <- cut(emp$TotalWorkingYears,breaks = c(0,10,20,40),
                 labels = c("0-10","10-20","20-40"), include.lowest = TRUE)

##############Exploring Ctaegorical Variables#############

class(emp$BusinessTravel)
str(emp)
empcat

pc1 <- ggplot(emp,aes(x = BusinessTravel,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") 

pc2 <- ggplot(emp,aes(x = Department,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") + 
  theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1))
                                                                          

pc3 <- ggplot(emp,aes(x = EducationField,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") +
  theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1))

pc4 <- ggplot(emp,aes(x = Gender,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

pc5 <- ggplot(emp,aes(x = JobRole,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") +
  theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1))


pc6 <- ggplot(emp,aes(x = MaritalStatus,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

pc7 <- ggplot(emp,aes(x = Over18,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

pc8 <- ggplot(emp,aes(x = OverTime,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

PC9 <- ggplot(emp,aes(x = EnvironmentSatisfaction,..count..)) + 
  geom_bar(position = "fill")


PC10 <- ggplot(emp,aes(x = JobInvolvement,..count..)) + 
  geom_bar(position = "fill")

PC11 <- ggplot(emp,aes(x = JobLevel,..count..)) + 
  geom_bar(position = "fill")

PC12 <- ggplot(emp,aes(x = JobSatisfaction,..count..)) + 
  geom_bar(position = "fill")
grid.arrange(pc1,pc2,pc3,pc4,ncol=2)

grid.arrange(pc6,co16,ncol=2)


ggplot(emp, aes(x =TotalWorkingYears, y = YearsAtCompany,col = BusinessTravel)) +
  geom_point() +
  facet_grid(.~Attrition)

ggplot(emp, aes(x = YearsAtCompany, y = Age,col  = OverTime)) +
  geom_point() +
  facet_grid(.~Attrition)


#############################################################################################


names(emp[empn])

selectedVars <- names(emp[ ,c("Age","MonthlyIncome","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole"     
                  ,"YearsSinceLastPromotion","YearsWithCurrManager")])

corrplot(cor(emp[empn]),type = "upper",method='color',tl.cex = .7,cl.cex = .7,number.cex = 0.7)

prin_comp <- prcomp(emp[selectedVars], scale. = T, retx = TRUE)

# Considering only strong predictor variables

(newDat <- emp[, c("Agec","BusinessTravel","OverTime","JobInvolvement","JobLevel",
                   "Attrition","JobSatisfaction","StockOptionLevel",
                   "NumCompaniesWorked","StockOptionLevel","YearsAtCompany",
                   "JobRole", "DistanceFromHome","PercentSalaryHike",
                   "RelationshipSatisfaction","WorkLifeBalance")])

#Splitting data
library(caret)
index <- createDataPartition(newDat$Attrition , p =0.7,list = FALSE)
Train <- newDat[index,]
Test <- newDat[-index,]

## # Building my first model. including all the variables
attrition_model <- glm(Attrition ~ . , data = Train, family=binomial(link = 'logit'))
summary(attrition_model)

##Validating Test - classification
Test$rankP <- predict(attrition_model, newdata = Test, type = "response")
Test$rankV <- ifelse(Test$rankP >= 0.5 ,1,0)
mytable <- table(Test$Attrition,Test$rankV)
mytable

 score <- (718+77)/(718+77+65+21)
 
 ########Accuracy is 0.90  #############

 
 
str(emp)
 
 ggplot(insurance, aes(x = age, y = charges)) +
   geom_point() + 
   facet_wrap( ~ smoker, ncol = 2) +
   theme(text = element_text(size=20))






