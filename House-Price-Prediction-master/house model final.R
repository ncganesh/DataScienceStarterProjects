#Predicting the House Price
library("gridExtra")
library("corrplot")
library("e1071")
library("gridExtra")
library("dummies")
library("caret")
library("randomForest")
library("tidyverse")
library("Metrics")
library("gbm")
library("xgboost")
library("glmnet")
library("Boruta")
library("gbm")




###Predicting the House Price#################

#Importing the data.

train<- read.csv("D:/KAGGLE/HousePrice/Dataset/train.csv",header = T,stringsAsFactors = F)
test<- read.csv("D:/KAGGLE/HousePrice/Dataset/test.csv",header = T,stringsAsFactors = F )


dim(train)                                       
dim(test)
###Removing Outliers
options(scipen = 9999)
ggplot(train,aes(x = GrLivArea,y = SalePrice)) +
  geom_point(col = "red")
train[which(train$GrLivArea >4000 & train$SalePrice < 200000), ]$GrLivArea <- mean(hprice$GrLivArea)

###Transforming dependent variable
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")+scale_x_log10()
train$SalePrice <- log(train$SalePrice+1)
test$SalePrice <- as.numeric(0)

###Binding the train and test
hprice <- rbind(train,test)
dim(hprice)
hprice = hprice[ ,-1]


options(scipen = 99)
train[1:46] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue") 

train[46:81] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue") 
  ##Transforming variables which are categorical

hprice$MSSubClass <- as.character(hprice$MSSubClass)
hprice$OverallQual <- as.character(hprice$OverallQual)
hprice$OverallCond <- as.character(hprice$OverallCond)

#Structure of the data set
str(hprice)
#Summary of dataset
summary(hprice)

##checking Columns which have  NA'S

missing_data <- as.data.frame(sort(sapply(hprice, function(x) sum(is.na(x))),decreasing = T))

missing_data <- (missing_data/2919)*100
colnames(missing_data)[1] <- "missingvaluesPercentage"
missing_data$features <- rownames(missing_data)
ggplot(missing_data[missing_data$missingvaluesPercentage >5,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage,fill= features)) +
  geom_bar(stat="identity") +theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missingvalues") +
  xlab("Feature") + ggtitle("Understanding Missing Data")

ggplot(hprice,aes(x = Fence)) +
  geom_bar(fill = "blue")
#Imputing Missing Values

hprice$PoolQC[is.na(hprice$PoolQC)] = 'None'
hprice$MiscFeature[is.na(hprice$MiscFeature)] = 'None'
hprice$Alley[is.na(hprice$Alley)] = 'None'
hprice$Fence[is.na(hprice$Fence)] = 'None'
hprice$FireplaceQu[is.na(hprice$FireplaceQu)] = 'None'

#####taking care of  NA's - garage 
hprice$GarageCond[is.na(hprice$GarageCond)] = 'None'
hprice$GarageFinish[is.na(hprice$GarageFinish)] = 'None'
hprice$GarageQual[is.na(hprice$GarageQual)] = 'None'
hprice$GarageType[is.na(hprice$GarageType)] = 'None'


######taking care of NA's - Bsmt 
hprice$BsmtCond[is.na(hprice$BsmtCond)] = 'None'
hprice$BsmtExposure[is.na(hprice$BsmtExposure)] = 'None'
hprice$BsmtQual[is.na(hprice$BsmtQual)] = 'None'
hprice$BsmtFinType1[is.na(hprice$BsmtFinType1)] = 'None'
hprice$BsmtFinType2[is.na(hprice$BsmtFinType2)] = 'None'

###Masonry
hprice$MasVnrType[is.na(hprice$MasVnrType)] = 'None'
hprice$MasVnrArea[is.na(hprice$MasVnrArea)] = 0

### Looking into Lot Frontage - Linear feet of street connected to property which has 486 NA's:
##Imputing all the NA's with median value

hprice$LotFrontage[is.na(hprice$LotFrontage)] = 0 

###Garage year built
yblt <- which(is.na(hprice$GarageYrBlt))
hprice[yblt, 'GarageYrBlt'] <- hprice[yblt, 'YearBuilt']


#removing rest of the cols which has NA's

table(hprice$BsmtFullBath)
table(hprice$BsmtHalfBath)
table(hprice$MSZoning)
table(hprice$Utilities)
table(hprice$Functional)
table(hprice$Exterior1st)
table(hprice$Exterior2nd)
table(hprice$Electrical)
table(hprice$KitchenQual)
table(hprice$SaleType)
table(hprice$GarageCars)


hprice$BsmtFullBath[is.na(hprice$BsmtFullBath)] = 0
hprice$BsmtHalfBath[is.na(hprice$BsmtHalfBath)] = 0
hprice$MSZoning[is.na(hprice$MSZoning)] = 'RL'
hprice$Utilities[is.na(hprice$Utilities)] = 'AllPub'
hprice$Functional[is.na(hprice$Functional)] = 'Typ'
hprice$Exterior1st[is.na(hprice$Exterior1st)] = 'MetalSd'
hprice$Exterior2nd[is.na(hprice$Exterior2nd)] = 'VinylSd'
hprice$Electrical[is.na(hprice$Electrical)] = 'SBrkr'
hprice$KitchenQual[is.na(hprice$KitchenQual)] = 'TA'
hprice$SaleType[is.na(hprice$SaleType)] = 'WD'
hprice$GarageCars[is.na(hprice$GarageCars)] = 2
hprice$BsmtFinSF1[is.na(hprice$BsmtFinSF1)] <- 0
hprice$BsmtFinSF2[is.na(hprice$BsmtFinSF2)] <- 0
hprice$BsmtUnfSF[is.na(hprice$BsmtUnfSF)] <- 0
hprice$TotalBsmtSF[is.na(hprice$TotalBsmtSF)] <- 0 
hprice$GarageArea[is.na(hprice$GarageArea)] <- 0

sum(is.na(hprice))

#################


###Recode ordered factors as pseudo-continuous numerical variables -
hprice$ExterQual<- recode(hprice$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$ExterCond<- recode(hprice$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$BsmtQual<- recode(hprice$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$BsmtCond<- recode(hprice$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$BsmtExposure<- recode(hprice$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
hprice$BsmtFinType1<- recode(hprice$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
hprice$BsmtFinType2<- recode(hprice$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
hprice$HeatingQC<- recode(hprice$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$KitchenQual<- recode(hprice$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$Functional<- recode(hprice$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
hprice$FireplaceQu<- recode(hprice$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$GarageFinish<- recode(hprice$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
hprice$GarageQual<- recode(hprice$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$GarageCond<- recode(hprice$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$PoolQC<- recode(hprice$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$Fence<- recode(hprice$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)

hprice$TotalSF = hprice$TotalBsmtSF + hprice$X1stFlrSF + hprice$X2ndFlrSF

summary(hprice)

#######Encoding for Categorical variable
hpricedummy <- dummy.data.frame(hprice,dummy.classes = "character")
hpricedummy <- rename(hpricedummy,"MSZoningC"="MSZoningC (all)")
hpricedummy <- rename(hpricedummy,"RoofMatlTarGrv"="RoofMatlTar&Grv")
hpricedummy <- rename(hpricedummy,"Exterior1stWdSdng"="Exterior1stWd Sdng")
hpricedummy <- rename(hpricedummy,"Exterior2ndBrkCmn"="Exterior2ndBrk Cmn")
hpricedummy <- rename(hpricedummy,"Exterior2ndWdSdng"="Exterior2ndWd Sdng")
hpricedummy <- rename(hpricedummy,"Exterior2ndWdShng"="Exterior2ndWd Shng")

str(hpricedummy)
dim(hpricedummy)

###########Transforming Skewed Variables#############

numnames <- names(which(sapply(hpricedummy,is.numeric)))
catname <- names(which(sapply(hpricedummy,is.character)))
numnam <- names(which(sapply(hpricedummy,is.numeric)))

skew <- sapply(numnames,function(x){skewness(hpricedummy[[x]],na.rm = T)})
# Let us determine a threshold skewness and transform all variables above the treshold.
skew <- skew[skew > 0.75]
# transform excessively skewed features with log(x + 1)
for(x in names(skew)) {
 hpricedummy[[x]] = log(hpricedummy[[x]]+1)
}

##################Train & Test#######################
train1 <- hpricedummy[1:1460,]
test1 <- hpricedummy[1461:2919, ]

###########Linear Regression##############0.141######
trainctrl = trainControl(method = "cv",number = 5,
                         verboseIter = T)

lmmodel = train(SalePrice ~ .,train1,method = "lm",
                trControl = trainControl(method = "cv",number = 5,verboseIter = T))
summary(lmmodel)

lmmodel1 = lm(SalePrice ~ .,train1)
plot(lmmodel1)
lmpred = exp(predict(lmmodel,test1))

lmsubmission = data.frame(Id = test$Id,SalePrice = lmpred)
write.csv(lmsubmission,"lmsubmission.csv",row.names = F)


#########################lm - pca ####### 0.13796##########

lmpcamodel = train(SalePrice ~ .,train1,method = "lm",trControl = trainControl(method = "cv",number = 5,
                                                                               verboseIter = T),
                   preProcess = c("zv","center","scale","pca"))


summary(lmpcamodel)
lmpcapred = exp(predict(lmpcamodel,test1))

lmpcasubmission = data.frame(Id = test$Id,SalePrice = lmpcapred)
write.csv(lmpcasubmission,"lmpcasubmission.csv",row.names = F)


####################Random Forest##########0.144#####
set.seed(123)
rfmodel = train(SalePrice ~ .,train1,tuneLength = 5,method = "ranger",
                trControl = trainControl(method = "cv",number = 5,
                                         verboseIter = T))
plot(rfmodel)
summary(rfmodel)

rfpred = exp(predict(rfmodel,test1)) - 1

rfsubmission = data.frame(Id = test$Id,SalePrice = rfpred)
write.csv(rfsubmission,"rfsubmission.csv",row.names = F)

################GLMNET MOdels#############0.1316###
set.seed(1234)
glmnetmodel = train(SalePrice ~ .,train1,tuneGrid = expand.grid(alpha = 0:1,lambda = seq(0.0001,1,length = 10)),
                    method = "glmnet",metric = "RMSE",trControl = trainctrl)
plot(glmnetmodel)

glmnetpred = exp(predict(glmnetmodel,test1)) - 1

glmnetsubmission = data.frame(Id = test$Id,SalePrice = glmnetpred)
write.csv(glmnetsubmission,"glmnetsubmission.csv",row.names = F)

####################GBM###################

set.seed(12)
gbmodel = train(SalePrice ~ .,train1,
                method = "gbm",metric = "RMSE",trControl = trainctrl)
gbmpred = exp(predict(gbmodel,test1))

gbmsubmission = data.frame(Id = test$Id,SalePrice = gbmpred)
write.csv(gbmsubmission,"gbmsubmission.csv",row.names = F)
names(train1)

##################XGBOOST#######################################

dtrain = xgb.DMatrix(data = as.matrix(train1[ ,-266]),label = as.matrix(train1$SalePrice))
dtest = xgb.DMatrix(data = as.matrix(test1[ ,-266]),label = as.matrix(test1$SalePrice))

xgb <- xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE, 
               objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
               gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)

xgbpred = exp(predict(xgb,dtest))-1
xgbsubmission =  data.frame(Id = test$Id,Saleprice = xgbpred)
write.csv(xgbsubmission,"xgbsubmission.csv",row.names = F)

####################weighted average##########0.12834################


weightedsubmission = data.frame(Id = test$Id,SalePrice = (glmnetpred + gbmpred + )/2)
write.csv(weightedsubmission,"weightedsubmission.csv",row.names = F)

weightsubmission = data.frame(Id = test$Id,SalePrice = (0.6*glmnetpred + 0.4*gbmpred))
write.csv(weightsubmission,"weightsubmission.csv",row.names = F)

#####################Including only Variable############
set.seed(12)
gbimpmodel = train(SalePrice ~ TotalSF+GrLivArea+ ExterQual+ KitchenQual + FireplaceQu +
                     BsmtQual+  GarageCars+YearBuilt+ GarageArea+GarageFinish+
                     LotArea+YearRemodAdd +BsmtFinSF1+X1stFlrSF+ TotalBsmtSF +
                     MSZoningC+ Fireplaces+Functional+BsmtUnfSF+ BsmtFinType1,
                   data = train1,
                   method = "gbm",metric = "RMSE",trControl = trainctrl)
gbmimppred = exp(predict(gbimpmodel,test1)) -1

gbmimpsubmission = data.frame(Id = test$Id,SalePrice = gbmimppred)
write.csv(gbmimpsubmission,"gbmimpsubmission.csv",row.names = F)


###############StepAIC######

stepmodel = step(lm(SalePrice ~ ., data = train1), direction = "both")
summary(stepmodel)
steppred = exp(predict(stepmodel,test1))-1
xgbsubmission =  data.frame(Id = test$Id,Saleprice = steppred)
write.csv(xgbsubmission,"stepsubmission.csv",row.names = F)



#step(lm(SalePrice ~ selvar, data = train1), direction = "both")

m1<-  lm(formula = SalePrice ~ `MSZoningC (all)` + MSZoningFV + MSZoningRH + 
           MSZoningRL + LotArea + AlleyGrvl + LandContourBnk + LandContourHLS + 
           LotConfigCulDSac + LandSlopeGtl + LandSlopeMod + Condition2 + 
           OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatlClyTile + 
           RoofMatlCompShg + RoofMatlRoll + `RoofMatlTar&Grv` + RoofMatlWdShake + 
           Exterior1st + Exterior2nd + MasVnrTypeBrkCmn + MasVnrTypeBrkFace + 
           MasVnrArea + ExterQual + ExterCond + FoundationPConc + FoundationSlab + 
           FoundationStone + BsmtQual + BsmtCond + BsmtFinSF1 + BsmtFinSF2 + 
           BsmtUnfSF + TotalBsmtSF + HeatingGrav + CentralAirN + ElectricalMix + 
           X1stFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + FullBath + 
           HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Functional + 
           Fireplaces + GarageType2Types + GarageTypeAttchd + GarageTypeBasment + 
           GarageTypeCarPort + GarageYrBlt + GarageCars + GarageArea + 
           GarageQualEx + WoodDeckSF + EnclosedPorch + ScreenPorch + 
           PoolArea + MiscFeatureGar2 + MiscFeatureShed + MiscVal + 
           SaleTypeCWD + SaleTypeNew + SaleConditionAbnorml + SaleConditionFamily, 
         data = train1)






