############Loading Libraries##############

library("tidyverse")
library("data.table")
library("leaflet")
library("ggmap")
library("lubridate")
library("gridExtra")
library("caret")
library("xgboost")
library("dummies")
library("gbm")
library("sp")
library("rgeos")
library("geosphere")

taxitrain <- fread("D:/KAGGLE/Taxi/Dataset/train.csv")
taxitest <- fread("D:/KAGGLE/Taxi/Dataset/test.csv")

##########Structure and Summary of Data#######

str(taxitrain)
summary(taxitrain)


#################Visualizing Dependent Variable################


options(scipen = 99)                     
ggplot(taxitrain,aes(x = trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10()

###################Transforming Dependent Variable - result - rmsle####

taxitrain$trip_duration = log(taxitrain$trip_duration + 1)

###Combining train and test

taxitest$trip_duration = 0
taxitest$dropoff_datetime = NA
taxicom =  bind_rows(taxitrain,taxitest)

##########Converting data acc to specific data types#########

taxicom = taxicom %>%
            mutate(pickup_datetime  = ymd_hms(pickup_datetime),
                   dropoff_datetime = ymd_hms(dropoff_datetime),
                   vendor_id        = factor(vendor_id),
                   passenger_count  = factor(passenger_count))

##############Visualizing the NYC MAP############

set.seed(1234)
foo <- sample_n(taxitrain,8e3)
leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
                   color = "red", fillOpacity = 0.3)



  
##########################Univariate Analysis###########
  
options(scipen =  99)  
 
 names(taxitrain)
 taxitrain %>%
   select(vendor_id) %>%
   group_by(vendor_id) %>%
   count()
 
 p1 = ggplot(taxitrain,aes(x = vendor_id)) + 
    geom_bar(fill = "red")
  
 taxitrain %>%
   select(passenger_count) %>%
   group_by(passenger_count) %>%
   count() 
 
 ggplot(taxitrain,aes(x = passenger_count)) + 
    geom_bar(fill = "red")
  
  
  p3 = ggplot(taxitrain,aes(x = store_and_fwd_flag)) + 
    geom_bar(fill = "red")
  
 p4 = ggplot(taxitrain,aes(x = pickup_datetime)) + 
   geom_histogram(fill = "blue") 
 
 p5 = ggplot(taxitrain,aes(x = dropoff_datetime)) + 
      geom_histogram(fill="blue")
 
 grid.arrange(p1,p2,p3,p4,p5)
  
 p6 =  ggplot(taxitrain,aes(pickup_latitude)) +
   geom_histogram(fill = "red", bins = 40) +
   scale_x_continuous(limits = c(40.62,40.85))
 
 p7 = ggplot(taxitrain,aes(pickup_longitude)) +
   geom_histogram(fill = "red", bins = 40) +
   scale_x_continuous(limits = c(-74.05,-73.75))
 
 p8 =  ggplot(taxitrain,aes(dropoff_latitude)) +
   geom_histogram(fill = "blue", bins = 40) +
   scale_x_continuous(limits = c(40.62,40.85))
 p9 = ggplot(taxitrain,aes(dropoff_longitude)) +
   geom_histogram(fill = "red", bins = 40) +
   scale_x_continuous(limits = c(-74.05,-73.75))

  grid.arrange(p6,p7,p8,p9)
 
 p10 =  ggplot(taxitrain,aes(x = day(pickup_datetime),col = vendor_id))+
   geom_histogram(fill = "blue") + scale_x_continuous(breaks=1:31)
 p11 = ggplot(taxitrain,aes(x = month(pickup_datetime)))+
   geom_histogram(fill = "blue") 
 p12 = ggplot(taxitrain,aes(x = hour(pickup_datetime)))+
   geom_histogram() +
   scale_x_continuous(breaks=1:24)
 
 grid.arrange(p10,p11,p12)
 
 
 #######################Bivariate##################
 
 bp1 = ggplot(taxitrain,aes(x = vendor_id,y = trip_duration)) +
   stat_summary(fun.y = "mean",geom = "bar")
 
bp2 =  ggplot(taxitrain,aes(x = hour(pickup_datetime),y = trip_duration)) +
   stat_summary(fun.y = "median",geom = "line",col = "red") +
   scale_x_continuous(breaks=1:24) +
  theme(axis.text.x = element_text(size = 8))
 
bp3 =  ggplot(taxitrain,aes(x = day(pickup_datetime),y = trip_duration)) +
   stat_summary(fun.y = "median",geom = "line",col = "red") +
   scale_x_continuous(breaks=1:31) + 
  theme(axis.line.x = element_text(size = 8.5))
 bp4 = ggplot(taxitrain,aes(x = month(pickup_datetime),y = trip_duration)) +
   stat_summary(fun.y = "median",geom = "line",col = "red") +
   scale_x_continuous(breaks=1:24)
 bp5 = ggplot(taxitrain,aes(x = wday(pickup_datetime),y = trip_duration)) +
   stat_summary(fun.y = "median",geom = "line",fill = "red") +
   scale_x_continuous(breaks=1:24)
 
 grid.arrange(bp1,bp2,bp3,bp4,bp5)

  ------------------------------------------------------------------
 ##############Combing data & Creating  Features##############

taxicom$hour_pickup = hour(taxicom$pickup_datetime)
taxicom$month_pickup = month(taxicom$pickup_datetime)
taxicom$day_pickup = day(taxicom$pickup_datetime)
taxicom$wday_pickup = wday(taxicom$pickup_datetime)

taxicom$passenger_count  = as.numeric(taxicom$passenger_count)
taxicom$store_and_fwd_flag = ifelse(taxicom$store_and_fwd_flag == "Y",1,0)
taxicom$vendor_id = as.numeric(taxicom$vendor_id)
names(taxicom)

taxida = taxicom[ ,-c(1,3,4)]

###################Splitting ####

traint = taxida[0:1458644, ]
testt  = taxida[1458645:2083778, ]


###############Building  Model####################

dtrain = xgb.DMatrix(data = as.matrix(traint[ ,-11]),label = as.matrix(traint$trip_duration))
dtest = xgb.DMatrix(data = as.matrix(testt[ ,-11]),label = as.matrix(testt$trip_duration))

##dataset rmse is evaluated at each iteration

watchlist = list(train=dtrain, test=dtest)

--------------------------------------------------------------------
  ########XGBOOST - CARET######## 0.49788#

fitControl <- trainControl(method="cv",number = 5,verboseIter = T)
xgbGrid <- expand.grid(nrounds = 100,
                       max_depth = 10,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .8,
                       min_child_weight = 1,
                       subsample = 1)


set.seed(13)
nycDataXGB = train(trip_duration ~ ., data = traint,
                   method = "xgbTree",trControl = fitControl,
                   tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")
nycDataXGB$modelInfo
summary(nycDataXGB)
plot(nycDataXGB)
pred2 = predict(nycDataXGB,testt)
sub1 = data.frame(id = taxitest$id,trip_duration = exp(pred2)-1)
write.csv(sub1,"xgbsub2.csv",row.names = F)



####################Feature Engineering###############

str(taxicom$pick)


taxicom$longdiff = taxicom$pickup_longitude - taxicom$dropoff_longitude
taxicom$latdiff  = taxicom$pickup_latitude - taxicom$dropoff_latitude

taxicom$min_pickup =min(taxicom$pickup_datetime)

names(taxicom)

taxida = taxicom[ ,-c(1,3,4)]
names(taxida)
###################Splitting ####

traint = taxida[0:1458644, ]
testt  = taxida[1458645:2083778, ]

names(traint)
###############Building  Model####################

dtrain = xgb.DMatrix(data = as.matrix(traint[ ,-8]),label = as.matrix(traint$trip_duration))
dtest = xgb.DMatrix(data = as.matrix(testt[ ,-8]),label = as.matrix(testt$trip_duration))

##dataset rmse is evaluated at each iteration

watchlist = list(train=dtrain, test=dtest)

--------------------------------------------------------------------
  ########XGBOOST - CARET######## 0.44#
  
fitControl <- trainControl(method="cv",number = 5,verboseIter = T)
xgbGrid <- expand.grid(
                       max_depth = c(3,5,7,9,12,15,17,25),
                       eta = C(0.01,0.015,0.025,0.05,0.1),
                       gamma = c(.05,0.1,0.3,0.5,0.7,0.9,1.0),
                       colsample_bytree = c(0.6,0.7,.8,0.9,1.0),
                       min_child_weight = c(1,3,5,7),
                       subsample = c(0.6,0.7,.8,0.9,1.0))


set.seed(14)
nycDataXGB = train(trip_duration ~ ., data = traint,
                   method = "xgbTree",trControl = fitControl,
                   tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")

nycDataXGB$results
nycDataXGB$modelInfo
summary(nycDataXGB)
plot(nycDataXGB)
pred3 = predict(nycDataXGB,testt)
sub2 = data.frame(id = taxitest$id,trip_duration = exp(pred3)-1)
write.csv(sub2,"xgbsubfeatues.csv",row.names = F)


tr1 = taxicom %>% select(id, pickup_longitude, pickup_latitude)%>% data.frame
tr2 = taxicom %>% select(id,dropoff_latitude,dropoff_longitude) %>% data.frame


coordinates(tr1) <- c("pickup_longitude", "pickup_latitude")
proj4string(tr1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
tr1 <- spTransform(tr1, CRS("+proj=longlat +datum=WGS84"))
coordinates(tr2) <- c("dropoff_longitude", "dropoff_latitude")
proj4string(tr2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
tr2 <- spTransform(tr2, CRS("+proj=longlat +datum=WGS84"))


taxicom$distance <- distCosine(tr1, tr2)

taxicom$haversine <- distHaversine(tr1, tr2)

taxicom$bearing <- bearing(tr1, tr2)


########3Modelling -Including new features -0.44########
names(taxicom)
taxida = taxicom[ ,-c(1,3,4)]

###################Splitting ####

traint = taxida[0:1458644, ]
testt  = taxida[1458645:2083778, ]

fitControl <- trainControl(method="cv",number = 5,verboseIter = T)
xgbGrid <- expand.grid(nrounds = 100,
                       max_depth = 10,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .8,
                       min_child_weight = 1,
                       subsample = 1)


set.seed(14)
nycDataXGB = train(trip_duration ~ ., data = traint,
                   method = "xgbTree",trControl = fitControl,
                   tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")

nycDataXGB$results
nycDataXGB$modelInfo
summary(nycDataXGB)
plot(nycDataXGB)
pred4 = predict(nycDataXGB,testt)
sub3 = data.frame(id = taxitest$id,trip_duration = exp(pred4)-1)
write.csv(sub3,"xgballfeatues.csv",row.names = F)


