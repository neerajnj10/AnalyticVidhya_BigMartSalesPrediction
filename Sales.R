# current score of 1159.4847738 on leaderboard with R2 of ~0.644

```{r}
options(scipen = 999)
#library("caret")
library('mlbench')
library('pROC')
library(varhandle)

train <-read.csv("train.csv")
test <- read.csv("test.csv")
# for train
levels(train$Item_Fat_Content)[levels(train$Item_Fat_Content)%in%c("Low Fat","LF","low fat")] <- "1"
levels(train$Item_Fat_Content)[levels(train$Item_Fat_Content)%in%c("Regular","reg")] <- "2"

levels(train$Outlet_Location_Type)[levels(train$Outlet_Location_Type)%in%c("Tier 1")] <- "1"
levels(train$Outlet_Location_Type)[levels(train$Outlet_Location_Type)%in%c("Tier 2")] <- "2"
levels(train$Outlet_Location_Type)[levels(train$Outlet_Location_Type)%in%c("Tier 3")] <- "3"



levels(train$Outlet_Type)[(levels(train$Outlet_Type) %in% c("Grocery Store"))] <- "1"
levels(train$Outlet_Type)[(levels(train$Outlet_Type) %in% c("Supermarket Type1"))] <- "2"
levels(train$Outlet_Type)[(levels(train$Outlet_Type) %in% c("Supermarket Type2"))] <- "3"
levels(train$Outlet_Type)[(levels(train$Outlet_Type) %in% c("Supermarket Type3"))] <- "4"

levels(train$Outlet_Size)[(levels(train$Outlet_Size) %in% c(""))] <- ""
levels(train$Outlet_Size)[(levels(train$Outlet_Size) %in% c("High"))] <- "3"
levels(train$Outlet_Size)[(levels(train$Outlet_Size) %in% c("Medium"))] <- "2"
levels(train$Outlet_Size)[(levels(train$Outlet_Size) %in% c("Small"))] <- "1"

#missing data
#library(rpart)
#fit.weight<-rpart(train$Item_Weight[!is.na(train$Item_Weight)]~Item_Identifier+Item_Fat_Content+Item_Visibility+Item#_Type+Item_MRP,data=train[!is.na(train$Item_Weight),],method='anova')
#train$Item_Weight[is.na(train$Item_Weight)]<-predict(fit.weight,train[is.na(train$Item_Weight),])

train$Item_Weight[is.na(train$Item_Weight)] <- mean(train$Item_Weight, na.rm=TRUE)
#train$Item_Visibility <- (train$Item_Visibility)/100
#train$Item_Weight <- round(train$Item_Weight, digits = 2)

#for test
levels(test$Item_Fat_Content)[levels(test$Item_Fat_Content)%in%c("Low Fat","LF","low fat")] <- "1"
levels(test$Item_Fat_Content)[levels(test$Item_Fat_Content)%in%c("Regular","reg")] <- "2"

levels(test$Outlet_Location_Type)[levels(test$Outlet_Location_Type)%in%c("Tier 1")] <- "1"
levels(test$Outlet_Location_Type)[levels(test$Outlet_Location_Type)%in%c("Tier 2")] <- "2"
levels(test$Outlet_Location_Type)[levels(test$Outlet_Location_Type)%in%c("Tier 3")] <- "3"



levels(test$Outlet_Type)[(levels(test$Outlet_Type) %in% c("Grocery Store"))] <- "1"
levels(test$Outlet_Type)[(levels(test$Outlet_Type) %in% c("Supermarket Type1"))] <- "2"
levels(test$Outlet_Type)[(levels(test$Outlet_Type) %in% c("Supermarket Type2"))] <- "3"
levels(test$Outlet_Type)[(levels(test$Outlet_Type) %in% c("Supermarket Type3"))] <- "4"

levels(test$Outlet_Size)[(levels(test$Outlet_Size) %in% c(""))] <- ""
levels(test$Outlet_Size)[(levels(test$Outlet_Size) %in% c("High"))] <- "3"
levels(test$Outlet_Size)[(levels(test$Outlet_Size) %in% c("Medium"))] <- "2"
levels(test$Outlet_Size)[(levels(test$Outlet_Size) %in% c("Small"))] <- "1"

#missing data.
#fit.weight1<-rpart(test$Item_Weight[!is.na(test$Item_Weight)]~Item_Identifier+Item_Fat_Content+Item_Visibility+Item_#Type+Item_MRP,data=test[!is.na(test$Item_Weight),],method='anova')
#test$Item_Weight[is.na(test$Item_Weight)]<-predict(fit.weight1,test[is.na(test$Item_Weight),])

test$Item_Weight[is.na(test$Item_Weight)] <- mean(test$Item_Weight, na.rm=TRUE)
#test$Item_Visibility <-(test$Item_Visibility)/100
#test$Item_Weight <- round(test$Item_Weight, digits = 2)
```


```{r}
library(h2o)
train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

dl <- h2o.deeplearning(
  x=c(1:11),
  y=12, 
  training_frame=train.hex,
  hidden=c(6),
  epochs=60,
  nfolds=5,
  activation = "Maxout",
  use_all_factor_levels = T,
  variable_importances = T,
  adaptive_rate = T,
  input_dropout_ratio = 0.2,
  loss = "Automatic",
  score_training_samples = 0,
  stopping_rounds = 5,
  epsilon = 1e-8,
  rho = 0.99,
  initial_weight_distribution = "UniformAdaptive",
  single_node_mode = T,
  force_load_balance = T,
  fast_mode = T,
  diagnostics = T,
  fold_assignment= "Modulo" 
  # can be "AUTO", "Modulo", "Random" or "Stratified"
  )

sol <- as.data.frame(h2o.predict(dl, test.hex))
my <-  test[, c("Item_Identifier", "Outlet_Identifier")]
my <- data.frame(my, sol$predict)
colnames(my) <- c("Item_Identifier", "Outlet_Identifier","Item_Outlet_Sales")
write.csv(my, "firstsubmit.csv", row.names = F)
```
