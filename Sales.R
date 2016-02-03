# current score of 1143.41302088 on leaderboard Rank 3

---
author: "Neeraj"
output: html_document
---

```{r}
options(scipen = 999)
#library("caret")
library('mlbench')
library('pROC')
library(varhandle)
train <-read.csv("train.csv")
test <- read.csv("test.csv")


train$Outlet_Size[train$Outlet_Type=="Grocery Store"]<-"Small"
train$Outlet_Size[train$Outlet_Type=="Supermarket Type1" & train$Outlet_Location_Type=="Tier 2"]<-"Small"

test$Outlet_Size[test$Outlet_Type=="Grocery Store"]<-"Small"
test$Outlet_Size[test$Outlet_Type=="Supermarket Type1" & test$Outlet_Location_Type=="Tier 2"]<-"Small"

# converting missing values to -1
train[is.na(train)] <- -1
test[is.na(test)] <- -1

#removing outliers
train <- train[train$Item_Visibility > 0.00000,]


##some feature engineering.
##for train.

train$outlet <- 1
train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Supersmalltier1"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Supersmalltier2"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Supersmalltier3"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Supermediumtier1"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Supermediumtier2"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Supermediumtier3"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Superhightier1"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Superhightier2"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type1"] <- "Superhightier3"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Supersmalltype2tier1"
train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Supersmalltype2tier2"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Supersmalltype2tier3"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Supermediumtype2tier1"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Supermediumtype2tier2"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Supermediumtype2tier3"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Superhightype2tier1"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Superhightype2tier2"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type2"] <- "Superhightype2tier3"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Supersmalltype3tier1"
train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Supersmalltype3tier2"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Supersmalltype3tier3"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Supermediumtype3tier1"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Supermediumtype3tier2"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Supermediumtype3tier3"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Superhightype3tier1"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Superhightype3tier2"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Supermarket Type3"] <- "Superhightype3tier3"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Grocery Store"] <- "GrocerysmallTier1"
train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Grocery Store"] <- "Grocerysmalltier2"

train$outlet[train$Outlet_Size=="Small" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Grocery Store"] <- "Grocerysmalltier3"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Grocery Store"] <- "Grocerymediumtier1"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "Grocery Store"] <- "Grocerymediumtier2"

train$outlet[train$Outlet_Size=="Medium" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Grocery Store"] <- "Grocerymediumtier3"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 1" & 
                  train$Outlet_Type == "Grocery Store"] <- "Groceryhightier1"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 2" & 
                  train$Outlet_Type == "SGrocery Store"] <- "Groceryhightier2"

train$outlet[train$Outlet_Size=="High" & train$Outlet_Location_Type=="Tier 3" & 
                  train$Outlet_Type == "Grocery Store"] <- "Groceryhightier3"


##for test
test$outlet <- 1
test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Supersmalltier1"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Supersmalltier2"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Supersmalltier3"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Supermediumtier1"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Supermediumtier2"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Supermediumtier3"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Superhightier1"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Superhightier2"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type1"] <- "Superhightier3"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Supersmalltype2tier1"
test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Supersmalltype2tier2"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Supersmalltype2tier3"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Supermediumtype2tier1"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Supermediumtype2tier2"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Supermediumtype2tier3"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Superhightype2tier1"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Superhightype2tier2"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type2"] <- "Superhightype2tier3"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Supersmalltype3tier1"
test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Supersmalltype3tier2"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Supersmalltype3tier3"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Supermediumtype3tier1"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Supermediumtype3tier2"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Supermediumtype3tier3"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Superhightype3tier1"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Superhightype3tier2"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Supermarket Type3"] <- "Superhightype3tier3"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Grocery Store"] <- "GrocerysmallTier1"
test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Grocery Store"] <- "Grocerysmalltier2"

test$outlet[test$Outlet_Size=="Small" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Grocery Store"] <- "Grocerysmalltier3"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Grocery Store"] <- "Grocerymediumtier1"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "Grocery Store"] <- "Grocerymediumtier2"

test$outlet[test$Outlet_Size=="Medium" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Grocery Store"] <- "Grocerymediumtier3"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 1" & 
               test$Outlet_Type == "Grocery Store"] <- "Groceryhightier1"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 2" & 
               test$Outlet_Type == "SGrocery Store"] <- "Groceryhightier2"

test$outlet[test$Outlet_Size=="High" & test$Outlet_Location_Type=="Tier 3" & 
               test$Outlet_Type == "Grocery Store"] <- "Groceryhightier3"



##further dicephering.
#for train.
table(train$Outlet_Establishment_Year, train$outlet)
train$OutletYear <- 1
train$OutletYear[train$outlet=="GrocerysmallTier1" & train$Outlet_Establishment_Year==1985] <- "GrocerysmallTier11985"
train$OutletYear[train$outlet=="Grocerysmalltier3" & train$Outlet_Establishment_Year==1998] <-"Grocerysmalltier31998"
train$OutletYear[train$outlet=="Superhightier3" & train$Outlet_Establishment_Year==1987] <-"Superhightier31987"
train$OutletYear[train$outlet=="Supermediumtier1" & train$Outlet_Establishment_Year==1999] <-"Supermediumtier11999"
train$OutletYear[train$outlet=="Supermediumtype2tier3" & train$Outlet_Establishment_Year==2009] <-"Supermediumtype2tier32009"
train$OutletYear[train$outlet=="Supermediumtype3tier3" & train$Outlet_Establishment_Year==1985] <-"Supermediumtype3tier31985"
train$OutletYear[train$outlet=="Supersmalltier1" & train$Outlet_Establishment_Year==1997]<-"Supersmalltier11997"

train$OutletYear[train$outlet=="Supersmalltier2" & train$Outlet_Establishment_Year %in% c(2002,2004,2007)]<-"Supersmalltier2"


# for test.
test$OutletYear <- 1
test$OutletYear[test$outlet=="GrocerysmallTier1" & test$Outlet_Establishment_Year==1985] <- "GrocerysmallTier11985"
test$OutletYear[test$outlet=="Grocerysmalltier3" & test$Outlet_Establishment_Year==1998] <-"Grocerysmalltier31998"
test$OutletYear[test$outlet=="Superhightier3" & test$Outlet_Establishment_Year==1987] <-"Superhightier31987"
test$OutletYear[test$outlet=="Supermediumtier1" & test$Outlet_Establishment_Year==1999] <-"Supermediumtier11999"
test$OutletYear[test$outlet=="Supermediumtype2tier3" & test$Outlet_Establishment_Year==2009] <-"Supermediumtype2tier32009"
test$OutletYear[test$outlet=="Supermediumtype3tier3" & test$Outlet_Establishment_Year==1985] <-"Supermediumtype3tier31985"
test$OutletYear[test$outlet=="Supersmalltier1" & test$Outlet_Establishment_Year==1997]<-"Supersmalltier11997"

test$OutletYear[test$outlet=="Supersmalltier2" & test$Outlet_Establishment_Year %in% c(2002,2004,2007)]<-"Supersmalltier2"

##further deeper.

#for train.
train$Outident <- 1
train$Outident[train$OutletYear=="GrocerysmallTier11985" &
                 train$Outlet_Identifier=="OUT019"] <-"G19"

train$Outident[train$OutletYear=="Grocerysmalltier31998" &
                 train$Outlet_Identifier=="OUT010"] <-"G10"

train$Outident[train$OutletYear=="Superhightier31987" &
                 train$Outlet_Identifier=="OUT013"] <-"SH13"
train$Outident[train$OutletYear=="Supermediumtier11999" &
                 train$Outlet_Identifier=="OUT049"] <-"SM49"
train$Outident[train$OutletYear=="Supermediumtype2tier32009" &
                 train$Outlet_Identifier=="OUT018"] <-"SMT218"
train$Outident[train$OutletYear=="Supermediumtype3tier31985" &
                 train$Outlet_Identifier=="OUT027"] <-"SMT327"
train$Outident[train$OutletYear=="Supersmalltier11997" &
                 train$Outlet_Identifier=="OUT046"] <-"SS46"
train$Outident[train$OutletYear=="Supersmalltier2" &
                 train$Outlet_Identifier %in% c( "OUT017", "OUT035", "OUT045")] <-"SST2"
                 
train$Outident <- as.factor(train$Outident)

##for test.
test$Outident <- 1
test$Outident[test$OutletYear=="GrocerysmallTier11985" &
                 test$Outlet_Identifier=="OUT019"] <-"G19"

test$Outident[test$OutletYear=="Grocerysmalltier31998" &
                 test$Outlet_Identifier=="OUT010"] <-"G10"
test$Outident[test$OutletYear=="Superhightier31987" &
                 test$Outlet_Identifier=="OUT013"] <-"SH13"
test$Outident[test$OutletYear=="Supermediumtier11999" &
                 test$Outlet_Identifier=="OUT049"] <-"SM49"
test$Outident[test$OutletYear=="Supermediumtype2tier32009" &
                 test$Outlet_Identifier=="OUT018"] <-"SMT218"
test$Outident[test$OutletYear=="Supermediumtype3tier31985" &
                 test$Outlet_Identifier=="OUT027"] <-"SMT327"
test$Outident[test$OutletYear=="Supersmalltier11997" &
                 test$Outlet_Identifier=="OUT046"] <-"SS46"
test$Outident[test$OutletYear=="Supersmalltier2" &
                 test$Outlet_Identifier %in% c( "OUT017", "OUT035", "OUT045")] <-"SST2"

test$Outident <- as.factor(test$Outident)


levels(train$Item_Fat_Content)[levels(train$Item_Fat_Content)%in%c("Low Fat","LF","low fat")] <- "1"
levels(train$Item_Fat_Content)[levels(train$Item_Fat_Content)%in%c("Regular","reg")] <- "2"

#for test
levels(test$Item_Fat_Content)[levels(test$Item_Fat_Content)%in%c("Low Fat","LF","low fat")] <- "1"
levels(test$Item_Fat_Content)[levels(test$Item_Fat_Content)%in%c("Regular","reg")] <- "2"

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
