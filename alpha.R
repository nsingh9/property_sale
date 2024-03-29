#https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data

#####

rm(list = ls())

####### Librarys #######

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(e1071)) install.packages("e1071")
if(!require(caret)) install.packages("caret")
if(!require(knitr)) install.packages("knitr")
if(!require(doSNOW)) install.packages("doSNOW")
if(!require(glmnet)) install.packages("glmnet")
if(!require(scales)) install.packages("scales")
if(!require(Matrix)) install.packages("Matrix")
if(!require(Rborist)) install.packages("Rborist")
if(!require(gganimate)) install.packages("gganimate")
if(!require(fastDummies)) install.packages("fastDummies")
if(!require(RColorBrewer)) install.packages("RColorBrewer")


####### Data Loading #######

block_train <- read.csv("train.csv", header = TRUE, sep = ",")
block_test <- read.csv("test.csv", header = TRUE, sep = ",")

dim(block_train)
dim(block_test)

#will save row number which will be used to split train and test later
break.point <- dim(block_train)

sale_price <- block_train$SalePrice

sale_price <- log(sale_price)

block_train$SalePrice <- NULL

temp <- rbind(block_train, block_test)

#change interger to numeric
vr <- which(sapply(temp, is.numeric))

temp[vr] <- lapply(temp[vr], as.numeric)

dim(temp)
str(temp)
#head(temp)

sapply(temp, class)

which(sapply(temp, is.numeric))
#get rid of ID

which(sapply(temp, is.factor))

#summary(temp)

##### to null id temp <- rbind(block_test, block_train) ####

temp$Id <- NULL

#also check our replace_na & coalesce

na.col <- which(colSums(is.na(temp)) > 0)
colSums(sapply(temp[na.col], is.na))
sapply(temp[na.col], class)
cat('There are', length(na.col), 'columns with missing values')

#Replace missing Manvnrtype value with most occuring value
#replace missing value of integer column with 0
#add level "None in factor columns
#Garage column without year build can be replaced with the year house was built

temp$GarageYrBlt[is.na(temp$GarageYrBlt)]
temp$GarageYrBlt[is.na(temp$GarageYrBlt)] = temp$YearBuilt[is.na(temp$GarageYrBlt)]

##### try to create a funtion ##### 

add.mode <- function(x){
  val <- sort(summary(x), decreasing = TRUE)
  a <- which(is.na(x))
  x[a] <- c(names(val[1]))
}

replace.mode.val <- temp[, c("MSZoning", "Utilities", "MasVnrType", 
                             "Electrical", "SaleType", "Exterior1st", 
                             "Exterior2nd", "Functional")]

temp[, c("MSZoning", "Utilities", "MasVnrType", "Electrical", "SaleType", 
         "Exterior1st", "Exterior2nd", 
         "Functional")] <- lapply(replace.mode.val, add.mode)



#replacting NA with 0 in MasVnrArea & LotFrontage
add.zero <- function(x){
  if (is.numeric(x)){
    x[is.na(x)] <- 0
    x
  }
}

replace.zero.val <- temp[ ,c("LotFrontage", "MasVnrArea", "BsmtFinSF1", 
                             "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", 
                             "BsmtFullBath", "BsmtHalfBath", "GarageCars", 
                             "GarageArea")]

temp[ ,c("LotFrontage", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", 
         "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", 
         "GarageCars", "GarageArea")] <- lapply(replace.zero.val, add.zero)

#replacing NA in factor columns with "None" except Electrical
add.none <- function(x){
  if (is.factor(x)){
    fct_explicit_na(x, "None")
  }
}

replace.none.val <- temp[ ,c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", 
                             "BsmtFinType1", "BsmtFinType2", "FireplaceQu", 
                             "KitchenQual", "GarageType", "GarageFinish", 
                             "GarageQual", "GarageCond", "PoolQC", "Fence", 
                             "MiscFeature")]

temp[ ,c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", 
         "BsmtFinType2", "FireplaceQu", "KitchenQual", "GarageType", 
         "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", 
         "MiscFeature")] <- as.data.frame(lapply(replace.none.val, add.none))

#checking for na in dataset
which(is.na(temp))

#set up factor
#switch.class <- c('MoSold', 'YrSold', 'YearBuilt', 'YearRemodAdd', 
#                   'MSSubClass', 'MasVnrType')

#switch.class<-lapply(switch.class, as.factor)

#assign values to quality columns

to.change <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

assign.val <- c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", 
                "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", 
                "GarageCond", "PoolQC")

assign.val <- select(temp, assign.val)

assign.val.df <- as.data.frame(
  lapply(assign.val, function(x) {
    recode(x, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 
                  'Gd' = 4, 'Ex' = 5)
  }))

temp[ ,c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC", 
         "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", 
         "PoolQC")] <- assign.val.df[ ,c("ExterQual", "ExterCond", "BsmtQual", 
                                         "BsmtCond", "HeatingQC", "KitchenQual", 
                                         "FireplaceQu", "GarageQual", 
                                         "GarageCond", "PoolQC")]


####### Data Engineering #######

temp <- temp %>% mutate(TotalBath = FullBath + 
                          HalfBath*0.5 + BsmtFullBath + BsmtHalfBath*0.5)

temp <- temp %>% mutate(TotalSqFeet = GrLivArea + TotalBsmtSF)

temp <- temp %>% mutate(TotalPorchSF = OpenPorchSF + 
                          EnclosedPorch + X3SsnPorch + ScreenPorch)

temp <- temp %>% mutate(Remodel = ifelse((temp$YearBuilt) == (temp$YearRemodAdd), 0, 1))
temp$Remodel <- as.factor(temp$Remodel)

temp <- temp %>% mutate(IsNew = ifelse(as.numeric(temp$YrSold) == as.numeric(temp$YearBuilt), 1, 0))
temp$IsNew <- as.factor(temp$IsNew)

####### Spliting Test and Train for Visualisation ####### 

a <- split.data.frame(temp, cumsum(1:nrow(temp) %in% (break.point[1]+1)))

a_train <- a[[1]]

a_test <- a[[2]]

a_train$SalePrice <- sale_price

####### Visualisation #######

summary(a_train$SalePrice)


summary(a_train$YearBuilt)

#graph for year built and sale price
p.1 <- a_train %>% 
  ggplot(aes(YearBuilt, SalePrice, colour = SalePrice)) +
  geom_smooth(method = lm) +
  geom_point() +
  scale_y_log10(labels = dollar) +
  labs(y = "Sale Price",
       x = "Year Built") +
  theme_bw()


summary(a_train$YrSold)

#grpah for year sold and sale price
p.2 <- a_train %>% 
  ggplot(aes(YrSold, SalePrice)) +
  geom_smooth(method = lm) +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x =  "Year Sold") +
  theme_bw()

#box plot of p.2
p.3 <- a_train %>% 
  ggplot(aes(YrSold, SalePrice, group=YrSold)) +
  geom_smooth(method = lm) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x = "Year Sold") +
  theme_bw()


summary(a_train$LotArea)

#graph for lot size and sale price
p.4 <- a_train %>% 
  #filter(LotArea<50000) %>% 
  #use log for lot area
  ggplot(aes(LotArea, SalePrice, colour = SalePrice)) +
  geom_smooth(method = lm) +
  geom_point() +
  scale_y_log10(labels = dollar) +
  scale_x_log10(labels = number) +
  labs(y = "Sale Price",
       x = "Area of Lot") +
  #scale_colour_hue() +
  theme_bw() #+
  #scale_colour_gradient(low = "dark", high = "white")


summary(a_train$MSZoning)

#box plot for zone and price
p.5 <- a_train %>% 
  ggplot(aes(MSZoning, SalePrice)) +
  geom_smooth(method = lm) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x = "Zoning Category") +
  theme_bw()


summary(a_train$Neighborhood)

#box plot for neighborhood and price
p.6 <- a_train %>% 
  ggplot(aes(Neighborhood, SalePrice)) +
  geom_smooth(method = lm) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x = "Neighbourhood") +
  theme_bw()


summary(a_train$MoSold)

#bar plot for property sold everymonth
p.7 <- a_train %>%
  mutate(months = factor(a_train$MoSold, 
                         labels = month.abb[1:12])) %>% 
  group_by(months) %>% 
  summarise(count = n(), average = mean(SalePrice)) %>% 
  ggplot() +
  geom_bar(aes(x = months, y = count, fill = average),
           stat = "identity") +
  scale_y_continuous(labels = number) +
  #scale_x_date(date_format("%B")) +
  xlab("Month") +
  #scale_x_date(labels = date_format(a_train$MoSold)) +
  theme_bw()

a_train %>% 
  group_by(MoSold) %>% 
  summarise(count = n(), average = mean(SalePrice)) %>% 
  arrange(desc(count, average))


#graph for house style and price for "1Story", "2Story", "1.5Fin"
a_train %>% 
  group_by(HouseStyle) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

first.map <- subset(a_train, 
                    a_train$HouseStyle %in% c("1Story", "2Story", "1.5Fin"))

second.map <- subset(a_train, 
                     a_train$HouseStyle %in% c("SLvl", "SFoyer", "1.5Unf", 
                                            "2.5Unf", "2.5Fin"))

p.8 <- first.map %>% 
  ggplot(aes(YearBuilt, SalePrice, colour = HouseStyle)) +
  geom_point() +
  geom_smooth(method = lm, data = first.map, se = FALSE) +
  scale_y_log10(labels = dollar) +
  labs(y = "Sale Price",
       x = "Year Built") +
  theme_bw()

#graph for house style and price for "SLvl", "SFoyer", "1.5Unf", #"2.5Unf", "2.5Fin"
p.9 <- second.map %>% 
  ggplot(aes(YearBuilt, SalePrice, colour = HouseStyle)) +
  geom_point() +
  geom_smooth(method = lm, data = second.map, se = FALSE) +
  scale_y_log10(labels = dollar) +
  labs(y = "Sale Price",
       x = "Year Built") +
  theme_bw()

a_train %>% 
  group_by(Neighborhood) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  kable()

rm(replace.mode.val,replace.none.val, replace.zero.val, assign.val,
   assign.val.df, a)


#changing to factor

temp$MoSold <- as.factor(temp$MoSold)
temp$YrSold <- as.factor(temp$YrSold)
temp$YearBuilt <- as.factor(temp$YearBuilt)
temp$YearRemodAdd <- as.factor(temp$YearRemodAdd)
temp$MSSubClass <- as.factor(temp$MSSubClass)
temp$MasVnrType <- as.factor(temp$MasVnrType)


####### Converting factor variables into numeric #######

#use fastDummy's dummy_col (because we can't use factor)

dum_number <- dummy_cols(temp)

####### Spliting Test and Train for model training ####### 

a <- split.data.frame(dum_number, cumsum(1:nrow(temp) %in% (break.point[1]+1)))

a_train <- a[[1]]

a_test <- a[[2]]

####### Seperating numeric data set form factor data set for model training ####### 

vr <- which(sapply(a_train, is.numeric))

train_set <- a_train[vr]

####### Data Analysis #######

####### ** Methods #######


####### Ridge ####### 

#will use thi control setup for all caret trian function
control <- trainControl(method = "cv", number = 5)

grid_ridge <- expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.0005))

fit_ridge <- train(x = train_set, 
               y = sale_price, 
               method = "glmnet", 
               trControl = control, 
               tuneGrid = grid_ridge)

fit_ridge$bestTune
min(fit_ridge$results$RMSE)

ridge_predict <- predict(fit_ridge, a_test)

rmse_1 <- min(fit_ridge$results$RMSE)

rmse_results <- data.frame(method = "Ridge Regression", RMSE = rmse_1,
                           stringsAsFactors = FALSE)


####### Lasso ####### 

grid_lasso <- expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0005))

fit_lasso <- train(x = train_set, 
               y = sale_price, 
               method = "glmnet", 
               trControl = control, 
               tuneGrid = grid_lasso)

fit_lasso$bestTune
min(fit_lasso$results$RMSE)

lasso_predict <- predict(fit_lasso, a_test)

rmse_2 <- min(fit_lasso$results$RMSE)

bind_rows(rmse_results,
          data.frame(method="Lasso Regression", RMSE = rmse_2, 
                     stringsAsFactors = FALSE))

####### xgboost #######

grid_xgb <- expand.grid(number = 1000,
                        eta = c(0.1, 0.05, 0.01),
                        max_depth = c(6, 7, 8, 9),
                        colsample_bytree = c(0.3, 0.4, 0.5),
                        min_child_weight = c(1, 2, 3, 4 ,5),
                        subsample = 1,
                        gamma = 0
)

cl <- makeCluster(4, type = "SOCK")

registerDoSNOW(cl)

fit_xgb <- train(x = train_set,
                 y = sale_price,
                 method = "xgbTree",
                 trControl = control,
                 tuneGrid = grid_xgb)

stopCluster(cl)

fit_xgb$bestTune





