getwd()
setwd("E:/Project/property_sale")

####### Librarys #######

library(tidyverse)
library(e1071)
library(caret)
library(knitr)
library(scales)
library(Matrix)
library(Rborist)
library(gganimate)
library(RColorBrewer)

####### Data Loading #######

temp <- read.csv("train.csv", header = TRUE, sep = ",")

dim(temp)
str(temp)
#head(temp)

sapply(temp, class)

which(sapply(temp, is.numeric))
#get rid of ID

which(sapply(temp, is.factor))

#summary(temp)

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

#Now for MasVnrType
sort(summary(temp$MasVnrType),decreasing = TRUE)
#by looking at data we can see None is mode
a <- which(is.na(temp$MasVnrType))
temp$MasVnrType[a] <- c("None")

#replacing Electrical Na to mode
sort(summary(temp$Electrical),decreasing = TRUE)
#SBrkr is most ocurring value in Electrical
a <- which(is.na(temp$Electrical))
temp$Electrical[a] <- c("SBrkr")

#replacting NA with 0 in MasVnrArea & LotFrontage
add.zero <- function(x){
  if (is.numeric(x)){
    x[is.na(x)] <- 0
    x
  }
}

replace.zero.val <- temp[ ,c("LotFrontage", "MasVnrArea")]

temp[,c("LotFrontage", "MasVnrArea")] <- lapply(replace.zero.val, add.zero)

#replacing NA in factor columns with "None" except Electrical
add.none <- function(x){
  if (is.factor(x)){
    fct_explicit_na(x, "None")
  }
}

replace.none.val <- temp[ ,c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", 
                             "BsmtFinType1", "BsmtFinType2", "FireplaceQu", 
                             "GarageType", "GarageFinish", "GarageQual", 
                             "GarageCond", "PoolQC", "Fence", "MiscFeature")]

temp[ ,c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", 
         "BsmtFinType2", "FireplaceQu", "GarageType", "GarageFinish", 
         "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature")] <- as.data.frame(lapply(replace.none.val, add.none))

#checking for na in dataset
which(is.na(temp))

#set up factor
#switch.class <- c('MoSold', 'YrSold', 'YearBuilt', 'YearRemodAdd', 
#                   'MSSubClass', 'MasVnrType')

#switch.class<-lapply(switch.class, as.factor)

#assign values to quality columns

to.change <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

assign.val <- c('ExterQual', 'ExterCond', 'BsmtQual', 'BsmtCond', 
                'HeatingQC', 'KitchenQual', 'FireplaceQu', 'GarageQual', 
                'GarageCond', 'PoolQC')

assign.val <- select(temp, assign.val)

assign.val.df <- as.data.frame(
  lapply(assign.val, function(x) {
    recode(x, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 
                  'Gd' = 4, 'Ex' = 5)
  }))

temp[ ,c('ExterQual', 'ExterCond', 'BsmtQual', 'BsmtCond', 'HeatingQC', 
         'KitchenQual', 'FireplaceQu', 'GarageQual', 'GarageCond', 
         'PoolQC')] <- assign.val.df[ ,c('ExterQual', 'ExterCond', 'BsmtQual', 
                                         'BsmtCond', 'HeatingQC', 'KitchenQual', 
                                         'FireplaceQu', 'GarageQual', 'GarageCond', 
                                         'PoolQC')]


####### Data Engineering #######

temp <- temp %>% mutate(TotalBath = FullBath + 
                          HalfBath*0.5 + BsmtFullBath + BsmtHalfBath*0.5)

temp <- temp %>% mutate(TotalSqFeet = GrLivArea + TotalBsmtSF)

temp <- temp %>% mutate(TotalPorchSF = OpenPorchSF + 
                          EnclosedPorch + X3SsnPorch + ScreenPorch)

temp <- temp %>% mutate(Remodel = ifelse((temp$YearBuilt) == (temp$YearRemodAdd), 0, 1))
temp$Remodel <- as.factor(temp$Remodel)

temp <- temp %>% mutate(IsNew = ifelse(as.numeric(temp$YrSold) == as.numeric(temp$YearBuilt), 1, 0))
temp$IsNew <- as.factor(t$IsNew)

####### Visualisation #######

summary(temp$SalePrice)


summary(temp$YearBuilt)

#graph for year built and sale price
p.1 <- temp %>% 
  ggplot(aes(YearBuilt, SalePrice, colour = SalePrice)) +
  geom_smooth(method = lm) +
  geom_point() +
  scale_y_log10(labels = dollar) +
  labs(y = "Sale Price",
       x = "Year Built") +
  theme_bw()


summary(temp$YrSold)

#grpah for year sold and sale price
p.2 <- temp %>% 
  ggplot(aes(YrSold, SalePrice)) +
  geom_smooth(method = lm) +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x =  "Year Sold") +
  theme_bw()

#box plot of p.2
p.3 <- temp %>% 
  ggplot(aes(YrSold, SalePrice, group=YrSold)) +
  geom_smooth(method = lm) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x = "Year Sold") +
  theme_bw()


summary(temp$LotArea)

#graph for lot size and sale price
p.4 <- temp %>% 
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


summary(temp$MSZoning)

#box plot for zone and price
p.5 <- temp %>% 
  ggplot(aes(MSZoning, SalePrice)) +
  geom_smooth(method = lm) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x = "Zoning Category") +
  theme_bw()


summary(temp$Neighborhood)

#box plot for neighborhood and price
p.6 <- temp %>% 
  ggplot(aes(Neighborhood, SalePrice)) +
  geom_smooth(method = lm) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  labs(y = "Sale Price",
       x = "Neighbourhood") +
  theme_bw()


summary(temp$MoSold)

#bar plot for property sold everymonth
p.7 <- temp %>% 
  ggplot(aes(x = (months = factor(temp$MoSold, labels = month.abb[1:12])), 
             fill = SalePrice)) + 
  geom_bar() +
  scale_y_continuous(labels = number) +
  #scale_x_date(date_format("%B")) +
  scale_fill_brewer(aes(start = min(temp$SalePrice), 
                        end = max(temp$SalePrice), 
                        aesthetics = "fill")) +
  xlab("Month") +
  #scale_x_date(labels = date_format(temp$MoSold)) +
  theme_bw()

temp %>% 
  group_by(MoSold) %>% 
  summarise(count = n(), average = mean(SalePrice)) %>% 
  arrange(desc(count, average))


temp %>% 
  group_by(HouseStyle) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

first.map <- subset(temp, 
                    temp$HouseStyle %in% c("1Story", "2Story", "1.5Fin"))

second.map <- subset(temp, 
                     temp$HouseStyle %in% c("SLvl", "SFoyer", "1.5Unf", 
                                            "2.5Unf", "2.5Fin"))
#graph for house style and price for "1Story", "2Story", "1.5Fin"
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

temp %>% 
  group_by(Neighborhood) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  kable()

#changing to factor

temp$MoSold <- as.factor(temp$MoSold)
temp$YrSold <- as.factor(temp$YrSold)
temp$YearBuilt <- as.factor(temp$YearBuilt)
temp$YearRemodAdd <- as.factor(temp$YearRemodAdd)
temp$MSSubClass <- as.factor(temp$MSSubClass)
temp$MasVnrType <- as.factor(temp$MasVnrType)

####### Data Analysis #######

####### ** Methods #######
control <-trainControl(method="cv", number=5)
grid <- expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0005))

train(x = temp, 
      y = temp$SalePrice, 
      method ='glmnet', 
      trControl = control, 
      tuneGrid = grid)

