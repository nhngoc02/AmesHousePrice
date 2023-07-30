# Kaggle project

## Load in useful packages
library(dplyr) 
library(tidyr)
library(ggplot2) 
library(onehot)  
library(MASS)
library(e1071) 
library(car) 
library(broom) 
library(leaps)

## Read in data
train <- read.csv(file = "train.csv")
test <- read.csv(file = "test.csv")

### Combined train and test datasets for initial cleaning process
test$SalePrice <- NA # test dataset does not have the column SalePrice
#train$train <- 1
#test$train <- 0
combined_data <- rbind(train, test)

## 1. Initial data cleaning ----------------------------------------------------
### 1.1 Check if there is any duplicate observations
count(combined_data[duplicated(combined_data) == T, ])
##### There is no duplicates.

### 1.2 Check if there is any observation that has NAs on more than 50% of the columns.
na_counts <- rowSums(is.na(combined_data))
which(na_counts > ncol(combined_data)*0.5)
##### There is no observation that has NAs on more than 50% of its columns.

### 1.3 Function calculate the missing values of a variable
NA_count <- function(df, var){
  return(sum(is.na(df[, var]))) 
}

### 1.4 Get the lists of numeric and categorical variables ------

#### The list of numeric variables
num_vars <- sapply(combined_data, function(x) is.numeric(x) || is.integer(x))
numeric_vars_list <- c(colnames(combined_data[, num_vars]))
numeric_vars_list <- as.vector(numeric_vars_list)
print(numeric_vars_list)
#--- In this list, MSSubClass should be treated as a categorical variable. 
# Because, MSSubClass represents the type of dwelling involved in the sale, 
# and its values do not have a numeric meaning.
numeric_vars_list <- numeric_vars_list[-2]
combined_data$MSSubClass <- as.factor(combined_data$MSSubClass)

#### The list of categorical variables
categ_vars_list <- c(colnames(combined_data[, !num_vars]))
categ_vars_list <- c("MSSubClass", categ_vars_list) # add "MSSubClass" to the list

### 1.5 Identify missing value ----------------------------------
missing_values <- data.frame(cols = colnames(combined_data),
                             count = colSums(is.na(combined_data)))
missing_values_filtered <- missing_values[missing_values$count > 0, ]

#### Columns with missing values: 
#--- MSZoning: 4
combined_data %>% 
  filter(is.na(MSZoning))
##### These observations are in the test dataset.
##### One solution is to replace MSZoning of these observations with the most common 
##### MSZoning values of their Neighborhood types.

##### Find the most common MSZoning class for Neighborhood IDOTRR and Mitchel
names(which.max(table(combined_data %>% filter(Neighborhood == "IDOTRR") %>% pull(MSZoning)))) # error in here
names(which.max(table(combined_data %>% filter(Neighborhood == "Mitchel") %>% pull(MSZoning))))
##### "RM" and "RL", respectively.

##### Replace the NA MSZoning values with corresponding common MSZoning values 
##### of their Neighborhood types
combined_data[combined_data$Id == 1916, "MSZoning"] <- "RM"
combined_data[combined_data$Id == 2217, "MSZoning"] <- "RM"
combined_data[combined_data$Id == 2251, "MSZoning"] <- "RM"
combined_data[combined_data$Id == 2905, "MSZoning"] <- "RL"

#--- LotFrontage: 486
combined_data %>% 
  filter(is.na(LotFrontage))
##### One solution is to impute the means of LotFrontage based on MSZoning to 
##### these NAs.
mean_C <- round(mean(combined_data[combined_data$MSZoning=="C (all)" & !is.na(combined_data$LotFrontage), "LotFrontage"]), 1)
mean_FV <- round(mean(combined_data[combined_data$MSZoning=="FV" & !is.na(combined_data$LotFrontage), "LotFrontage"]), 1)
mean_RH <- round(mean(combined_data[combined_data$MSZoning=="RH" & !is.na(combined_data$LotFrontage), "LotFrontage"]), 1)
mean_RL <- round(mean(combined_data[combined_data$MSZoning=="RL" & !is.na(combined_data$LotFrontage), "LotFrontage"]), 1)
mean_RM <- round(mean(combined_data[combined_data$MSZoning=="RM" & !is.na(combined_data$LotFrontage), "LotFrontage"]), 1)

combined_data[combined_data$MSZoning=="C (all)" & is.na(combined_data$LotFrontage), "LotFrontage"] <- mean_C
combined_data[combined_data$MSZoning=="FV" & is.na(combined_data$LotFrontage), "LotFrontage"] <- mean_FV
combined_data[combined_data$MSZoning=="RH" & is.na(combined_data$LotFrontage), "LotFrontage"] <- mean_RH
combined_data[combined_data$MSZoning=="RL" & is.na(combined_data$LotFrontage), "LotFrontage"] <- mean_RL
combined_data[combined_data$MSZoning=="RM" & is.na(combined_data$LotFrontage), "LotFrontage"] <- mean_RM

combined_data[is.na(combined_data$LotFrontage),]

#--- Alley: 2721

###### There are 2721 missing values over 2919 observations. However, according to 
###### The dataset description, the NA values represent houses with no access to alley.
###### Therefore, these NAs values will be replaced by "No alley access" to avoid confusion.
combined_data[is.na(combined_data$Alley), "Alley"] <- "No alley access"

#--- Utilities: 2
combined_data[is.na(combined_data$Utilities), ]
###### These NAs can be replaced by the most common Utilities types of the Neighborhood
###### these observation belong to.
###### Find the most common Utilities class for Neighborhood IDOTRR and Gilbert
names(which.max(table(combined_data %>% filter(Neighborhood == "IDOTRR") %>% pull(Utilities))))
names(which.max(table(combined_data %>% filter(Neighborhood == "Gilbert") %>% pull(Utilities))))
###### The most frequent Utilities class for both Neighborhood is "AllPub".
combined_data[is.na(combined_data$Utilities), "Utilities"] <- "AllPub"

#--- Exterior1st: 1
combined_data[is.na(combined_data$Exterior1st), ]
###### The NA can be replaced by the most common Exterior1st types of the Neighborhood
###### these observation belong to.
###### Find the most common Exterior1st class for Neighborhood Edwards
names(which.max(table(combined_data %>% filter(Neighborhood == "Edwards") %>% pull(Exterior1st))))
###### The most frequent Exterior1st class for Neighborhood Edwards is "Wd Sdng".
combined_data[is.na(combined_data$Exterior1st), "Exterior1st"] <- "Wd Sdng"

#--- Exterior2nd: 1
combined_data[is.na(combined_data$Exterior2nd), ]
###### The NA can be replaced by the most common Exterior2nd types of the Neighborhood
###### these observation belong to.
###### Find the most common Exterior2nd class for Neighborhood Edwards
names(which.max(table(combined_data %>% filter(Neighborhood == "Edwards") %>% pull(Exterior2nd))))
###### The most frequent Exterior1st class for Neighborhood Edwards is "Wd Sdng".
combined_data[is.na(combined_data$Exterior2nd), "Exterior2nd"] <- "Wd Sdng"

#--- MasVnrType: 24
combined_data[is.na(combined_data$MasVnrType) & !is.na(combined_data$MasVnrArea), ]
### Among these observation, the house with an Id of 2611 has a masonry veneer area
### of 198 square feet, but is lack of the value of masonry veneer type. This can be 
### missed in the data entry process. One solution is to impute the most common 
### MasVnrType of other houses in the same Neighborhood with the same HouseStyle.
combined_data[combined_data$Id == 2611, "MasVnrType"] <- names(which.max(table(combined_data %>% filter(Neighborhood == "Mitchel" & HouseStyle == "1Story" & MasVnrType!="None") %>% pull(MasVnrType))))

combined_data[is.na(combined_data$MasVnrType) & combined_data$Fence != "No Fence", ]
### Since all other observations without a value MasVnrType have no fence, I assume that
### these houses do not have masonry veneer. Therefore, these NA values will be 
### replaced by "None".
combined_data[is.na(combined_data$MasVnrType), "MasVnrType"] <- "None"

#--- MasVnrArea: 23
combined_data[is.na(combined_data$MasVnrArea), ]
combined_data[is.na(combined_data$MasVnrArea) & combined_data$MasVnrType!="None", ]
### Since all houses with missing MasVnrArea value have no masonry veneer type, these
### houses do not possibly have masonry veneer. Therefore, these NAs will be replaced
### by zeros.
combined_data[is.na(combined_data$MasVnrArea), "MasVnrArea"] <- 0

#--- BsmtQual: 81
BsmtQual_empty <- c(combined_data[is.na(combined_data$BsmtQual), "Id"])
BsmtCond_empty <- c(combined_data[is.na(combined_data$BsmtCond), "Id"])
BsmtExposure_empty <- c(combined_data[is.na(combined_data$BsmtExposure), "Id"])
### According to the dataset description, NAs in BsmtQual, BsmtCond and BsmtExposure
### represents the the houses have no basement.

combined_data[is.na(combined_data$BsmtQual) & !(combined_data$Id %in% BsmtCond_empty), ]
combined_data[is.na(combined_data$BsmtQual) & !(combined_data$Id %in% BsmtExposure_empty), ]
### Observations 2218 and 2219 have NAs in BsmtQual, but not in BsmtCond and BsmtExposure.
### Therefore, for the sake of consistency, we assume that these houses do not have 
### a basement. Hence, their BsmtCond and BsmtExposure have to be changed into "No Basement"
### as well.
combined_data[combined_data$Id %in% c(2218,2219), "BsmtCond"] <- "No Basement"
combined_data[combined_data$Id %in% c(2218,2219), "BsmtExposure"] <- "No Basement"

### Change other NAs values in BsmtQual into "No Basement" to avoid confusion.
combined_data[is.na(combined_data$BsmtQual), "BsmtQual"] <- "No Basement"

#--- BsmtCond: 82
combined_data[is.na(combined_data$BsmtCond) & !(combined_data$Id %in% BsmtQual_empty), ]
combined_data[is.na(combined_data$BsmtCond) & !(combined_data$Id %in% BsmtExposure_empty), ]
### Observations 2041, 2186 and 2525 have NAs in BsmtCond, but not in BsmtQual and BsmtExposure.
### Since 2 out of 3 columns about basement have values, these NAs in BsmtCond may respresent
### the lack of data from the data collection process, rather than representing the houses without
### basements. 
### One solution is to replace these NAs with the most common BsmtCond values based on
### the Neighborhood that these houses are located in.

### Find the most common BsmtCond for Neighborhood Veenker, Edwards and CollgCr
names(which.max(table(combined_data %>% filter(Neighborhood == "Veenker") %>% pull(BsmtCond))))
names(which.max(table(combined_data %>% filter(Neighborhood == "Edwards") %>% pull(BsmtCond))))
names(which.max(table(combined_data %>% filter(Neighborhood == "CollgCr") %>% pull(BsmtCond))))
### The most frequent BsmtCond level for the three Neighborhood is TA.
combined_data[combined_data$Id %in% c(2041, 2186, 2525), "BsmtCond"] <- "TA"

### Change other NAs values in BsmtCond into "No Basement" to avoid confusion.
combined_data[is.na(combined_data$BsmtCond), "BsmtCond"] <- "No Basement"

#--- BsmtExposure: 82
combined_data[is.na(combined_data$BsmtExposure) & !(combined_data$Id %in% BsmtQual_empty), ]
combined_data[is.na(combined_data$BsmtExposure) & !(combined_data$Id %in% BsmtCond_empty), ]
### Observations 949, 1488 and 2349 have NAs in BsmtExposure, but not in BsmtQual and BsmtCond.
### Since 2 out of 3 columns about basement have values, these NAs in BsmtExposure may respresent
### the lack of data from the data collection process, rather than representing the houses without
### basements. 
### One solution is to replace these NAs with the most common BsmtExposure values based on
### the Neighborhood that these houses are located in.

### Find the most common BsmtExposure for Neighborhood CollgCr and Somerst:
names(which.max(table(combined_data %>% filter(Neighborhood == "CollgCr") %>% pull(BsmtExposure))))
names(which.max(table(combined_data %>% filter(Neighborhood == "Somerst") %>% pull(BsmtExposure))))
### The most frequent BsmtExposure level for the 2 Neighborhood is "No".
combined_data[combined_data$Id %in% c(949, 1488, 2349), "BsmtExposure"] <- "No"

### Change other NAs values in BsmtExposure into "No Basement" to avoid confusion.
combined_data[is.na(combined_data$BsmtExposure), "BsmtExposure"] <- "No Basement"

#--- BsmtFinType1: 79
BsmtFinType1_empty <- c(combined_data[is.na(combined_data$BsmtFinType1), "Id"])
BsmtFinType2_empty <- c(combined_data[is.na(combined_data$BsmtFinType2), "Id"])

combined_data[is.na(combined_data$BsmtFinType1) & !(combined_data$Id %in% BsmtQual_empty), ]
combined_data[is.na(combined_data$BsmtFinType1) & !(combined_data$Id %in% BsmtCond_empty), ]
combined_data[is.na(combined_data$BsmtFinType1) & !(combined_data$Id %in% BsmtExposure_empty), ]
combined_data[is.na(combined_data$BsmtFinType1) & !(combined_data$Id %in% BsmtFinType2_empty), ]
### Since the NAs values in this variable are consistent with the observations 
### without a basement in other basement-related variables, these NAs values 
### will turned into "No Basement" to avoid confusion
combined_data[is.na(combined_data$BsmtFinType1), "BsmtFinType1"] <- "No Basement"

#--- BsmtFinSF1: 1
combined_data %>% 
  filter(is.na(BsmtFinSF1))
### Since this observation has no basement, the NA value will be replace with 0 value.
combined_data[is.na(combined_data$BsmtFinSF1), "BsmtFinSF1"] <- 0

#--- BsmtFinType2: 80
combined_data[is.na(combined_data$BsmtFinType2) & !(combined_data$Id %in% BsmtQual_empty), ]
combined_data[is.na(combined_data$BsmtFinType2) & !(combined_data$Id %in% BsmtCond_empty), ]
combined_data[is.na(combined_data$BsmtFinType2) & !(combined_data$Id %in% BsmtExposure_empty), ]
combined_data[is.na(combined_data$BsmtFinType2) & !(combined_data$Id %in% BsmtFinType1_empty), ]
### Observation 333 has NAs in BsmtFinType2, but not in BsmtQual, BsmtCond, BsmtExposure 
### and BsmtFinType1. Additionally, BsmtFinSF2 has a value of 479, proving that 
### this hous has a seond-type basement. Therefore, the missing value can be a mistake
### in the data
### One solution is to replace these NAs with the most common BsmtFinType2 values based on
### the Neighborhood that the house is located in.

### Find the most common BsmtFinType2 for Neighborhood NridgHt:
names(which.max(table(combined_data %>% filter(Neighborhood == "NridgHt") %>% pull(BsmtFinType2))))
### The most frequent BsmtFinType2 level for the 2 Neighborhood is "Unf".
combined_data[combined_data$Id == 333, "BsmtFinType2"] <- "Unf"

### Replace other NAs with "No Basement" to avoid confusion
combined_data[is.na(combined_data$BsmtFinType2),"BsmtFinType2"] <- "No Basement"

#--- BsmtFinSF2: 1
combined_data[is.na(combined_data$BsmtFinSF2),]
### Since the NA value in this variable is consistent with the observation 
### without a basement in other basement-related variables, the NA value 
### will be replaced by 0.
combined_data[combined_data$Id == 2121, "BsmtFinSF2"] <- 0

#--- BsmtUnfSF: 1
combined_data[is.na(combined_data$BsmtUnfSF),]
### Since the NA value in this variable is consistent with the observation 
### without a basement in other basement-related variables, the NA value 
### will be replaced by 0.
combined_data[combined_data$Id==2121, "BsmtUnfSF"] <- 0

#--- TotalBsmtSF: 1
combined_data[is.na(combined_data$TotalBsmtSF),]
### Since the NA value in this variable is consistent with the observation 
### without a basement in other basement-related variables, the NA value 
### will be replaced by 0.
combined_data[combined_data$Id==2121, "TotalBsmtSF"] <- 0

#--- Electrical: 1
combined_data[is.na(combined_data$Electrical),]
### A possible solution is to replace this NA with the most common Electrical level
### based on the Neighborhood this house is located in.
### Find the most frequent Electrical level in Timber:
names(which.max(table(combined_data %>% filter(Neighborhood == "Timber") %>% pull(Electrical))))
### Replace the NA with SBrkr, th most common Electrical type in Timber.
combined_data[is.na(combined_data$Electrical),"Electrical"] <- "SBrkr"

#--- BsmtFullBath: 2
combined_data[is.na(combined_data$BsmtFullBath),]
### These two houses do not have a basement, thus, the NAs should be replace with
### value 0.
combined_data[is.na(combined_data$BsmtFullBath),"BsmtFullBath"] <- 0

#--- BsmtHalfBath: 2
combined_data[is.na(combined_data$BsmtHalfBath),]
### These two houses do not have a basement, thus, the NAs should be replace with
### value 0.
combined_data[is.na(combined_data$BsmtHalfBath),"BsmtHalfBath"] <- 0

#--- KitchenQual: 1
combined_data[is.na(combined_data$KitchenQual),]
### A possible solution is to replace this NA with the most common KitchenQual level
### based on the Neighborhood this house is located in.
### Find the most frequent KitchenQual level in ClearCr:
names(which.max(table(combined_data %>% filter(Neighborhood == "ClearCr") %>% pull(KitchenQual))))
### Replace the NA with TA, the most common KitchenQual type in ClearCr
combined_data[is.na(combined_data$KitchenQual),"KitchenQual"] <- "TA"

#--- Functional: 2
combined_data[is.na(combined_data$Functional),]
### A possible solution is to replace these NAs with the most common Functional level
### based on the Neighborhood this house is located in.
### Find the most frequent Functional level in IDOTRR:
names(which.max(table(combined_data %>% filter(Neighborhood == "IDOTRR") %>% pull(Functional))))
### Replace the NAs with Typ, the most common Functional type in IDOTRR
combined_data[is.na(combined_data$Functional),"Functional"] <- "Typ"

#--- FireplaceQu: 1420
### According to the dataset description, NAs in this variable denotes the houses
### do not have a fire place. 
### Check if there is a consistency between the number of fire places and the NAs
### in FireplacQu
combined_data[combined_data$Fireplaces!=0 & is.na(combined_data$FireplaceQu),]
### These missing values will be replaced by "No Fireplace" to avoid confusion.
combined_data[is.na(combined_data$FireplaceQu), "FireplaceQu"] <- "No Fireplace"

#--- GarageType: 157
### According to the dataset description, NAs in this variable denotes the houses
### do not have a garage. 
### Check if there is a consistency between the garage areas and the NAs in GarageType
combined_data[combined_data$GarageArea!=0 & is.na(combined_data$GarageType),]
### These missing values will be replaced by "No Garage" to avoid confusion.
combined_data[is.na(combined_data$GarageType), "GarageType"] <- "No Garage"

#--- GarageYrBlt: 159
### NAs values in GarageYrBlt can imply that the houses do not have a garage.
combined_data[is.na(combined_data$GarageYrBlt) & combined_data$GarageType=="No Garage",]
### However, there are two observations where the houses have a sign of having a
### garage but have missing values in GarageYrBlt. So, the missing values are resulted
### from insufficient data collection. 
combined_data[is.na(combined_data$GarageYrBlt) & combined_data$GarageType!="No Garage",]
### Particularly, observation 2127 has a garage, thus, the NA in GarageYrBlt is missed in the data 
### collection process. One solution is to replace the NA with the average GarageYrBlt of other houses
### that were also built in 1910.
combined_data[combined_data$Id==2127, "GarageYrBlt"] <- round(mean(combined_data[!is.na(combined_data$GarageYrBlt) & combined_data$YearBuilt==1910, "GarageYrBlt"]),0)
### For observation 2577, since the GarageArea is also missing, the NA of GarageYrBlt
### can be replaced by the average GarageYrBlt of other houses in the same Neighborhood
### with the same GarageType.
combined_data[combined_data$Id==2577, "GarageYrBlt"] <- round(mean(combined_data[!is.na(combined_data$GarageYrBlt) & combined_data$Neighborhood=="IDOTRR" & combined_data$GarageType=="Detchd", "GarageYrBlt"]),0)
combined_data[combined_data$Id==2577, ]

### Other NAs will need to be converted into "No Garage" to avoid confusion. 
### GarageYrBlt will be used to calculate GarageAge, which will then be converted
### into multiple-level categorical variable.

### Take a look at the minimum and maximum GarageAge
combined_data %>% 
  filter(!is.na(GarageYrBlt)) %>% 
  mutate(GarageAge = 2011 -GarageYrBlt) %>% 
  summarise(min = min(GarageAge),
            max = max(GarageAge))
combined_data[(combined_data$GarageYrBlt>2011) & !is.na(combined_data$GarageYrBlt),]
### It is illogical to have houses with garages built after the data collecting time. 
### Since the data collection time was 2011, the GarageYrBlt value that is larger 
### than 2011 (in observation 2593) will be replaced by the time the house was sold (YrSold).
combined_data[combined_data$Id == 2593, "GarageYrBlt"] <- combined_data[combined_data$Id == 2593, "YrSold"]

combined_data %>% 
  filter(!is.na(GarageYrBlt)) %>% 
  mutate(GarageAge = 2011 - GarageYrBlt) %>% 
  summarise(min = min(GarageAge),
            max = max(GarageAge))
### The min GarageAge is 1 and the max GarageAge is 116. GarageAge variable can be 
### divided into 5 levels: "No Garage", "10 years and under", "11 to 30 years", 
### "31 to 60 years", and "Over 60 years".
combined_data$GarageAge <- ifelse(is.na(combined_data$GarageYrBlt), "No Garage",
                                  ifelse((2011 - combined_data$GarageYrBlt) <= 10, "10 years and under",
                                         ifelse((2011 - combined_data$GarageYrBlt) <= 30, "11 to 30 years",
                                                ifelse((2011 - combined_data$GarageYrBlt) <= 60, "31 to 60 years",
                                                       "Over 60 years"))))

#--- GarageFinish: 159
### According to the dataset description, NAs in this variable denotes the houses
### do not have a garage. 
### Check if there is a consistency between the garage type and the NAs in GarageFinish
combined_data[combined_data$GarageType!="No Garage" & is.na(combined_data$GarageFinish),]
### Observation 2127 and 2577 have a garage, thus, the NAs in GarageFinish are missed in the data 
### collection/ entry process.
### One solution is to replace the NAs with the most common GarageFinish of other houses
### that were also built in the same year.
combined_data[combined_data$Id == 2127, "GarageFinish"] <- names(which.max(table(combined_data %>% filter(YearBuilt == 1910) %>% pull(GarageFinish))))
combined_data[combined_data$Id == 2577, "GarageFinish"] <- names(which.max(table(combined_data %>% filter(YearBuilt == 1923) %>% pull(GarageFinish))))

### Other NAs will be replaced with "No Garage"
combined_data[is.na(combined_data$GarageFinish), "GarageFinish"] <- "No Garage"

#--- GarageCars: 1
combined_data[is.na(combined_data$GarageCars),]
### Observation 2577 is missing the GarageCars value. One solution is to replace it
### with the mean GarageCars value of the neighbohood that this house is
### located in.
combined_data[combined_data$Id == 2577, "GarageCars"] <- round(mean(combined_data[combined_data$Neighborhood == "IDOTRR", "GarageCars"], na.rm = T),0)

#--- GarageArea: 1
### Observation 2577 is missing the GarageArea value. One solution is to replace it
### with the mean GarageArea value of the same GarageCars number.
combined_data[combined_data$Id == 2577, "GarageArea"] <- round(mean(combined_data[combined_data$GarageCars==1, "GarageArea"], na.rm = T),0)

#--- GarageQual: 159
combined_data[is.na(combined_data$GarageQual) & combined_data$GarageType!="No Garage",]
### Observations 2127 and 2577 are lack of GarageQual values, which can be replaced 
### by the most common GarageQual of other houses in the same Neighbohood with the 
### same GarageCars value.
combined_data[combined_data$Id == 2127, "GarageQual"] <- names(which.max(table(combined_data %>% filter(Neighborhood=="OldTown" & GarageCars==1) %>% pull(GarageQual))))
combined_data[combined_data$Id == 2577, "GarageQual"] <- names(which.max(table(combined_data %>% filter(Neighborhood=="IDOTRR" & GarageCars==1) %>% pull(GarageQual))))

### Other NAs value will be replaced by "No Garage".
combined_data[is.na(combined_data$GarageQual), "GarageQual"] <- "No Garage"

#--- GarageCond: 159
combined_data[is.na(combined_data$GarageCond) & combined_data$GarageType!="No Garage",]

### Observations 2127 and 2577 are lack of GarageCond values, which can be replaced 
### by the most common GarageCond of other houses in the same Neighbohood with the 
### same GarageCars value.
combined_data[combined_data$Id == 2127, "GarageCond"] <- names(which.max(table(combined_data %>% filter(Neighborhood=="OldTown" & GarageCars==1) %>% pull(GarageCond))))
combined_data[combined_data$Id == 2577, "GarageCond"] <- names(which.max(table(combined_data %>% filter(Neighborhood=="IDOTRR" & GarageCars==1) %>% pull(GarageCond))))

### Other NAs value will be replaced by "No Garage".
combined_data[is.na(combined_data$GarageCond), "GarageCond"] <- "No Garage"

#--- PoolQC: 2909
### According to the dataset description, NAs in PoolQC correspond to houses without
### a pool.
### Check if the NAs are consistent with the PoolArea:
combined_data[is.na(combined_data$PoolQC) & combined_data$PoolArea!=0,]
### Observations 2421, 2504 and 2600 have a pool, but still have missing PoolQC values.
### This can be missed in the data collection process. Therefore, NAs in these rows
### will be replaced by the most common PoolQC of the dwelling type (BldgType)
### these three houses have.
names(which.max(table(combined_data %>% filter(BldgType == "1Fam") %>% pull(PoolQC)))) #"Ex"
combined_data[combined_data$Id %in% c(2421, 2504, 2600), "PoolQC"] <- "Ex"

### Replace other NAs with "No Pool" to avoid confusion
combined_data[is.na(combined_data$PoolQC), "PoolQC"] <- "No Pool"

#--- Fence: 2348
### According to the dataset description, NAs in Fence variable corresponds to houses 
### having no fence. Thus, the NAs will be replaced by "No Fence".
combined_data[is.na(combined_data$Fence), "Fence"] <- "No Fence"

#--- MiscFeature: 4
### NAs represent houses with no miscellaneous feature
combined_data[is.na(combined_data$MiscFeature), "MiscFeature"] <- "None"

#--- SaleType: 1
combined_data[is.na(combined_data$SaleType),]
### One solution is to replace the NA with the most common SaleType of other houses
### with the same Normal SaleCondition.
combined_data[is.na(combined_data$SaleType),"SaleType"] <- names(which.max(table(combined_data %>% filter(SaleCondition == "Normal") %>% pull(SaleType)))) #"WD"

#--- SalePrice: 1459
### These NAs values are assigned to the test dataset to merge with the train data. Thus, 
### there is no issue in this variables.


### 1.6 Apply changes to some numeric variables -----------------
# OverallQual
ggplot(combined_data %>% filter(!is.na(SalePrice)), aes(OverallQual, SalePrice)) +
  geom_point()
#### Different levels of OverallQual seem to have different correlation with SalePrice,
#### thus, treating OverallQual as a factor can provide more details to the model.
combined_data$OverallQual <- as.factor(combined_data$OverallQual)
#### 
# OverallCond
ggplot(combined_data %>% filter(!is.na(SalePrice)), aes(OverallCond, SalePrice)) +
  geom_point()
#### Similar to OverallQual, treating OverallCond as a factor can provide more details to the model.
combined_data$OverallCond <- as.factor(combined_data$OverallCond)
#### 
# MoSold
ggplot(combined_data %>% filter(!is.na(SalePrice)), aes(MoSold, SalePrice)) +
  geom_point()
#### Treating MoSold as a factor can provide more details on the seasonality of 
#### SalePrice for the model.
combined_data$MoSold <- as.factor(combined_data$MoSold)

# YearBuilt
### YearBuilt can be used to calculate Age variable, which represents 
### the number of years between when each house was built and when the data was
### collected (2011). The variable is will show the newness of each house, which
### can protentially e an important predictor for SalePrice.
combined_data <- combined_data %>% 
  mutate(Age = 2011 - YearBuilt)
### Check if there is any negative Age
combined_data %>% 
  filter(Age<0)

# YearRemodAdd
### Similar to YearBuilt, YearRemodAdd can be used to calculate RemodAge, which
### can tell how recent each house was remodeled.
combined_data <- combined_data %>% 
  mutate(RemodAge = 2011 - YearRemodAdd)
### Check if there is any negative Age
combined_data %>% 
  filter(RemodAge<0) # no observation

# YrSold
unique(combined_data$YrSold)
### Since all houses were sold between 2006 and 2010, there are relatively few
### unique YrSold values. Thus, it is logical to treat this variable as a factor
### to get more information on each year level and its correlation with SalePrice.
combined_data$YrSold <- as.factor(combined_data$YrSold)

# OpenPorchSF, EnclosedPorch, X3SsnPorch and ScreenPorch
### These predictors will be combined to create another predictor - "Porch".
combined_data <- combined_data %>% 
  mutate(Porch = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)

# Update the numeric and categorical variable lists
### Update numeric variable list
numeric_vars_list <- numeric_vars_list[! numeric_vars_list 
                                       %in% c("Id", "OverallQual", "OverallCond",
                                              "YearBuilt", "YearRemodAdd","GarageYrBlt",
                                              "OpenPorchSF", "EnclosedPorch", "X3SsnPorch",
                                              "ScreenPorch","MoSold", "YrSold")]
numeric_vars_list <- c(numeric_vars_list, "Porch", "Age", "RemodAge")
### Update ategorical variable list
categ_vars_list <- c(categ_vars_list, "OverallQual", "OverallCond", "YrSold", "GarageAge")

### 1.7 Identify typos for categorical variables ----------------


print(categ_vars_list)
for(x in categ_vars_list){
  print("----")
  print(x)
  print(unique(combined_data[, x]))
}

### MSZoning
unique(combined_data$MSZoning)
##### RL      RM      C (all)   FV      RH 
##### Change the "C (all)" level to "C" (which stands for Commercial) to be 
##### consistent with the dataset description.
# unclass(combined_data$MSZoning)
combined_data[combined_data$MSZoning == "C (all)", "MSZoning"] <- "C"

### BldgType
unique(combined_data$BldgType)
### "1Fam"   "2fmCon" "Duplex" "TwnhsE" "Twnhs" 
### Change "2fmCon" level to "2FmCon", and "Duplex" to "Duplx", "Twnhs" to "TwnhsI"
### to be consistet with the dataset decription
combined_data[combined_data$BldgType == "2fmCon", "BldgType"] <- "2FmCon"
combined_data[combined_data$BldgType == "Duplex", "BldgType"] <- "Duplx"
combined_data[combined_data$BldgType == "Twnhs", "BldgType"] <- "TwnhsI"

### Exterior2nd
### Change "Wd Shng" level into "WdShing", "CmentBd" into "CemntBd", "Brk Cmn" into
### "BrkComm"
combined_data[combined_data$Exterior2nd == "Wd Shng", "Exterior2nd"] <- "WdShing"
combined_data[combined_data$Exterior2nd == "CmentBd", "Exterior2nd"] <- "CemntBd"
combined_data[combined_data$Exterior2nd == "Brk Cmn", "Exterior2nd"] <- "BrkComm"

### 1.8 Convert integer variables into numeric ------------------
for(x in numeric_vars_list){
  combined_data[, x] <- as.numeric(combined_data[,x])
}
### 1.9 Check variable distribution -----------------------------
count_percent <- function(df, var) {
  # detect if there is any missing values 
  if(any(is.na(df[, var]))) {
    return("There are some missing values.")
    }
  
  # for categorical variable
  else if (is.factor(df[, var]) || is.character(df[, var])) {
    # Count the number of observations for each level of the variable
    counts <- table(df[,var])
    # Calculate the percentage for each level
    percents <- round(prop.table(counts)*100, 2)
    # Return a dataframe with the level and the percentage
    return(data.frame(level = names(counts), percent = percents))
    
    # for numeric variable
  } else if (is.integer(df[, var]) || is.numeric(df[, var])) {
    # Get the unique values of the variable
    unique_values <- unique(df[,var])
    # Create an empty vector to store the counts
    counts <- numeric(length(unique_values))
    # Iterate over the unique values and count the occurrences
    for (i in 1:length(unique_values)) {
      counts[i] <- sum(df[,var] == unique_values[i])
    }
    # Calculate the percentage for each unique value
    percents <- round(prop.table(counts)*100, 2)
    # Return a dataframe with the level and the percentage
    return(data.frame(level = unique_values, percent = percents))
  }
}
col_list <- as.vector(colnames(combined_data))
# Update column list
col_list <- col_list[! col_list %in% c("Id", "YearBuilt", "YearRemodAdd","GarageYrBlt",
                                       "OpenPorchSF", "EnclosedPorch", "X3SsnPorch",
                                       "ScreenPorch","MoSold")]

# Identify the columns with more than 95% of the obervations belonging to one unique
# value

combined_data <- combined_data[,col_list]
result <- sapply(combined_data, function(col) {
  tbl <- table(col)
  max_prop <- max(tbl) / sum(tbl)
  if (max_prop >= 0.95) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

imbalanced_cols <- names(result[result])
imbalanced_cols

### The following variables have almost all observations belonging to just one level:
### - "Street": Pave - 99.59%
count_percent(combined_data, "Street")

### - "Utilities": AllPub - 99.97%
count_percent(combined_data, "Utilities")

### - "LandSlope": Gtl - 95.17%
count_percent(combined_data, "LandSlope")

### - "PoolArea": 0 - 99.55%
count_percent(combined_data, "PoolArea")

### - "PoolQC": No Pool - 99.55%
count_percent(combined_data, "PoolQC")

### - "MiscFeature": None - 96.40%
count_percent(combined_data, "MiscFeature")

### - "MiscVal": 0 - 96.47%
count_percent(combined_data, "MiscVal")

### - "Condition2": Norm - 98.97%
count_percent(combined_data, "Condition2")

### - "RoofMatl": CompShg - 98.53%
count_percent(combined_data, "RoofMatl")

### - "Heating": GasA - 98.46%
count_percent(combined_data, "Heating")

### These predictors do not provide much useful insight, therefore, they are likely
### excluded from the model.

# Update variable list
col_list <- col_list[! col_list %in% c("Street", "Utilities", "LandSlope", "PoolArea", "PoolQC",
                             "MiscFeature", "MiscVal", "Condition2", "RoofMatl", "Heating")]
numeric_vars_list <- numeric_vars_list[! numeric_vars_list %in% c("PoolArea", "MiscVal")]
categ_vars_list <- categ_vars_list[! categ_vars_list %in% c("Street", "Utilities", "LandSlope", "PoolQC",
                                                            "MiscFeature", "Condition2", "RoofMatl", "Heating")]

### 1.11 Check skewness for numeric variables --------------------
combined_data_num <- combined_data[, numeric_vars_list]

# calculate_skewness <- sapply(combined_data_num, function(x) {
#       return(skewness(x))
# })
# print(calculate_skewness)

# Calculate skewness of each column
skewness_df <- apply(combined_data_num, 2, skewness)

# Filter out columns with skewness greater than 1
skewed_cols <- names(skewness_df[skewness_df > 1.0])
skewed_cols

## Find the prescription to improve the skewness of the following columns:
## "LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "TotalBsmtSF",
## "X1stFlrSF", "LowQualFinSF", "GrLivArea", "BsmtHalfBath", "KitchenAbvGr", "WoodDeckSF"  
## "Porch".      

# Function to check skewness of a variable:
check_skewness <- function(var) {
  print(skewness(combined_data[,var]))
  ggplot(combined_data, aes(combined_data[,var]))+
    geom_density()
}
#--- "LotFrontage"
check_skewness("LotFrontage") # 1.542125
## This variable is highly skewed. I will try square root transformation to see if 
## it improves the skewness.
ggplot(combined_data, aes((LotFrontage)^(1/2)))+
  geom_density()
skewness((combined_data$LotFrontage)^(1/2)) # 0.0005762056
## Skewness issue has been significantly improved.

#--- "LotArea"
check_skewness("LotArea") # 12.81584
## This variable is highly skewed. I will try log10 transformation to see if 
## it improves the skewness.
ggplot(combined_data, aes(log10(LotArea)))+
  geom_density()
skewness(log10(combined_data$LotArea)) # -0.5050227
## Skewness issue has been significantly improved.

#--- "MasVnrArea" 
check_skewness("MasVnrArea") # 2.612249
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

#--- "BsmtFinSF1" 
check_skewness("BsmtFinSF1") # 1.424498
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

#--- "BsmtFinSF2"
check_skewness("BsmtFinSF2")
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

#--- "TotalBsmtSF" 
check_skewness("TotalBsmtSF")
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

# "X1stFlrSF" 
check_skewness("X1stFlrSF") # 1.468849
ggplot(combined_data, aes(log10(X1stFlrSF)))+
  geom_density()
skewness(log10(combined_data$X1stFlrSF)) # 0.06382938
## Skewness issue has been significantly improved.

# "LowQualFinSF"
check_skewness("LowQualFinSF") # 12.08255
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

# "GrLivArea" 
check_skewness("GrLivArea") # 1.268705
## Try to apply log transformation
ggplot(combined_data, aes(log10(GrLivArea)))+
  geom_density()
skewness(log10(combined_data$GrLivArea)) # 0.01237294
## The skewness is improved.

# "BsmtHalfBath"
check_skewness("BsmtHalfBath") # 3.929574
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

# "KitchenAbvGr" 
check_skewness("KitchenAbvGr") # 4.300044
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

# "WoodDeckSF" 
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

# "Porch"
check_skewness("Porch") # 2.236117
## This variable is highly skewed. However, this skewness is a result of having many
## zeros value. Thus, no concave transformation can change the skewed shape of it.

# "SalePrice"
skewness(train$SalePrice) # 1.879009
ggplot(train, aes(SalePrice))+
  geom_density()
## SalePrice values in the train dataset is highly skewed.
## Try to apply log transformation
ggplot(train, aes(log10(SalePrice)))+
  geom_density()
skewness(log10(train$SalePrice)) # 0.1210859
## improved

### 1.12 Convert categorical variables into factor type ----------
for(x in categ_vars_list){
  combined_data[, x] <- as.factor(combined_data[,x])
}
### 1.10 Identify outliers for numeric variables -----------------
#### Function generating boxplot fo numeric variables
# outlier_plot <- function(var) {
#   ggplot(combined_data, aes(combined_data[, var])) +
#     geom_boxplot() +
#     labs(title = paste(var, "boxplot"), x = var)
# }

## 2. The “plain vanilla” model ------------------------------------------------
### 2.1 Check multicollinearity ----------------------------------
# split the dataset back to the train and test dataset
train_cleaned <- combined_data[!is.na(combined_data$SalePrice), col_list]
test_cleaned <- combined_data[is.na(combined_data$SalePrice), col_list]

##-- There are some levels of categorical variables available in the test data set 
##-- but not in the train data set. These levels will be replaced by their closest
##-- level.
#--- combine the MSSubClass 150 level to the closest one - MSSubClass 120
test_cleaned[test_cleaned$MSSubClass=="150", "MSSubClass"] <- "120"
#--- combined the OverallQual 1 level to the closest one - OverallQual 2
test_cleaned[test_cleaned$OverallQual=="1", "OverallQual"] <- "2"
#--- combined the OverallCond 1 level to the closest one - OverallCond 2
test_cleaned[test_cleaned$OverallCond=="1", "OverallCond"] <- "2"

# Plain vanilla model
plain_model <- lm(formula = log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea
                                                 + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(X1stFlrSF)) + I(log10(GrLivArea)), 
                  data = train_cleaned)
summary(plain_model)

plain_predict_1 <- predict(plain_model, test_cleaned)
plain_SalePrice_beforeDiagnostic <- data.frame(Id = test$Id, SalePrice = 10^plain_predict_1)

# vif(plain_model) # the function would not work because there are aliased coefficients in the model
# use alias() function to identify high-correlated (aliased) predictors
alias(plain_model)

#-- Drop the following variables from the model due to their high correlation with other variables: 
#- BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType 
# - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2

# Rerun the plain vanila model
plain_model2 <- lm(formula = log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea 
                  + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(X1stFlrSF)) + I(log10(GrLivArea))
                  - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2,
                  data = train_cleaned)
summary(plain_model2)
vif(plain_model2)

# The following columns have high vif values: X1stFlrSF, X2ndFlrSF, GrLivArea
## X2ndFlrSF, X1stFlrSF, GrLivArea
cor(train_cleaned$GrLivArea, train_cleaned$X1stFlrSF)
cor(train_cleaned$GrLivArea, train_cleaned$X2ndFlrSF)
# Noticing that the sum of X2ndFlrSF, X1stFlrSF is aproximately equal to GrLivArea,
# we keep GrLivArea and drop X1stFlrSF and X2ndFlrSF.

# Reun the plain vanilla model:
plain_model3 <- lm(formula = log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea 
                   + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                   - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2
                   - X1stFlrSF - X2ndFlrSF, 
                   data = train_cleaned)
summary(plain_model3)
vif(plain_model3)
## No predictors have a vif value of 4 or over.

### 2.2 Check nonlinearity & heterscedasticity -------------------
#-- Residuals vs Fitted plot
plot(plain_model3, which = 1)
#-- Scale-Location plot 
plot(plain_model3, which = 3) # not plotting observations with leverage one: 251, 326, 667, 1012, 1188, 1371 ??

## In the Residuals vs Fitted plot, since the red curve in the plot is relatively 
## horizontal, the residuals values have a mean of close to 0, implying there is no 
## non-linearity issue.
## On the other hand, the residuals value are distributed in a wider range below
## 0 than above and vary more in the middle than the begin and end. Accompanied  
## with the curve pattern in the Scale-Location plot, we can conclude that 
## heterscedasticity issue exits in the model.

## Prescription: concave transformation on SalePrice 

## 
### 2.3 Identify influential points ------------------------------
plot(plain_model3, which = 4, id.n = 5)
#-- Overinfluential points: 376 and 534
train_cleaned <- train_cleaned[-c(376, 534),]

#-- Rerun the model
plain_model3 <- lm(formula = log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea 
                   + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                   - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2
                   - X1stFlrSF - X2ndFlrSF, 
                   data = train_cleaned)
plot(plain_model3, which = 4, id.n = 5)
# No point exceeding the Cook's distance of 1. Point 1299 is worth investigating later.

## 3. Implementing analytical techniques ---------------------------------------
### MSE - Plain Vanilla model
MSE_Train_Plain <- mean(train_cleaned$SalePrice - 10^plain_model3$fitted.values)
MSE_Train_Plain # 1190.692

# SalePrice prediction based on the plain vanilla model:
plain_predict <- predict(plain_model3, test_cleaned)

### 3.1 Forward, Backward, Hybrid using AIC ----------------------
#-- Forward using AIC
null_model <- lm(log10(SalePrice) ~ 1, data = train_cleaned) #specify null model
Forward_AIC <- stepAIC(null_model, direction = "forward", 
                       scope = list(lower = null_model, upper = ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea 
                                    + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                                    - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2
                                    - X2ndFlrSF - PoolArea - MiscVal), k = 2)
summary(Forward_AIC)
# SalePrice predictions based on  the forward AIC model:
Forward_AIC_predict <- predict(Forward_AIC, test_cleaned)

#-- Backward using AIC
Backward_AIC <- stepAIC(plain_model3, direction = "backward", k = 2)
summary(Backward_AIC)

# SalePrice predictions based on the backward AIC model:
Backward_AIC_predict <- predict(Backward_AIC, test_cleaned)

#-- Hybrid using AIC
Hybrid_AIC <- stepAIC(null_model, direction = "both", 
                      scope = list(lower = null_model, upper = ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea 
                                   + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                                   - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2
                                   - X2ndFlrSF - PoolArea - MiscVal), k = 2)
summary(Hybrid_AIC)

# SalePrice predictions based on the hybrid AIC model:
Hybrid_AIC_predict <- predict(Hybrid_AIC, test_cleaned)

### 3.2 Lasso ----------------------------------------------------
#-- Training data set
library(glmnet)
x_train <- model.matrix(log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea 
                        + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                        - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2, data = train_cleaned)[, -1]
y_train <- log10(train_cleaned$SalePrice)
#-- Testing data set
test_cleaned$SalePrice <- 0.0000001
x_test <- model.matrix(log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea 
                       + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                       - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2, data = test_cleaned)[, -1]
# Run the Lasso model
set.seed(666)
cv_out_lasso <- cv.glmnet(x_train, y_train, alpha = 1, 
                          type.measure = "mse", nfolds = 10)
plot (cv_out_lasso)
bestlam_lasso <- cv_out_lasso$lambda.min
model_lasso <- glmnet (x_train, y_train, alpha = 1, 
                       standardize = TRUE, lambda = bestlam_lasso)
model_lasso$beta
mode(model_lasso$beta)

# SalePrice predictions based on Lasso model:
lasso_predict <- predict(model_lasso, newx = x_test)

### 3.3 Ridge ----------------------------------------------------
cv_out_ridge <- cv.glmnet(x_train, y_train, alpha = 0, 
                          type.measure = "mse", nfolds = 10)
plot (cv_out_ridge)
bestlam_ridge <- cv_out_ridge$lambda.min
model_ridge <- glmnet (x_train, y_train, alpha = 0, 
                       standardize = TRUE, lambda = bestlam_ridge)
model_ridge$beta

# SalePrice predictions based on Ridge model:
ridge_predict <- predict(model_ridge, newx = x_test)

### 3.4 Simple Regression Tree -----------------------------------
library(tree)
Simple_tree <- tree(formula = SalePrice ~., data = train_cleaned)
plot(Simple_tree)
summary(Simple_tree)

# SalePrice predictions based on Simple Regression Tree model:
Simple_SalePrice <- predict(Simple_tree, test_cleaned)

### 3.5 Tree Pruning ---------------------------------------------
Prune_tree <- cv.tree(Simple_tree, K=10)
plot(Prune_tree$size, Prune_tree$dev, type='b')
## The output of plot() shows that the best tree is the one with 
## 11 terminal nodes. That is the tree generated with the simple decision tree.
## Therefore, the tree does not need to be pruned.

### 3.6 Bagging --------------------------------------------------
## Perform bagging approach with 500 bootstraps
library(randomForest)
set.seed(1)
Tree_Bagging <- randomForest(SalePrice ~ ., data = train_cleaned,
                             ntrees = 500, mtry = 65, replace = TRUE,
                             importance = TRUE)
Tree_Bagging
importance(Tree_Bagging)

# SalePrice predictions based on Regression Bagging model:
Bagging_SalePrice <- predict(Tree_Bagging, test_cleaned)

### 3.7 Radom Forests -------------------------------------------
set.seed(1)
## Because the SalePrice values are unknown in the test data set, we can not 
## write a for loop to find the best mtry value. We can approximate the best mtry
## by splitting the train data set and use a part of it to be the "sub-test" dataset.
RF_train <- sample_frac(train_cleaned,0.7)
RF_test <- anti_join(train_cleaned,RF_train)

Test_MSE_RF <- rep(NA, 64)
RF_SalePrice_predict <-rep(NA, 64)

## Because running the for loop from range 1 to 64 takes a lot of time, I break
## down the loop into smaller intervals: 1 to 10, 11 to 20, ..., 61 to 64.

# i in [1:10]
for (i in 1:10){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = RF_train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  RF_SalePrice_predict <- predict(Tree_RF, RF_test)
  Test_MSE_RF[i] <- mean((RF_test$SalePrice - RF_SalePrice_predict)^2)
}
# i in [11:20]
for (i in 11:20){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = RF_train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  RF_SalePrice_predict <- predict(Tree_RF, RF_test)
  Test_MSE_RF[i] <- mean((RF_test$SalePrice - RF_SalePrice_predict)^2)
}
# i in [21:30]
for (i in 21:30){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = RF_train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  RF_SalePrice_predict <- predict(Tree_RF, RF_test)
  Test_MSE_RF[i] <- mean((RF_test$SalePrice - RF_SalePrice_predict)^2)
}
# i in [31:40]
for (i in 31:40){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = RF_train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  RF_SalePrice_predict <- predict(Tree_RF, RF_test)
  Test_MSE_RF[i] <- mean((RF_test$SalePrice - RF_SalePrice_predict)^2)
}
# i in [41:50]
for (i in 41:50){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = RF_train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  RF_SalePrice_predict <- predict(Tree_RF, RF_test)
  Test_MSE_RF[i] <- mean((RF_test$SalePrice - RF_SalePrice_predict)^2)
}
# i in [51:60]
for (i in 51:60){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = RF_train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  RF_SalePrice_predict <- predict(Tree_RF, RF_test)
  Test_MSE_RF[i] <- mean((RF_test$SalePrice - RF_SalePrice_predict)^2)
}
# i in [61:64]
for (i in 61:64){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = RF_train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  RF_SalePrice_predict <- predict(Tree_RF, RF_test)
  Test_MSE_RF[i] <- mean((RF_test$SalePrice - RF_SalePrice_predict)^2)
}

which.min(Test_MSE_RF) # mtry=28 gives the lowest MSE
Tree_RF <- randomForest(SalePrice ~ ., data = train_cleaned,
                        ntrees = 500, mtry = 28, replace = TRUE,
                        importance = TRUE)

# SalePrice predictions based on Regression Random Forest model:
RF_SalePrice <- predict(Tree_RF, test_cleaned)

### 3.8 Boosting ------------------------------------------------
library(gbm)
## Similar to the random forest method, we will find the optimal n.trees and
## interaction.depth using the train data set
n_trees <- rep(0, 6)
min_cv_error <- rep(0, 6)

## For loop version to find the optimal interaction.depth and the corresponding ntrees.
# for(i in 1:6){
#   set.seed(1)
#   Tree_Boosting <- gbm(SalePrice ~., data = RF_train, distribution = "gaussian",
#                        n.trees = 5000, interaction.depth = i, cv.folds = 10,
#                        shrinkage = 0.01)
#   n_trees[i] <- which.min(Tree_Boosting$cv.error)
#   min_cv_error[i] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
# }
## Break down the for loop into individual values to run faster.
# i = 1
Tree_Boosting <- gbm(SalePrice ~., data = RF_train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 1, cv.folds = 10,
                     shrinkage = 0.01)
n_trees[1] <- which.min(Tree_Boosting$cv.error)
min_cv_error[1] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
# i = 2
Tree_Boosting <- gbm(SalePrice ~., data = RF_train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 2, cv.folds = 10,
                     shrinkage = 0.01)
n_trees[2] <- which.min(Tree_Boosting$cv.error)
min_cv_error[2] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
# i = 3
Tree_Boosting <- gbm(SalePrice ~., data = RF_train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 3, cv.folds = 10,
                     shrinkage = 0.01)
n_trees[3] <- which.min(Tree_Boosting$cv.error)
min_cv_error[3] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
# i = 4
Tree_Boosting <- gbm(SalePrice ~., data = RF_train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 4, cv.folds = 10,
                     shrinkage = 0.01)
n_trees[4] <- which.min(Tree_Boosting$cv.error)
min_cv_error[4] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
# i = 5
Tree_Boosting <- gbm(SalePrice ~., data = RF_train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 5, cv.folds = 10,
                     shrinkage = 0.01)
n_trees[5] <- which.min(Tree_Boosting$cv.error)
min_cv_error[5] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
# i = 6
Tree_Boosting <- gbm(SalePrice ~., data = RF_train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 6, cv.folds = 10,
                     shrinkage = 0.01)
n_trees[6] <- which.min(Tree_Boosting$cv.error)
min_cv_error[6] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]

which.min(min_cv_error)

#interaction.depth = 6 gives the lowest cv error.
#The corresponding n.trees is 4999.

n_trees[6]

Tree_Boosting <- gbm(SalePrice ~., data = train_cleaned, distribution = "gaussian",
                     n.trees =  4999, interaction.depth = 6,
                     shrinkage = 0.01)
summary(Tree_Boosting)

Boosting_SalePrice_predict <- predict(Tree_Boosting, test_cleaned)

# 4. Summary -------------------------------------------------------------------

# Based on the public score table, the Boosting method generated the most accurate 
# model, followed by Lasso. However, since the Boosting model has low in interpretability, 
# the Lasso model is superior due to its interpretability and relatively great accuracy. 
# Therefore, in my opinion, the Lasso model would provide better insights on the predictors 
# regarding the response variables.

## 4.1 Filter out the most important variables:
lasso_model_beta <- as.data.frame(as.matrix(model_lasso$beta))
lasso_model_beta %>% 
  filter(s0 != 0) %>% 
  arrange(desc(abs(s0)))

# lasso_model_beta %>% 
#   filter(s0 == 0) 

## Top most important coefficients:
# I(log10(GrLivArea))      4.153382e-01
# OverallQual9             1.127962e-01
# FunctionalSev           -1.102835e-01
# OverallQual10            9.207216e-02
# OverallQual2            -8.949275e-02
# I(log10(LotArea))        6.854458e-02
# OverallCond3            -6.728260e-02
# FunctionalMaj2          -6.575209e-02

## 4.2 Interpret the important coefficients

### 4.2.1 I(log10(GrLivArea))      4.153382e-01
(1.01^(4.153382e-01)-1)*100 # 0.4141304
#-- For every 1% increase in above-ground living area in square feet, the median house price
#-- will on average increase by around 0.414% holding everything else constant.

### 4.2.2 OverallQual9             1.127962e-01
(10^(1.127962e-01) -1)*100 # 29.65707
#-- Holding everything else constant, the median price of a house with overall quality  
#-- of 9 is on average 29.66% higher than that of houses with overall quality of 1.

### 4.2.3 FunctionalSev           -1.102835e-01
(10^(-1.102835e-01) -1)*100 # -22.42594
#-- Holding everything else constant, the median price of a house with severely 
#-- damaged home functionality is on average 22.43% lower than that of houses with 
#-- home functionality of major deduction 1.

### 4.2.4 OverallQual10            9.207216e-02
(10^(9.207216e-02) -1)*100 # 23.61528
#-- Holding everything else constant, the median price of a house with overall quality  
#-- of 10 is on average 23.62% higher than that of houses with overall quality of 1.

### 4.2.5 OverallQual2            -18.62196
(10^(-8.949275e-02) -1)*100 # 23.61528
#-- Holding everything else constant, the median price of a house with overall quality  
#-- of 2 is on average 18.62% lower than that of houses with overall quality of 1.

### 4.2.6 I(log10(LotArea))        6.854458e-02
(1.01^(6.854458e-02)-1)*100 # 0.06822739
#-- For every 1% increase in lot size in square feet, the median house price
#-- will on average increase by around 0.068% holding everything else constant.

### 4.2.7 OverallCond3            -6.728260e-02
(10^(-6.728260e-02) -1)*100 # -14.35197
#-- Holding everything else constant, the median price of a house with overall condition  
#-- of 3 is on average 14.35% lower than that of houses with overall condition of 1.

### 4.2.8 FunctionalMaj2          -6.575209e-02
(10^(-6.575209e-02) -1)*100 # -14.0496
#-- Holding everything else constant, the median price of a house with home 
#-- functionality of major deduction 2 is on average 14.05% lower than that of houses with 
#-- home functionality of major deduction 1.

## 4.3 Unimportant variables
lasso_model_beta %>% 
  filter(s0 == 0) 

# 5. Rerun the chosen model with interaction (Lasso) ---------------------------
x_train_v4 <- model.matrix(log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea
                           + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                           - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2
                           + OverallQual*GrLivArea + OverallQual*LotArea, data = train_cleaned)[, -1]
y_train_v4 <- log10(train_cleaned$SalePrice)
test_cleaned$SalePrice <- 0.0000001
x_test_v4 <- model.matrix(log10(SalePrice) ~ . - LotFrontage - LotArea - X1stFlrSF - GrLivArea
                          + I(LotFrontage^(1/2)) + I(log10(LotArea)) + I(log10(GrLivArea))
                          - BldgType - Exterior2nd - BsmtQual - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF - Electrical - GarageType - GarageFinish - BsmtCond - GarageQual - BsmtExposure - GarageAge - BsmtFinType2
                          + OverallQual*GrLivArea + OverallQual*LotArea, data = test_cleaned)[, -1]

set.seed(666)
cv_out_lasso_interaction <- cv.glmnet(x_train_v4, y_train_v4, alpha = 1,
                                      type.measure = "mse", nfolds = 10)
plot (cv_out_lasso_interaction)
bestlam_lasso_int <- cv_out_lasso_interaction$lambda.min
model_lasso_int <- glmnet (x_train_v4, y_train_v4, alpha = 1,
                           standardize = TRUE, lambda = bestlam_lasso_int)
model_lasso_int$beta
lasso_predict_interaction <- predict(model_lasso_int, newx = x_test_v4)
lasso_SalePrice_v4 <- data.frame(Id = test$Id, SalePrice = 10^lasso_predict_interaction)
colnames(lasso_SalePrice_v4)[2] <- "SalePrice"

## The rerun Lasso model with interaction perform slightly better than the original 
## Lasso model.

