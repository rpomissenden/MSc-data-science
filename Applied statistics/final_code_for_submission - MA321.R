######################### Loading Libraries ######################### 

library(recipes)
library(tidyverse)
library(ggcorrplot)
library(mltools)
library(data.table)
library(Boruta)
library(writexl)
library(dplyr)
library(readr)
library(olsrr)
library(leaps)
library(nnet) 
library(mlogit)
library(rfUtilities)
library(caret) 
library(e1071) 
library(parsnip)
library(yardstick)
library(e1071)
library(kernlab)
library(rsample)
library(cvTools)
library(randomForest)

######################### LOADING DATA ######################### 

data <- read.csv(file = 'house-data.csv', stringsAsFactors = TRUE)
data = subset(data, select = -c(Id))

### Summaries

dim(data)

str(data)

head(data)

######################### EDA (Q1) ######################### 

# Missing Value (% of Features)
missing.values <- data %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('White', 'Black'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

# Distribution of SalePrice
ggplot(data, aes(x=SalePrice)) +
  geom_histogram(binwidth=10000, colour="black", fill="grey") +
  geom_vline(aes(xintercept=mean(SalePrice, na.rm=T)),
             color="red", linetype="dashed", size=1)+
  scale_x_continuous(labels = scales::comma)

# Distribution of Neighborhood
ggplot(data, aes(x=Neighborhood))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+
  coord_flip()

# Distribution of YearBuilt
ggplot(data, aes(x=YearBuilt)) +
  geom_histogram(binwidth=10, colour="black", fill="grey") 

######################### PREPROCESSING ######################### 

# Impute categorical 'NA' values to new factor 'None' where contextually appropriate
data$PoolQC = factor(data$PoolQC, levels=c(levels(data$PoolQC), 'None'))
data$PoolQC[is.na(data$PoolQC)] = 'None'

data$MiscFeature = factor(data$MiscFeature, levels=c(levels(data$MiscFeature), 'None'))
data$MiscFeature[is.na(data$MiscFeature)] = 'None'

data$Alley = factor(data$Alley, levels=c(levels(data$Alley), 'None'))
data$Alley[is.na(data$Alley)] = 'None'

data$Fence = factor(data$Fence, levels=c(levels(data$Fence), 'None'))
data$Fence[is.na(data$Fence)] = 'None'

data$GarageCond = factor(data$GarageCond, levels=c(levels(data$GarageCond), 'None'))
data$GarageCond[is.na(data$GarageCond)] = 'None'

data$GarageType = factor(data$GarageType, levels=c(levels(data$GarageType), 'None'))
data$GarageType[is.na(data$GarageType)] = 'None'

data$BsmtQual = factor(data$BsmtQual, levels=c(levels(data$BsmtQual), 'None'))
data$BsmtQual[is.na(data$BsmtQual)] = 'None'

data$BsmtCond = factor(data$BsmtCond, levels=c(levels(data$BsmtCond), 'None'))
data$BsmtCond[is.na(data$BsmtCond)] = 'None'

# Impute MasVnrArea with mean of feature
data$MasVnrArea[is.na(data$MasVnrArea)] <- mean(data$MasVnrArea, na.rm = TRUE)

# Impute LotFrontage with median of neigbourhood
data$LotFrontage[is.na(data$LotFrontage)] <- with(data, ave(LotFrontage, Neighborhood, 
                                                            FUN = function(x) median(x, na.rm = TRUE)))[is.na(data$LotFrontage)]

# Correlation Matrix
corr_data <- select_if(data, is.numeric)

corr <- round(cor(corr_data), 1)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


# Bar plots
bar1 <- ggplot(data, aes(x=Alley))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal()

bar2 <- ggplot(data, aes(x=HouseStyle))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal()

bar3 <- ggplot(data, aes(x=LotConfig))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal()

library('gridExtra')
g <-grid.arrange(bar1, bar2, bar3)

#Captures the data before one hot encoding for a later SVM model
svm_data <- data

# One Hot Encoding for Categorical variables
data <- one_hot(as.data.table(data))

# Group data by OverallCond
data$OverallCond <- cut(data$OverallCond, 
                        c(-Inf, 3, 6, Inf),
                        labels = c('Poor', 'Average', 'Good'))

# Create non ordinal OverallCond for purpose of logistic regression
data$OverallCond <- factor( data$OverallCond, ordered = FALSE )

######################### MODELLING - CONDITION (Q2) ######################### 

## Feature Selection
boruta_output2 <- Boruta(OverallCond ~ ., data=data, doTrace=0)

#prints confirmed and tentative variables
boruta_signif2 <- getSelectedAttributes(boruta_output2, withTentative = TRUE)
print(boruta_signif2)

#tests whether tentative variables are significant or not
roughFixMod2 <- TentativeRoughFix(boruta_output2)
boruta_signif2 <- getSelectedAttributes(roughFixMod2)
print(boruta_signif2)

#extracts confirmed important variables and orders them in descending order
impsx <- attStats(roughFixMod2)
impsx2 = impsx[impsx$decision != 'Rejected', c('meanImp', 'decision')]
impsx2[order(-impsx2$meanImp), ]

#plot of variable importance
plot(boruta_output2, cex.axis=.5, las=2, xlab="", main="Variable Importance for OverallCond") 

row.names(impsx2)

#subset of important variables only
OverallCondRelevantData = subset(data, select = c("OverallCond", "LotFrontage", "LotArea", "Alley_Grvl", "Neighborhood_BrkSide", "Neighborhood_CollgCr", "Neighborhood_Crawfor", "Neighborhood_NAmes", "Neighborhood_NWAmes", "Neighborhood_OldTown", "Neighborhood_SawyerW", "Condition1_PosA", "BldgType_1Fam", "BldgType_Duplex", "BldgType_TwnhsE", "HouseStyle_1.5Fin", "HouseStyle_2Story", "OverallQual", "YearBuilt", "Exterior1st_MetalSd", "Exterior1st_VinylSd", "Exterior1st_Wd Sdng", "MasVnrArea", "ExterQual_Ex", "ExterQual_Fa", "ExterQual_Gd", "ExterQual_TA", "ExterCond_Ex", "ExterCond_Fa", "ExterCond_Gd", "ExterCond_TA", "Foundation_BrkTil", "Foundation_CBlock", "Foundation_PConc", "BsmtQual_Ex", "BsmtQual_Gd", "BsmtQual_TA", "BsmtQual_None", "BsmtCond_Po", "BsmtCond_None", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "GrLivArea", "FullBath", "BedroomAbvGr", "KitchenAbvGr", "KitchenQual_Ex", "KitchenQual_Gd", "KitchenQual_TA", "TotRmsAbvGrd", "Functional_Maj2", "Functional_Typ", "Fireplaces", "GarageType_Attchd", "GarageType_Detchd", "GarageArea", "PavedDrive_Y", "Fence_MnPrv", "Fence_None", "SaleType_WD", "SaleCondition_Normal", "SaleCondition_Partial", "SalePrice"))

## Train/Test Split

train <- sample(nrow(OverallCondRelevantData), 0.7*nrow(OverallCondRelevantData), replace = FALSE)
cond_training <- OverallCondRelevantData[train,]
cond_testing <- OverallCondRelevantData[-train,]

## Logistic Regression

cond_training$OverallCond <-relevel(cond_training$OverallCond, ref="Good")

#create repeated kfold cross validation
cond_train.control <- trainControl(method = "cv", 
                              number = 10)

model <- train(OverallCond~., data = cond_training, method = "multinom", 
               trControl = cond_train.control)


cond_training$predicted <- predict(model, newdata = cond_training) #predict values for the train dataset

cond_train_table <- table(cond_training$OverallCond, cond_training$predicted) #classification table to compare predicted values v actual values

confusionMatrix(cond_training$OverallCond, cond_training$predicted) 


cond_testing$predicted <- predict(model, newdata = cond_testing, "raw")


confusionMatrix(cond_testing$OverallCond, cond_testing$predicted)

## Random Forest
model = randomForest(cond_training$OverallCond ~ ., data = cond_training ,importance = TRUE, trControl = cond_train.control)

model


predTest <- predict(model, cond_testing, type = "raw")
table(predTest, cond_testing$OverallCond)
mean(predTest == cond_testing$OverallCond)

confusionMatrix(predTest,cond_testing$OverallCond)

######################### MODELLING - HOUSE PRICES (Q3a) ##########################
## Feature Selection

boruta_output <- Boruta(SalePrice ~ ., data=svm_data, doTrace=0)

#prints confirmed and tentative variables
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

#tests whether tentative variables are significant or not
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#extracts confirmed important variables and orders them in descending order
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
imps2[order(-imps2$meanImp), ]

#plot of variable importance
plot(boruta_output, cex.axis=.5, las=2, xlab="", main="Variable Importance for SalePrice") 

row.names(imps2)

#subset of important variables only
SalePriceRelevantData = subset(data, select = c("SalePrice", "LotFrontage", "LotArea", "Alley_Grvl", "Alley_Pave", "Alley_None", "Neighborhood_BrDale", "Neighborhood_ClearCr", "Neighborhood_CollgCr", "Neighborhood_Crawfor", "Neighborhood_Edwards", "Neighborhood_Gilbert", "Neighborhood_IDOTRR", "Neighborhood_MeadowV", "Neighborhood_NAmes", "Neighborhood_NoRidge", "Neighborhood_NPkVill", "Neighborhood_NridgHt", "Neighborhood_NWAmes", "Neighborhood_OldTown", "Neighborhood_Sawyer", "Neighborhood_Somerst", "Condition1_Norm", "BldgType_1Fam", "BldgType_Duplex", "BldgType_Twnhs", "BldgType_TwnhsE", "HouseStyle_1.5Fin", "HouseStyle_1Story", "HouseStyle_2Story", "HouseStyle_SLvl", "OverallQual", "OverallCond", "YearBuilt", "RoofStyle_Hip", "Exterior1st_HdBoard", "Exterior1st_MetalSd", "Exterior1st_Plywood", "Exterior1st_VinylSd", "MasVnrArea", "ExterQual_Ex", "ExterQual_Fa", "ExterQual_Gd", "ExterQual_TA", "Foundation_BrkTil", "Foundation_CBlock", "Foundation_PConc", "Foundation_Slab", "BsmtQual_Ex", "BsmtQual_Fa", "BsmtQual_Gd", "BsmtQual_TA", "BsmtQual_None", "BsmtCond_Fa", "BsmtCond_None", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "GrLivArea", "FullBath", "BedroomAbvGr", "KitchenAbvGr", "KitchenQual_Ex", "KitchenQual_Fa", "KitchenQual_Gd", "KitchenQual_TA", "TotRmsAbvGrd", "Functional_Typ", "Fireplaces", "GarageType_Attchd", "GarageType_BuiltIn", "GarageType_Detchd", "GarageType_None", "GarageArea", "GarageCond_TA", "GarageCond_None", "PavedDrive_N", "PavedDrive_Y", "SaleType_New", "SaleCondition_Partial"))

## Train/Test Split

set.seed(4595)
#Create train/test split on the relevant data only
train_test_split <- initial_split(SalePriceRelevantData, strata = "SalePrice", prop = 0.8)

#Create training and test sets
price_train <- training(train_test_split)
price_test  <- testing(train_test_split)

## Random Forest

#Set model hyperparameters
rf_model <- rand_forest(mode = "regression", mtry = 10, min_n = 3, trees = 2000)

#Train the model by fitting X and Y and performing log transformation on SalePrice
rf_fit_model <- 
  rf_model %>%
  fit_xy(
    x = price_train[,-1],
    y = log10(price_train$SalePrice)
  )

#Print model information
rf_fit_model

#Use the model to predict on the test set, applying log transformation to test set
rf_model_results <- 
  price_test %>%
  dplyr::select(SalePrice) %>%
  mutate(SalePrice = log10(SalePrice)) %>%
  bind_cols(
    predict(rf_fit_model, new_data = price_test[,-1])
  )

#Print the first ten rows of ground truth and model prediction
rf_model_results %>% slice(1:10)
#Print RMSE, MAE and R-Squared metrics to evaluate the model
rf_model_results %>% metrics(truth = SalePrice, estimate = .pred)

#Inverse the log transformation and print results
rf_inverse_log = 10^rf_model_results
rf_inverse_log %>% slice(1:10)
rf_inverse_log %>% metrics(truth = SalePrice, estimate = .pred)

## SVM

set.seed(4595)

#Creates a test/train split using the SVM data without one hot encoding
train_test_split <- initial_split(svm_data, strata = "SalePrice", prop = 0.8)

#Create training and test sets specifically for SVM
svr_price_train <- training(train_test_split)
svr_price_test  <- testing(train_test_split)

#Center and scale all numeric values (except Sale Price) using the training data
svr_scale <-
  recipe(SalePrice ~ ., data = svr_price_train) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep()

#Extract the scaled training data
price_train_scaled = juice(svr_scale)
#Fit and transform the test set using the scaling metrics from the training set
price_test_scaled = bake(svr_scale, svr_price_test)

#Selecting SVM radial basis function and hyperparameters
svm_model <- svm_rbf(mode = 'regression', cost = 10, rbf_sigma = 0.1)

#Fitting model to X_train, Y_train and taking log of Sale Price for better predictions
svm_fit_model <- 
  svm_model %>%
  fit_xy(
    x = price_train_scaled[,0:49],
    y = log10(price_train$SalePrice)
  )

#Model fit results
svm_fit_model

#Use the model to predict on the test set, applying log transformation to test set
svm_model_results <- 
  price_test %>%
  dplyr::select(SalePrice) %>%
  mutate(SalePrice = log10(SalePrice)) %>%
  bind_cols(
    predict(svm_fit_model, new_data = price_test_scaled[,0:49])
  )

#Print the first ten rows of ground truth and model prediction
svm_model_results %>% slice(1:10)

#Print RMSE, MAE and R-Squared metrics to evaluate the model
svm_model_results %>% metrics(truth = SalePrice, estimate = .pred)

#Inverse the log transformation and print results
svm_inverse_log = 10^svm_model_results
svm_inverse_log %>% slice(1:10)
svm_inverse_log %>% metrics(truth = SalePrice, estimate = .pred)

######################### RESAMPLING (Q3b) #########################

## Cross Validation

folds <- cvFolds(NROW(data), K=10) #creates 10 folds from the data

rf_model <- rand_forest(mode = "regression", mtry = 10, min_n = 3, trees = 2000) #our model

rfallresults <- data.frame(metric = c('rmse', 'rsq', 'mae')) #creates dataframe to save results in

for(i in 1:10){
  data_train <- SalePriceRelevantData[folds$subsets[folds$which != i], ] #Set the training set
  data_test <- SalePriceRelevantData[folds$subsets[folds$which == i], ] #Set the validation set
  
  rf_fit_model <- 
    rf_model %>%
    fit_xy(
      x = data_train[,-1],
      y = log10(data_train$SalePrice)
    )
  
  rf_model_results <- 
    data_test %>%
    dplyr::select(SalePrice) %>%
    mutate(SalePrice = log10(SalePrice)) %>%
    bind_cols(
      predict(rf_fit_model, new_data = data_test[,-1])
    )
  
  # Inverse the log transformation and print results
  rf_inverse_log = 10^rf_model_results
  results <- rf_inverse_log %>% metrics(truth = SalePrice, estimate = .pred) #saves results of this fold
  rfallresults <- cbind(rfallresults, results[,3]) #appends the results to the dataframe
}

rfallresults 

# Calculate the mean of each fold
rmsemean <- rowMeans(rfallresults[1,-1])
rsqmean <- rowMeans(rfallresults[2,-1])
maemean <- rowMeans(rfallresults[3,-1])

# Final table displaying mean of each metric for all folds
rfmeanresults <- data.frame(metric = c('rmse', 'rsq', 'mae'), mean = c(format(rmsemean, digits = 3), format(rsqmean, digits = 3), format(maemean, digits = 3)))
rfmeanresults


## Support Vector Regression

# Selecting SVM radial basis function and hyperparameters
folds <- cvFolds(NROW(svm_data), K=10) #creates 10 folds from the data

svm_model <- svm_rbf(mode = 'regression', cost = 10, rbf_sigma = 0.1)

svmallresults <- data.frame(metric = c('rmse', 'rsq', 'mae')) #creates dataframe to save results in

for(i in 1:10){
  data_train <- svm_data[folds$subsets[folds$which != i], ] #Set the training set
  data_test <- svm_data[folds$subsets[folds$which == i], ] #Set the validation set
  
  svm_fit_model <- 
    svm_model %>%
    fit_xy(
      x = data_train[,0:49],
      y = log10(data_train$SalePrice)
    )
  
  svm_model_results <- 
    data_test %>%
    dplyr::select(SalePrice) %>%
    mutate(SalePrice = log10(SalePrice)) %>%
    bind_cols(
      predict(svm_fit_model, new_data = data_test[,0:49])
    )
  
  # Inverse the log transformation and print results
  svm_inverse_log = 10^svm_model_results
  results <- svm_inverse_log %>% metrics(truth = SalePrice, estimate = .pred) #saves results of this fold
  svmallresults <- cbind(svmallresults, results[,3]) #appends the results to the dataframe
}

svmallresults
# Calculate the mean of each fold
rmsemean <- rowMeans(svmallresults[1,-1])
rsqmean <- rowMeans(svmallresults[2,-1])
maemean <- rowMeans(svmallresults[3,-1])

# Final table displaying mean of each metric for all folds
svmmeanresults <- data.frame(metric = c('rmse', 'rsq', 'mae'), mean = c(format(rmsemean, digits = 3), format(rsqmean, digits = 3), format(maemean, digits = 3)))
svmmeanresults


## Bootstrapping - Random Forest
library(boot)

rf_model <- rand_forest(mode = "regression", mtry = 10, min_n = 3, trees = 2000) #our model

# Function that returns the rsquared score of the random forest method
rsqRF <- function(data, indices) {
  datasample <- data[indices,] #lets the bootstrap take a sample of the data
  
  train_test_split <- initial_split(datasample, strata = "SalePrice", prop = 0.8)
  data_train <- training(train_test_split)
  data_test  <- testing(train_test_split)
  
  rf_fit_model <- rf_model %>%
    fit_xy(
      x = data_train[,-1],
      y = log10(data_train$SalePrice)
    )
  
  rf_model_results <- 
    data_test %>%
    dplyr::select(SalePrice) %>%
    mutate(SalePrice = log10(SalePrice)) %>%
    bind_cols(
      predict(rf_fit_model, new_data = data_test[,-1])
    )
  
  rf_inverse_log = 10^rf_model_results
  metric <- rf_inverse_log %>% metrics(truth = SalePrice, estimate = .pred)
  rsq <- rowMeans(metric[2,3])
  return(rsq)
}

# Testing the function on a sample of the first 100 rows of the data
rsqRF(data, 1:100)

# Bootstrap with 1000 samples
resultsRF <- boot(data, rsqRF, R=1000)
resultsRF
boot.ci(resultsRF, type = "basic")
plot(resultsRF)

## Bootstrapping - SVM

svm_model <- svm_rbf(mode = 'regression', cost = 10, rbf_sigma = 0.1)

# Function that returns the rsquared score of the SVM method
rsqSVM <- function(svm_data, indices) {
  datasample <- svm_data[indices,] #lets the bootstrap take a sample of the data
  
  train_test_split <- initial_split(datasample, strata = "SalePrice", prop = 0.8)
  data_train <- training(train_test_split)
  data_test  <- testing(train_test_split)
  
  svm_fit_model <- svm_model %>%
    fit_xy(
      x = data_train[,1:49],
      y = log10(data_train$SalePrice)
    )
  
  svm_model_results <- 
    data_test %>%
    dplyr::select(SalePrice) %>%
    mutate(SalePrice = log10(SalePrice)) %>%
    bind_cols(
      predict(svm_fit_model, new_data = data_test[,1:49])
    )
  
  svm_inverse_log = 10^svm_model_results
  metric <- svm_inverse_log %>% metrics(truth = SalePrice, estimate = .pred)
  rsq <- rowMeans(metric[2,3])
  return(rsq)
}


# Bootstrap with 1000 samples
resultsSVM <- boot(svm_data, rsqSVM, R=1000)
resultsSVM
boot.ci(resultsSVM, type = "basic")
plot(resultsSVM)

######################### MODELLING - YEAR BUILT (Q4) ######################### 

## Feature Selection

boruta_output3 <- Boruta(YearBuilt ~ ., data=data, doTrace=0)

#prints confirmed and tentative variables
boruta_signif3 <- getSelectedAttributes(boruta_output3, withTentative = TRUE)
print(boruta_signif3)

#tests whether tentative variables are significant or not
roughFixMod3 <- TentativeRoughFix(boruta_output3)
boruta_signif3 <- getSelectedAttributes(roughFixMod3)
print(boruta_signif3)

#extracts confirmed important variables and orders them in descending order
impsxx <- attStats(roughFixMod3)
impsx3 = impsxx[impsxx$decision != 'Rejected', c('meanImp', 'decision')]
impsx3[order(-impsx3$meanImp), ]

#plot of variable importance
plot(boruta_output3, cex.axis=.5, las=2, xlab="", main="Variable Importance for YearBuilt") 

row.names(impsx3)

## Train Test Split
YearBuiltRelevantData = subset(data, select = c("YearBuilt", "LotFrontage", "LotArea", "Alley_Grvl", "Alley_Pave", "Alley_None", "LotConfig_CulDSac", "LotConfig_Inside", "Neighborhood_BrDale", "Neighborhood_BrkSide", "Neighborhood_ClearCr", "Neighborhood_CollgCr", "Neighborhood_Crawfor", "Neighborhood_Gilbert", "Neighborhood_IDOTRR", "Neighborhood_MeadowV", "Neighborhood_Mitchel", "Neighborhood_NAmes", "Neighborhood_NoRidge", "Neighborhood_NridgHt", "Neighborhood_NWAmes", "Neighborhood_OldTown", "Neighborhood_Sawyer", "Neighborhood_SawyerW", "Neighborhood_Somerst", "Neighborhood_SWISU", "Condition1_Artery", "Condition1_Norm", "Condition1_PosA", "BldgType_1Fam", "BldgType_2fmCon", "BldgType_Duplex", "BldgType_Twnhs", "BldgType_TwnhsE", "HouseStyle_1.5Fin", "HouseStyle_1Story", "HouseStyle_2.5Fin", "HouseStyle_2.5Unf", "HouseStyle_2Story", "HouseStyle_SFoyer", "HouseStyle_SLvl", "OverallQual", "OverallCond", "RoofStyle_Flat", "RoofStyle_Gable", "RoofStyle_Hip", "RoofMatl_CompShg", "Exterior1st_AsbShng", "Exterior1st_BrkFace", "Exterior1st_CemntBd", "Exterior1st_HdBoard","Exterior1st_MetalSd", "Exterior1st_Plywood", "Exterior1st_Stucco", "Exterior1st_VinylSd", "MasVnrArea", "ExterQual_Ex", "ExterQual_Gd", "ExterQual_TA", "ExterCond_Fa", "ExterCond_Gd", "ExterCond_TA", "Foundation_BrkTil", "Foundation_CBlock", "Foundation_PConc", "Foundation_Slab", "BsmtQual_Ex", "BsmtQual_Fa", "BsmtQual_Gd", "BsmtQual_TA", "BsmtQual_None", "BsmtCond_Fa", "BsmtCond_None", "TotalBsmtSF", "Heating_GasA", "Heating_GasW", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea", "FullBath", "BedroomAbvGr", "KitchenAbvGr", "KitchenQual_Ex", "KitchenQual_Fa", "KitchenQual_Gd", "KitchenQual_TA", "TotRmsAbvGrd", "Functional_Typ", "Fireplaces", "GarageType_Attchd", "GarageType_BuiltIn", "GarageType_Detchd", "GarageType_None", "GarageArea", "GarageCond_Fa", "GarageCond_TA", "GarageCond_None", "PavedDrive_N", "PavedDrive_P", "PavedDrive_Y", "Fence_MnPrv", "Fence_None", "SaleType_New", "SaleType_WD", "SaleCondition_Normal", "SaleCondition_Partial", "SalePrice"))

names(YearBuiltRelevantData)[names(YearBuiltRelevantData) == "YearBuilt"] <- "Target"


set.seed(4595)

train_test_split <- initial_split(YearBuiltRelevantData, prop = 0.8)

data_train <- training(train_test_split)
data_test  <- testing(train_test_split)

## Multiple Linear Regression

# Training
regressor_lm = lm(formula = Target ~ .,
                  data = data_train)
print(regressor_lm)

summary(regressor_lm)

# Testing
y_pred_lm = predict(regressor_lm, newdata = data_test)


Pred_Actual_lm <- as.data.frame(cbind(Prediction_lm = y_pred_lm, Actual_lm = data_test$Target))

# Plot 
gg.lm <- ggplot(Pred_Actual_lm, aes(Actual_lm, Prediction_lm )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual Year Built",
       y = "Predicted Year Built") +
  theme(plot.title = element_text(family = "Lucida Sans", face = "bold", size = (15)), 
        axis.title = element_text(family = "Lucida Sans", size = (10)))
gg.lm

#Calculate and print Mean Squared Error
MSE.lm <- sum((data_test$Target - y_pred_lm)^2)/nrow(data_test)
print(paste("Mean Squared Error (Multiple Linear Regression):", MSE.lm))

#Calculate and print R-Squared
R2.lm <- R2(data_test$Target, y_pred_lm, form = "traditional")
print(paste("R-square error  (Multiple Linear Regression):", R2.lm))



