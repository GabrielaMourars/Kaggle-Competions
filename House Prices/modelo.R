# predição do valor das casas usando R

# biblioteca de pacotes
library('tidyverse')

# importar o banco de dados
test <- read.csv("C:/Users/Gabriela/Desktop/Ciência de Dados/Kaggle/House Prices/house/test.csv")
train <- read.csv("C:/Users/Gabriela/Desktop/Ciência de Dados/Kaggle/House Prices/house/train.csv")

head(train)

#retirar a coluna id do dataset
dataset <- train %>% select(-Id)

#inspeção do banco de dados
glimpse(dataset)

# observar a distribuição de preços das casas

dataset %>% select(SalePrice) %>% summary()

#plotar a figura
dataset %>% select(SalePrice) %>% ggplot(aes(x = SalePrice)) +
  geom_histogram(aes(y=..density..),alpha=0.5, fill = 'green') +
  geom_density(alpha=0.6)

# Distribuição dos dados numéricos
table(sapply(dataset,class)) 

#novo dataset somente com as colunas numéricas
dataset_num <- dataset %>% select(where(is.numeric))

dataset_num %>% head()

# plotar a distribuição de todas as colunas
library(Hmisc)
hist.data.frame(dataset_num)

# preparação do dataset
#----------------------------------------------------------------------------
# transformar os parametros para os tipos corretos 
#dataset <- dataset %>% mutate_at(c('TM_FH', 'TM_AR', 'C_AR'), as.numeric)
fatores <- c('MSZoning', 'LandContour',
             'Street', 'Alley','LotShape',
             'Utilities',
             'LotConfig', 'LandSlope', 'Neighborhood',
             'Condition1', 'Condition2', 'BldgType', 
             'HouseStyle', 'OverallQual', 'OverallCond',
             'RoofStyle',
             'RoofMatl','Exterior1st', 'Exterior2nd', 
             'MasVnrType', 'ExterQual', 'ExterCond',
             'Foundation', 'BsmtQual', 'BsmtCond',
             'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',
             'Heating', 'HeatingQC', 'CentralAir', 'Electrical',
             'KitchenQual', 'Functional',
             'FireplaceQu','GarageType',
             'GarageFinish',
             'GarageQual', 'GarageCond', 'PavedDrive',
             'PoolQC', 'Fence', 'MiscFeature',
             'SaleType', 'SaleCondition')
dataset <- train %>% mutate_at(fatores, as.factor) %>% select(-Id)
glimpse(dataset)
# abordagem para os valores faltantes: pacote mice
# Multivariate Imputation by Chained Equations

# porcentagem de na por coluna
pMiss <- function(x){sum(is.na(x))/length(x)*100}
na <- apply(dataset,2,pMiss) %>% as.data.frame() %>% filter(. > 40) 


# as colunas MiscFeature, Fence, PoolQC, Alley apresentam mais de 50% dos valores
# como missing values e, portanto, serão retirados do banco de dados
dataset_clean <- dataset %>% select(- FireplaceQu, -MiscFeature, -Fence, -PoolQC,-Alley)

# o banco de dados já está separado em base de teste e de treino
# a validação do modelo é feita por cross - validation, com 10 folds e repetido 5 vezes

# treinamento do modelo
#----------------------------------------------------------------------------
library('caret')
# a performance dos modelos será avaliada pela métrica RMSE
fitControl <- caret::trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 5,
  ## Evaluate performance using 
  ## the following function
  summaryFunction = defaultSummary
)

# começar com o random forest
library('randomForest')

# grade para encontrar os melhores parâmetros
rfGrid <- expand.grid(mtry = c(1:10))

set.seed(123)
# ajuste do modelo no data set de treino para encontrar os melhores parâmetros
rf_model <- caret::train(SalePrice ~., data = dataset, 
                         method = "rf", 
                         trControl = fitControl, 
                         verbose = FALSE, 
                         tuneGrid = rfGrid,
                         #preProcess = "knnImpute",
                         na.action = na.roughfix,
                         ## Specify which metric to optimize
                         metric = "RMSE")
# não consegui plotar a árvore com o randomForest no caret =/

# avaliação do modelo 
plot(rf_model)

# variáveis importantes
varImp(rf_model) %>% plot(top = 10)
rf_model$bestTune
id <- test %>% select(Id)
glimpse(test)

teste <- test %>%  select(-Id)%>% mutate_at(fatores, as.factor) 

# cálculo da predição e métricas de performance

rf.pred <- predict(rf_model,teste, na.action=na.roughfix) 

sample_submission <- read.csv("C:/Users/Gabriela/Desktop/Ciência de Dados/Kaggle/House Prices/house/sample_submission.csv")
submission <- sample_submission 
submission$SalePrice <- rf.pred
submission$SalePrice %>% class
library(data.table)
fwrite(df, "/Users/admin/new_file.csv")
write.csv(submission, "submission.csv",eol = "\n", row.names = FALSE)

?write.csv
#------------------------------------------------------------------
# tentar de outro jeito
rf <- randomForest(SalePrice ~., data=dataset,
                   na.action=na.roughfix) 
print(rf)
pred <- predict(rf,test)

#Error in predict.randomForest(rf, test) : 
#Type of predictors in new data do not match that of the training data.
dataset %>% select(where(is.factor)) %>% summary
