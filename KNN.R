# Jack Kauphusman
# KNN

# Packages
packages<-c("raster", "rgdal", "maptools","rgbif", "dplyr", "ggplot2", "sf","sp", "gistr", "dismo", "smoothr", "adehabitatHR",
            "ggmap", "GISTools", "ggspatial", "maps", "devtools", "ggsn", "rgeos", "rJava")
sapply(packages, require, character.only=T)

# Pre processing

pa=read.csv("Pres_abs.csv")

head(pa)
pa<-pa[,-1]

set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

set.seed(825)

pb = as.factor(training$pb)

library(caret)

set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

head(training)

## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

mod_fit=train(pb~.,data=training,trControl=train_control,method="knn")

summary(mod_fit)

## importance of the different predictors
varImp(mod_fit)

## test the model
p1=predict(mod_fit, newdata=testing) #predict on the test data

#test model fit-auc
library(pROC)

roc.glmModel = pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc.glmModel)
auc

plot(roc.glmModel)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

p1 = predict(stck, mod_fit) #use predict to implement the MARS model stored
#in mod_fit on the raster stack of our predictors
plot(p1,main="kNN Predictive Map")
