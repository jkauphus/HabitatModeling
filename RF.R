# Jack Kauphusman
# Random Forest Habitat Modeling

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

mod_fit=train(pb~.,data=training,trControl=train_control,method="rf", importance = TRUE)

summary(mod_fit)

## importance of the different predictors
varImp(mod_fit)

## test the model
p1=predict(mod_fit, newdata=testing) #predict on the test data

#test model fit-auc
library(pROC)

roc= pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc)
auc

plot(roc)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

#Building an SDM

ext<-extent(-90.33333, -81.83333, 34.5, 36.83333)

# Elevation Predictors
srtm1<-getData(name = "SRTM", lat = 35, lon = -85, path = "./Data")
srtm2<-getData(name = "SRTM", lat = 35, lon = -90, path = "./Data")
srtm3<-getData(name = "SRTM", lat = 40, lon = -85, path = "./Data")
srtm4<-getData(name = "SRTM", lat = 40, lon = -90, path = "./Data")
srtm5<-getData(name = "SRTM", lat = 30, lon = -85, path = "./Data")
srtm6<-getData(name = "SRTM", lat = 30, lon = -90, path = "./Data")
mosaic_strm<-mosaic(srtm1,srtm2,srtm3, srtm4,srtm5,srtm6, fun = mean)

# Bioclim indicators
all.worldclim = raster::getData("worldclim", res = 10, var = 'bio')

#Cropping Rasters to match extant
elevation = setExtent(mosaic_strm, ext)
tn.worldclim = crop(all.worldclim, ext)

elevation
tn.worldclim

#Resampling to match cell size
elevation <- resample(elevation, tn.worldclim, method = "bilinear")

#Stacking Rasters
## MUST HAVE SAME EXTENT AND RESOLUTION
stck<-stack(elevation, tn.worldclim)


p = predict(stck, mod_fit) #use predict to implement the MARS model stored
#in mod_fit on the raster stack of our predictors
plot(p,main="RF Predictive Map")

#Test the impact of individual predictors
require(randomForest)
m1 <-randomForest(pb~., data = training)

#Partial Dependence Plots
partialPlot(m1, training, bio7, pb)

p2<-predict(stck, m1)

plot(p2, main = "RF Predictive Map")

m2<-randomForest(pb~., data = pa)
p3=predict(stck,m2)

plot(p3, main="RF Predictive Map-All")

### Otherways of assessing model accuracy
library(ModelMetrics)
p1=predict(m1, newdata=testing)

auc(testing$pb, p1)

#Confusion Matrix
confusionMatrix(testing$pb, p1, cutoff = 0.5)
overall=(119+11)/(119+8+4+11)
overall

logLoss(testing$pb, p1, distribution = "binomial")

