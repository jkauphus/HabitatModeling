#Jack Kauphusman 
#GLM Suitability Modelling 

# Packages
packages<-c("raster", "rgdal", "maptools","rgbif", "dplyr", "ggplot2", "sf","sp", "gistr", "dismo", "smoothr", "adehabitatHR",
            "ggmap", "GISTools", "ggspatial", "maps", "devtools", "ggsn", "rgeos", "rJava")
sapply(packages, require, character.only=T)

library(pROC)
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

#GLM Model
m1= glm(pb~., data = training)
summary(m1)

#Caret
# defining training control-->  10fold cv
train_control = trainControl(method = 'cv', number=10)
mod_fit=train(pb~., data = training, trControl=train_control, method='glm', family='binomial')

summary(mod_fit)

### Importance Metric
varImp(mod_fit)

## Test Model
p1<-predict(mod_fit, newdata=testing)
p2<-predict(mod_fit, newdata=testing, type='raw')

roc.glmModel = pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc.glmModel)

plot(roc.glmModel)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

## build an SDM
#Exent
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


p1 = predict(stck, mod_fit) #use predict to implement the GLM model stored
#in mod_fit on the raster stack of our predictors
plot(p1,main="GLM Predictive Map")

p2 = predict(stck, m1) #use the basic GLM to predict 
#implement the GLM model stored
#in mod_fit on the raster stack of our predictors
plot(p2,main="GLM Predictive Map")

### remove land use as a predictor
names(stck)

s=dropLayer(stck,3)
names(s)

mod_fit2=train(pb~bio14+layer+bio17+bio5,data=training,trControl=train_control,method="glm",family="binomial")
#remove land use type
summary(mod_fit2)

p3 = predict(s, mod_fit2) #use predict to implement the GLM model stored
#in mod_fit on the raster stack of our predictors
plot(p3,main="GLM Predictive Map")
