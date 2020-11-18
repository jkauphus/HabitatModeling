# Jack Kauphusman
# Resize the Raster Data

land = raster("SDM_Data/landuse1.tif")
alt = raster("SDM_Data/altitude.tif")

land
plot(land)

# Resampling cells to match the resolution of another

landC <- resample(land, alt, method = "bilinear")

## first arguement is the raster whose spatial resolution we want to change
## second raster is the one whose resolution we want to apply on 1

plot(landC)

landC


