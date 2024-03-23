#install.packages("maxnet")
#install.packages("terra")
#install.packages("predicts")

#load needed packages
library(maxnet) #for making models
library(terra) #for working w raster files
library(predicts) #for analyzing models

coral<- read.csv("coral.csv") #access coral locations
coral <- coral[,2:3] #subset the x and y coordinates, omit ID column

#crs(coral) <- "epsg:32629"

#next, list the input files as batch process
env_vars <- rast(list.files("/Users/kevinbuck/Desktop/AdvancedGIS/Lab5/CoralMaxent/Input",
                            pattern= ".asc" , #only extracting .asc files
                            full.names = T))
#plot(env_vars[[4]]) #plot one of the environmental input rasters
set.seed(123) #be able to randomly sample background same way 
backgound <- spatSample(env_vars, #spatially sampling the input rasters
                        #next line takes 1000 random points, eliminates NA values
                        #and returns the result as a raster (points)
                        size= 1000, "random", na.rm=TRUE,as.points=T
                        ) #note the output is a raster
#points(coral) #plots the points of known coral presence
presvals <- extract(env_vars,coral) #take input conditions at presence locations
presvals <- presvals[,-1] #taking off the id column
backvals <- values(backgound) #save data of the random bkgd points

#making a df with presence absence associating input conditions w presence and
#absence from the presvals and random backgd vals data
pb <- c(rep(1,nrow(presvals)), rep(0,nrow(backvals)))
sdmdata <- data.frame(cbind(pb,rbind(presvals,backvals)))

#head(sdmdata).    #look at all the data
#tail(sdmdata)
#summary(sdmdata)
#pairs(sdmdata[,2:10], cex=0.1)

sdmdata <- na.omit(sdmdata) #get rid of NA data
#make a maxent model with the data
#syntax is maxnet([species occurrence], [inputs] )
mod <- maxnet(sdmdata[,1], sdmdata[,-1])

#look at plot of the model response curves
plot(mod)

#create a prediction from the model
prediction <- predict(env_vars, #input raster stack
                      mod, #model to use
                      clamp=F,#extrapolate, dont just interpolate
                      type="cloglog", #prediction type is essentially a  probability surface
                      na.rm=T)
plot(prediction) #look at predition
#save prediciton
writeRaster(prediction,"Maxnet_prediction_example.tif")

#checking how model fits given vector of presence, and vector of abs vals
model_evaluation <- pa_evaluate(sdmdata[sdmdata==1,],
                                        sdmdata[sdmdata==0,],
                                        mod)
#looking at receiver operator curve of model
plot(model_evaluation, "ROC")