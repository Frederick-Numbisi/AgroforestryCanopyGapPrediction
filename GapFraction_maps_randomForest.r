#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# CREATE SPATIALLY-EXPLICIT GAP FRACTION MAPS BASED ON THE RANDOM FOREST ALGORITHM

library(ggplot2)
library(MASS)
library(caret)

library(haven)
library(dplyr)
library(leaps)
library(tidyverse)



setwd("path/to/the/folder/with/GapFractionData/") # set the working directory



#install.packages("ModelMap")

library("ModelMap")
Data = read.csv("BokEfoul_Gamma0GF.csv") 

Data2 = subset(Data, select = -c(PlotID, luse, no.of.gaps,	gap.area))

# Separate training and test dataset and run RF model

set.seed(42)

ind = sample(2, nrow(Data2), replace = TRUE, prob = c(0.7, 0.3)) # partition date in ratio 70% and 30%
Data2_train= Data2[ind ==1,]
Data2_test= Data2[ind ==2,]
write.csv(Data2_train, file = 'Data2_train.csv')
write.csv(Data2_test, file = 'Data2_test.csv')

model.type <- "RF" # specify the type of model

folder <- getwd() # for the output


# Split the data into training and test data sets

get.test( proportion.test=0.3, # 30% of data subset as test data
          qdatafn=Data2,
          seed=42,
          folder=folder,
          qdata.trainfn="Data2_train.csv",
          qdata.testfn="Data2_test.csv")



MODELfn.a <- "VModelMapEx1a"


predList <- c( "Gamma0VH",
               "Gamma0VV",
               "Gamma0VHdb",
               "Gamma0VVdb",
               "Gamma0VV.VH",
               "VH.VVonVH.VV",
               "VH.VV",
               "VVVHratio",
               "VHVVratio",
               "VV.VHonVV.VH")

predFactor <- FALSE

#Define the response variable, and whether it is continuous, binary or categorical.
response.name.a <- "gap.fraction"
response.type <- "continuous"


#Define the seeds for each model.
seed.a <- 38


#Define the column that contains unique identifiers for each data point. These identifiers will be
#used to label the output file of observed and predicted values when running model validation.

unique.rowname <- "ID"


model.obj.ex1a <- model.build( model.type=model.type,
                               qdata.trainfn="Data2_train.csv",
                               folder=folder,
                               unique.rowname=unique.rowname,
                               MODELfn=MODELfn.a,
                               predList=predList,
                               predFactor=predFactor,
                               response.name=response.name.a,
                               response.type=response.type,
                               seed=seed.a)


model.pred.ex1a <- model.diagnostics( model.obj=model.obj.ex1a,
                                      qdata.testfn="Data2_test.csv",
                                      folder=folder,
                                      MODELfn=MODELfn.a,
                                      unique.rowname=unique.rowname,
                                      # Model Validation Arguments
                                      prediction.type="TEST",
                                      device.type=c("pdf"),
                                      cex=1.2)


rastLUTfn <- "BokEfoul_Gamma0GF_LUT_B1.csv"
rastLUTfn <- read.table( rastLUTfn,
                         header=FALSE,
                         sep=",",
                         stringsAsFactors=FALSE)
rastLUTfn[,1] <- paste(folder,rastLUTfn[,1],sep="/")


model.mapmake( model.obj=model.obj.ex1a,
               folder=folder,
               MODELfn=MODELfn.a,
               rastLUTfn=rastLUTfn,
               na.action="na.omit",
               # Mapping arguments
               map.sd=TRUE)



VModelMapEx1a_map.img

l = seq(100,0,length.out=101)
c = seq(0,100,length.out=101)
col.ramp <- hcl(h = 120, c = c, l = l)

mapgrid.a <- raster(paste(MODELfn.a,"_map.img",sep=""))


rastLUTfn <- "BokEfoul_Gamma0GF_LUT_M2.csv"
rastLUTfn <- read.table( rastLUTfn,
                         header=FALSE,
                         sep=",",
                         stringsAsFactors=FALSE)
rastLUTfn[,1] <- paste(folder,rastLUTfn[,1],sep="/")


model.mapmake( model.obj=model.obj.ex1a,
               folder=folder,
               MODELfn=MODELfn.a,
               rastLUTfn=rastLUTfn,
               na.action="na.omit",
               # Mapping arguments
               map.sd=TRUE)

mapgrid.bok2 <- raster(paste(MODELfn.a,"_map.img",sep=""))



#zlim <- c(0,max(maxValue(mapgrid.a)))
#legend.label<-rev(pretty(zlim,n=5))
legend.colors<-col.ramp[trunc((legend.label/max(legend.label))*100)+1]
legend.label<-paste(legend.label,"%",sep="")
image( mapgrid.a,
       col=col.ramp,
       xlab="",ylab="",xaxt="n",yaxt="n",
       #zlim=zlim,
       25,
       asp=1,bty="n",main="")



model.importance.plot( model.obj.1=model.obj.ex1a,
                       model.name.1="gap_fraction",
                       sort.by="predList",
                       predList=predList,
                       scale.by="sum",
                       main="Variable Importance",
                       device.type="pdf",
                       PLOTfn="VModelMapEx1CompareImportance",
                       folder=folder)











