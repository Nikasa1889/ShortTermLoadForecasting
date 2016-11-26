##First read in the arguments listed at the command line
args=(commandArgs(TRUE))

##args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args)<3){
    print("Need 3 argument: methodNames, datasets, and nzones")
}else{
    for(i in 1:length(args)){
         eval(parse(text=args[[i]]))
    }
}

#Setup loging file
source("Lib/SetupLog.R")

library(tidyr)
library(dplyr)
source("Lib/PredictRandomForest.R")
source("Lib/PredictDSHW.R")
source("Lib/PredictSemiParametricArima.R")
source("Lib/PredictTBATS.R")
source("Lib/PredictAverageARIMABaseline.R")
NCores = 8


if ("Hvaler" %in% datasets) {
    Zones = paste0("subs.", seq(1, nzones))#Only test nzones
    Temperatures = c("T01")
    Horizons = seq(1, 24)
    #Data for Hvaler
    HvalerTrainingFile = "Hvaler/training_set.csv"
    HvalerCompleteFile = "Hvaler/imputed_complete.csv"
    OutputDir = "Hvaler/Predictions/"
    HvalerClasses = c('POSIXct', rep("numeric", 21))
    trainingDf = read.csv(HvalerTrainingFile, stringsAsFactors=FALSE, colClasses=HvalerClasses)
    completeDf = read.csv(HvalerCompleteFile, stringsAsFactors=FALSE, colClasses=HvalerClasses)
} 
if ("GEFCOM2012" %in% datasets){
    Zones = paste0("zone.", seq(1, nzones))
    Temperatures = c("T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","T11")
    Horizons = seq(1, 24)
    #Data for GEFCom2012
    GEFCom2012TrainingFile = "GEFCom2012/training_set.csv"
    GEFCom2012CompleteFile = "GEFCom2012/complete.csv"
    OutputDir = "GEFCom2012/Predictions/"
    GEFCom2012Classes = c('POSIXct', rep("numeric", 32))
    trainingDf = read.csv(GEFCom2012TrainingFile, stringsAsFactors=FALSE, colClasses=GEFCom2012Classes)
    completeDf = read.csv(GEFCom2012CompleteFile, stringsAsFactors=FALSE, colClasses=GEFCom2012Classes)
}

prettyPrint("Done loading data, start predicting")               
if ("AverageARIMA" %in% methodNames){
    prettyPrint(paste("Running", "AverageARIMA"))               
    time = system.time(predictAverageARIMABaselineParallel(OutputDir, trainingDf, completeDf, Zones, 
                                                       Horizons, NCores = NCores, saveResult = TRUE))
    prettyPrint(time)
}
if ("DSHW" %in% methodNames){
    prettyPrint(paste("Running", "DSHW"))  
    time = system.time(predictDSHWParallel(OutputDir, trainingDf, completeDf, Zones, 
                                       Horizons, modifiedDSHW=FALSE, saveResult = TRUE))
    prettyPrint(time)
}
if ("modifiedDSHW" %in% methodNames){
    prettyPrint(paste("Running", "modifiedDSHW"))  
    time = system.time(predictDSHWParallel(OutputDir, trainingDf, completeDf, Zones, 
                                       Horizons, modifiedDSHW=TRUE, NCores = NCores, saveResult = TRUE))
    prettyPrint(time)
} 
if ("SemiParametric" %in% methodNames){
    prettyPrint(paste("Running", "SemiParametric"))  
    time = system.time(predictSemiParametricArimaParallel(OutputDir, trainingDf, completeDf, 
                                                          Zones, Temperatures, Horizons, NCores = NCores, saveResult = TRUE))
    prettyPrint(time)
}
if ("TBATS" %in% methodNames){
    prettyPrint(paste("Running", "TBATS"))  
    time = system.time(predictTBATSParallel(OutputDir, trainingDf, completeDf, Zones, Horizons, NCores = NCores, saveResult=TRUE))
    prettyPrint(time)
} 

sink()