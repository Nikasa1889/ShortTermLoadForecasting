#Setup loging file
sink("RunExperiment.log", append = TRUE, type = c("output", "message"))

prettyPrint <- function (message){
    print(paste0(Sys.time(),": ", message))
}

##Run for Hvaler
library(tidyr)
library(dplyr)
source("Lib/PredictRandomForest.R")
source("Lib/PredictDSHW.R")
source("Lib/PredictSemiParametricArima.R")
source("Lib/PredictTBATS.R")
source("Lib/PredictAverageARIMABaseline.R")
#Predict for Hvaler
HvalerTrainingFile = "Hvaler/training_set.csv"
HvalerCompleteFile = "Hvaler/imputed_complete.csv"
OutputDir = "Hvaler/Predictions/"
HvalerClasses = c('POSIXct', rep("numeric", 21))
NCores = 8
Zones = paste0("subs.", seq(1, 8))#Only test 1 zone now
Temperatures = c("T01")
Horizons = seq(1, 24)
trainingDf = read.csv(HvalerTrainingFile, stringsAsFactors=FALSE, colClasses=HvalerClasses)
completeDf = read.csv(HvalerCompleteFile, stringsAsFactors=FALSE, colClasses=HvalerClasses)

prettyPrint("Done loading data, start predicting")
prettyPrint("Running AverageARIMA")
time = system.time(predictAverageARIMABaselineParallel(OutputDir, trainingDf, completeDf, Zones, 
                                                       Horizons, saveResult = TRUE))
print(time)
prettyPrint("---------Done--------")

prettyPrint("Running DSHW")
time = system.time(predictDSHWParallel(OutputDir, trainingDf, completeDf, Zones, 
                                       Horizons, modifiedDSHW=FALSE, saveResult = TRUE))
print(time)
prettyPrint("---------Done--------")

prettyPrint("Running SemiParametric")
time = system.time(predictSemiParametricParallel(OutputDir, trainingDf, completeDf, 
                                                 Zones, Horizons, modifiedDSHW=FALSE, saveResult = TRUE))
print(time)
prettyPrint("---------Done--------")

prettyPrint("Running TBATS")
time = system.time(predictTBATS(OutputDir, trainingDf, completeDf, Zones, Horizons, saveResult=TRUE))
print(time)
prettyPrint("---------Done--------")
sink()