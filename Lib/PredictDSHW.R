#Parallelize over zones
predictDSHWParallel <- function(outputDir, 
                        trainingDf, 
                        completeDf, 
                        zones,
                        horizons,
                        modifiedDSHW = FALSE,
                        NCores = 8,
                        plotResult = FALSE,
                        saveResult = TRUE){
    stopifnot(require("doParallel"))
    stopifnot(require("foreach"))
    source("Lib/SavePredictions.R")
    registerDoParallel(NCores)
    
    predictions = foreach(zones = zones, 
                          .combine=function(pred1, pred2) combinePredictions(horizons, zones, pred1, pred2),
                          .errorhandling="remove") %dopar% 
                          predictDSHW(outputDir, trainingDf, completeDf, zones, horizons, modifiedDSHW, 
                                                plotResult = FALSE, saveResult = FALSE)
    stopImplicitCluster()
    
    methodName = ifelse(modifiedDSHW, "ModifiedDSHW", "OriginalDSHW")    
    if (saveResult){
        savePredictions(methodName, predictions, horizons, outputDir)
    }
}

predictDSHW <- function(outputDir, 
                        trainingDf, 
                        completeDf, 
                        zones,
                        horizons,
                        modifiedDSHW = FALSE,
                        plotResult = FALSE,
                        saveResult = FALSE){
    stopifnot(require("forecast"))
    stopifnot(require("xts"))
    source("Lib/SavePredictions.R")

    if (modifiedDSHW){
        source("Lib/ModifiedDSHW.R")
        methodName = "ModifiedDSHW"
    } else{
        methodName = "OriginalDSHW"
        source("Lib/OriginalDSHW.R")
    }
    #Extract testing period
    idxNaCases = !complete.cases(trainingDf)
    startPoints =  which(idxNaCases & !c(FALSE, head(idxNaCases, -1)) & c(tail(idxNaCases, -1), TRUE))
    endPoints = which(idxNaCases & c(TRUE, head(idxNaCases, -1)) & !c(tail(idxNaCases, -1), FALSE))
    startDates = trainingDf$DateTime[startPoints]
    endDates = trainingDf$DateTime[endPoints]
    nTestingPeriods = length(startDates)
    
    
    xtsDf = xts(x = completeDf[, -1], order.by = completeDf[, 1])
    maxHorizons = max(horizons)
    maxPoint = nrow(trainingDf)
    #Build models and make predictions
    predictions = rep(list(trainingDf), max(horizons));
    season1 = 24; #Hourly seasonal
    season2 = 24*7; #Weekly seasonal
    for (zone in zones){
        xts = xtsDf[, zone]
        for (period in seq(1, nTestingPeriods)){
            startPoint = startPoints[period]
            endPoint = endPoints[period]
            startTrainingPoint = startPoint - 12*season2 #Only get 3 months of data for training
            trainXts = xts[startTrainingPoint:(startPoint-1)]
            model = dshw(trainXts, season1, season2, h=season1)
            testXts = trainXts
            for (currentPoint in seq(startPoint, endPoint)){
                prediction = dshw(testXts, h=maxHorizons, model = model)$mean
                for (h in horizons){
                    if (currentPoint+h-1 <= endPoint){
                       predictions[[h]][currentPoint+h-1, zone] = prediction[h]
                    }
                }            
                testXts = c(testXts, xts[currentPoint])
            }
        }
    }
    if (saveResult){
        savePredictions(methodName, predictions, horizons, outputDir)
    }
    return (predictions)
}