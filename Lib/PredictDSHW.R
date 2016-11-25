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
    registerDoParallel(NCores)

    combinePredictions <- function (predictions1, predictions2){
        result = predictions1
        for (h in horizons){
            for (zone in zones){
                result[[h]][[zone]] = ifelse(!is.na(predictions1[[h]][[zone]]), predictions1[[h]][[zone]], predictions2[[h]][[zone]])
            }
        }
        return(result)
    }
    
    predictions = foreach(zones = zones, .combine=combinePredictions) %dopar% 
                    predictDSHW(outputDir, trainingDf, completeDf, zones, horizons, modifiedDSHW, 
                                                plotResult = FALSE, saveResult = FALSE)
    stopImplicitCluster()
    
    if (saveResult){
        saveResult(predictions, horizons, outputDir)
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
        saveResult(predictions, horizons, outputDir)
    }
    return (predictions)
}

saveResult <- function (predictions, horizons, outputDir){
    for (h in horizons){
                csvFile = paste0(outputDir, methodName, "_horizon_", as.character(h), ".csv")
                write.csv(predictions[[h]], csvFile, row.names=FALSE)
    }
}