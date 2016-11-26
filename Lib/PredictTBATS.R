#Parallelize over zones
predictTBATSParallel <- function(outputDir, 
                                trainingDf, 
                                completeDf, 
                                zones,
                                horizons, 
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
                         predictTBATS(outputDir, trainingDf, completeDf, 
                                zones, horizons,  plotResult, saveResult = FALSE)
    stopImplicitCluster()
    
    if (saveResult){
        savePredictions("TBATS", predictions, horizons, outputDir)
    }
}
    
predictTBATS <- function(outputDir, 
                        trainingDf, 
                        completeDf, 
                        zones,
                        horizons,
                        plotResult = FALSE,
                        saveResult = FALSE){
    
    stopifnot(require("forecast"))
    stopifnot(require("xts"))
    source("Lib/SavePredictions.R")
    #Setup loging file
    source("Lib/SetupLog.R")

    #Extract testing period
    idxNaCases = !complete.cases(trainingDf)
    startPoints =  which(idxNaCases & !c(FALSE, head(idxNaCases, -1)) & c(tail(idxNaCases, -1), TRUE))
    endPoints = which(idxNaCases & c(TRUE, head(idxNaCases, -1)) & !c(tail(idxNaCases, -1), FALSE))
    startDates = trainingDf$DateTime[startPoints]
    endDates = trainingDf$DateTime[endPoints]
    nTestingPeriods = length(startDates)
        
    #Start prediction
    xtsDf = xts(x = completeDf[, -1], order.by = completeDf[, 1])
    maxHorizons = max(horizons)
    #Build models and make predictions
    predictions = rep(list(trainingDf), max(horizons));
    season1 = 24; #Hourly seasonal
    season2 = 24*7; #Weekly seasonal
    for (zone in zones){
        xts = xtsDf[, zone]
        for (period in seq(1, nTestingPeriods)){
            startTime = Sys.time()

            startPoint = startPoints[period]
            endPoint = endPoints[period]
            startTrainingPoint = startPoint - 12*season2 #Only get 3 months of data for training
            trainXts = xts[startTrainingPoint:(startPoint-1)]
            model = tbats(drop(coredata(trainXts)), seasonal.periods = 24, use.box.cox = TRUE, num.cores = 8)
            testXts = trainXts
            for (currentPoint in seq(startPoint, endPoint)){
                refit = tbats(drop(coredata(testXts)), model=model)
                prediction = forecast(refit, h=maxHorizons)$mean
                for (h in horizons){
                    if (currentPoint+h-1 <= endPoint){
                       predictions[[h]][currentPoint+h-1, zone] = prediction[h]
                    }
                }            
                testXts = c(testXts, xts[currentPoint])
            }
            prettyPrint(paste0("TBATS|", zone, "|period ", period, "|Done in ", (Sys.time()-startTime)[[1]]));
        }
    }
    
    if (saveResult){
        for (h in horizons){
            savePredictions("TBATS", predictions, horizons, outputDir)
        }
    }
    return (predictions)
}