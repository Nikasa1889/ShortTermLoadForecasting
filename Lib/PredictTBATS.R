predictTBATS <- function(outputDir, 
                        trainingDf, 
                        completeDf, 
                        zones,
                        horizons,
                        plotResult = FALSE,
                        saveResult = FALSE){
    
    stopifnot(require("forecast"))
    stopifnot(require("xts"))
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
        }
    }
    
    if (saveResult){
        for (h in horizons){
                csvFile = paste0(outputDir, "TBATS_horizon_", as.character(h), ".csv")
                write.csv(predictions[[h]], csvFile, row.names=FALSE)
        }
    }
    return (predictions)
}