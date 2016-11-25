#Parallelize over zones
predictAverageARIMABaselineParallel <- function(outputDir, 
                        trainingDf, 
                        completeDf, 
                        zones,
                        horizons,
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
                    predictAverageARIMABaseline(outputDir, trainingDf, completeDf, zones, horizons, 
                                                plotResult = FALSE, saveResult = FALSE)
    stopImplicitCluster()
    
    if (saveResult){
        saveResult(predictions, horizons, outputDir)
    }
}

#Function for make prediction
predictAverageARIMABaseline <- function(outputDir, 
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
    
    
    #xtsDf = xts(x = completeDf[, -1], order.by = completeDf[, 1])
    maxHorizon = max(horizons)
    maxPoint = nrow(trainingDf)
    oneweek = 7*24
    #Build models and make predictions
    featureDf = completeDf
    featureDf$Residuals = rep(0, nrow(featureDf))
    predictions = rep(list(trainingDf), max(horizons))
    for (zone in zones){
        for (period in seq(1, nTestingPeriods)){
            startPoint = startPoints[period]
            endPoint = endPoints[period]
            #Make Average Prediction
            idxTestHours = startPoint:endPoint
            arimaTraingSize = 12*7*24 # 3 months
            startTrainingPoint = startPoint - arimaTraingSize #Only get 3 months of data for training
            idxTrainHours = startTrainingPoint:(startPoint-1)
            idxTotal = c(idxTrainHours, idxTestHours)
            idxOneWeekBefore = idxTotal - oneweek
            idxTwoWeekBefore = idxTotal - 2*oneweek
            idxThreeWeekBefore = idxTotal - 3*oneweek
            idxFourWeekBefore = idxTotal - 4*oneweek
            predictorsDf = data.frame(featureDf[idxOneWeekBefore, zone],
                                      featureDf[idxTwoWeekBefore, zone], 
                                      featureDf[idxThreeWeekBefore, zone],
                                      featureDf[idxFourWeekBefore, zone])
            prediction = rowMeans(predictorsDf, na.rm = TRUE)
            for (h in horizons){ #prediction is the same for all
                predictions[[h]][idxTotal, zone] = prediction
            }
            featureDf$Residuals[idxTotal] = featureDf[idxTotal, zone] - prediction #Residuals of average prediction
            #Run Arima here
            xts = xts(featureDf$Residuals, featureDf$DateTime)
            trainXts = xts[idxTrainHours]
                #model = auto.arima(trainXts, num.cores=8, parallel=TRUE)
                #model = Arima(trainXts, order = c(3, 0, 3))
            model = Arima(trainXts, order = c(3, 0, 3), seasonal=list(order=c(3, 0, 3), period = 24))
            #order = arimaorder(model) Maybe need to save the order to reduce computation
            testXts = trainXts
            for (currentPoint in seq(startPoint, endPoint)){
                refit = Arima(testXts, model=model)
                prediction = forecast(refit, h=maxHorizon)$mean
                for (h in horizons){
                    if (currentPoint+h-1 <= endPoint){
                       predictions[[h]][currentPoint+h-1, zone] = predictions[[h]][currentPoint+h-1, zone] + prediction[h]
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
        csvFile = paste0(outputDir, "averageARIMA_horizon_", as.character(h), ".csv")
        write.csv(predictions[[h]], csvFile, row.names=FALSE)
    }
}