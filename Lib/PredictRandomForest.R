predictRandomForest <- function(outputDir, 
                                trainingDf, 
                                completeDf, 
                                zones,
                                temperatures,
                                horizons,
                                nDataPoints = -1,
                                plotResult = FALSE,
                                saveResult = TRUE){
    stopifnot(require('rminer'))
    stopifnot(require('ranger'))
    source("Lib/strip.R")
    source("Lib/SavePredictions.R")

    #Identify where are the start and end of the prediction periods by shifting index of NA
    idxNaCases = !complete.cases(trainingDf)
    startPoints =  which(idxNaCases & !c(FALSE, head(idxNaCases, -1)) & c(tail(idxNaCases, -1), TRUE))
    endPoints = which(idxNaCases & c(TRUE, head(idxNaCases, -1)) & !c(tail(idxNaCases, -1), FALSE))
    startDates = trainingDf$DateTime[startPoints]
    endDates = trainingDf$DateTime[endPoints]
    nTestingPeriods = length(startDates)
    
    #Create Temperature and Time Features
    
    completeDf = completeDf %>% mutate(ToD = factor(hour(completeDf$DateTime)), 
                                       DoW = factor(wday(completeDf$DateTime)))
    
    #Build models and make predictions
    predictions = rep(list(trainingDf), max(horizons));
    season1 = 24; #Hourly seasonal
    season2 = 24*7; #Weekly seasonal
    for (zone in zones){
        #Find the best correlated temperature with current zone
        maxCor = -1
        bestTemp = temperatures[[1]]
        for (temp in temperatures){
            correlation = cor(completeDf[[zone]], completeDf[[temp]])
            if (correlation > maxCor){
                maxCor = correlation
                bestTemp = temp
            }
        }
        
        completeDf$T = completeDf[[bestTemp]]
        completeDf$SmoT = completeDf$T
        completeDf$SmoT = completeDf$T
        for (i in 2:length(completeDf$T)){
            completeDf$SmoT[i] = 0.15*completeDf$T[i] + 0.85*completeDf$SmoT[i-1]
        }
        
        for (h in horizons){
            LagList = unique(c(h, h+1, h+2, h+3, h+4, season1, season1*2, season1*3, season2, season2*2))
            lossedRows = max(LagList)
            featureDf = CasesSeries(completeDf[[zone]], W = LagList)
            for (feature in c("DateTime", "ToD", "DoW", "T", "SmoT")){
                featureDf[[feature]] = tail(completeDf[[feature]], -lossedRows)
            }

            for (period in seq(1, nTestingPeriods)){
                startDate = startDates[period]
                endDate = endDates[period]
                trainData = featureDf %>% filter(DateTime < startDate) %>% select (-DateTime)
                if (nDataPoints > 0){
                    trainData = tail(trainData, nDataPoints)
                }
                #Split randomForest into #cores processes, then combine using the given function
                #ptm = proc.time()
                
                #model = foreach(y=seq(NCores), .combine=combine) %dopar%{
                #            set.seed(y)
                #            randomForest(y ~., trainData, ntree = 400/NCores, sampsize=5000, 
                #                         do.trace=FALSE, norm.votes=FALSE, proximity=FALSE)}#, nodesize=10)}
                model = ranger(y ~ ., trainData, sample.fraction=min(5000/nrow(trainData), 1))
                #print(proc.time() - ptm)

                testData = featureDf %>% filter ((DateTime >= startDate) & (DateTime <= endDate)) %>% select (-DateTime)
                prediction = predict(model, testData)$predictions #Remove $predictions with randomForest
                predictions[[h]][(completeDf$DateTime >= startDate) & (completeDf$DateTime <= endDate), zone] = prediction
            }
        }
    }
    #Write out result, possibly plot result
    for (h in horizons){
        if (saveResult){
            savePredictions("randomforest", predictions, horizons, outputDir)
        }
        if (plotResult){
            pdf(paste0(outputDir, "Visualizations/", "randomforest_horizon_", as.character(h), ".pdf"),width=7,height=5)
            zoneDf = predictions[[h]]
            for (zone in zones){
                strip(x = zoneDf[[zone]], 
                      date = zoneDf$DateTime,
                      cond = year(zoneDf$DateTime),
                      arrange = "wide",
                      main = paste("RandomForest Prediction", zone, "horizon", h))
            }
            dev.off()
        }
    }
    return (predictions)
}
#Create and alias, since randomForest in ranger package is already parallel
predictRandomForecastParallel = predictRandomForest