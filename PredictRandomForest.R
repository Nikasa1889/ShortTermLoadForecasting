predictRandomForest <- function(outputDir, 
                                trainingDf, 
                                completeDf, 
                                zones, 
                                horizons, 
                                NCores = 8, 
                                PlotResult = TRUE){
    require('rminer')
    #require('randomForest')
    require('ranger')
    #library(doMC)
    source("strip.R")
    #registerDoMC(NCores)
    
    #Identify where are the start and end of the prediction periods by shifting index of NA
    idxNaCases = !complete.cases(trainingDf)
    startPoints =  which(idxNaCases & !c(FALSE, head(idxNaCases, -1)) & c(tail(idxNaCases, -1), TRUE))
    endPoints = which(idxNaCases & c(TRUE, head(idxNaCases, -1)) & !c(tail(idxNaCases, -1), FALSE))
    startDates = trainingDf$DateTime[startPoints]
    endDates = trainingDf$DateTime[endPoints]
    nTestingPeriods = length(startDates)
    
    #Create Temperature and Time Features
    completeDf$SmoT = completeDf$T01
    for (i in 2:length(completeDf$T01)){
        completeDf$SmoT[i] = 0.15*completeDf$T01[i] + 0.85*completeDf$SmoT[i-1]
    }
    completeDf = completeDf %>% mutate(ToD = factor(hour(completeDf$DateTime)), 
                                       DoW = factor(wday(completeDf$DateTime)))
    
    #Build models and make predictions
    predictions = rep(list(trainingDf), max(horizons));
    season1 = 24; #Hourly seasonal
    season2 = 24*7; #Weekly seasonal
    for (zone in zones){
        for (h in horizons){
            LagList = unique(c(h, h+1, h+2, h+3, h+4, season1, season1*2, season1*3, season2, season2*2))
            lossedRows = max(LagList)
            featureDf = CasesSeries(completeDf[[zone]], W = LagList)
            for (feature in c("DateTime", "ToD", "DoW", "T01", "SmoT")){
                featureDf[[feature]] = tail(completeDf[[feature]], -lossedRows)
            }

            for (period in seq(1, nTestingPeriods)){
                startDate = startDates[period]
                endDate = endDates[period]
                trainData = featureDf %>% filter(DateTime < startDate) %>% select (-DateTime)

                #Split randomForest into #cores processes, then combine using the given function
                #ptm = proc.time()
                
                #model = foreach(y=seq(NCores), .combine=combine) %dopar%{
                #            set.seed(y)
                #            randomForest(y ~., trainData, ntree = 400/NCores, sampsize=5000, 
                #                         do.trace=FALSE, norm.votes=FALSE, proximity=FALSE)}#, nodesize=10)}
                model = ranger(y ~ ., trainData, sample.fraction=5000/nrow(trainData))
                #print(proc.time() - ptm)

                testData = featureDf %>% filter ((DateTime >= startDate) & (DateTime <= endDate)) %>% select (-DateTime)
                prediction = predict(model, testData)$predictions #Remove $predictions with randomForest
                predictions[[h]][(completeDf$DateTime >= startDate) & (completeDf$DateTime <= endDate), zone] = prediction
            }
        }
    }
    #Write out result, possibly plot result
    for (h in horizons){
            csvFile = paste0(outputDir, "randomforest_horizon_", as.character(h), ".csv")
            write.csv(predictions[[h]], csvFile, row.names=FALSE)
            if (PlotResult){
                pdf(paste0(outputDir, "Visualizations/", "randomforest_horizon_", as.character(h), ".pdf"),width=7,height=5)
                zoneDf = predictions[[h]]
                for (zone in zones){
                    strip(x = zoneDf[[zone]], 
                    date = zoneDf$DateTime,
                    cond = year(zoneDf$DateTime),
                    arrange = "wide",
                    main = paste("RandomForest Prediction", zone, "horizon", h))
                    dev.off()
                }
            }
    }
}