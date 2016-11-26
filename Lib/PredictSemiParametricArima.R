#Parallelize over zones
predictSemiParametricArimaParallel <- function(outputDir, 
                                trainingDf, 
                                completeDf, 
                                zones,
                                temperatures,
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
                           predictSemiParametricArima(outputDir, trainingDf, completeDf, 
                                zones, temperatures, horizons,  plotResult, saveResult = FALSE)
    stopImplicitCluster()
    
    if (saveResult){
        savePredictions("SemiParametricArima", predictions, horizons, outputDir)
    }
}


predictSemiParametricArima <- function(outputDir, 
                                trainingDf, 
                                completeDf, 
                                zones,
                                temperatures,
                                horizons,  
                                plotResult = FALSE,
                                saveResult = FALSE){
    stopifnot(require('KernSmooth'))
    stopifnot(require('bbemkr'))
    stopifnot(require('mgcv'))
    stopifnot(require('timeDate'))
    stopifnot(require("MASS"))
    stopifnot(require('forecast'))
    stopifnot(require('dplyr'))
    stopifnot(require('lubridate'))
    
    source("Lib/SavePredictions.R")
    #Setup loging file
    source("Lib/SetupLog.R")

    #Identify where are the start and end of the prediction periods by shifting index of NA
    idxNaCases = !complete.cases(trainingDf)
    startPoints =  which(idxNaCases & !c(FALSE, head(idxNaCases, -1)) & c(tail(idxNaCases, -1), TRUE))
    endPoints = which(idxNaCases & c(TRUE, head(idxNaCases, -1)) & !c(tail(idxNaCases, -1), FALSE))
    startDates = trainingDf$DateTime[startPoints]
    endDates = trainingDf$DateTime[endPoints]
    nTestingPeriods = length(startDates)

    #Create Time Features
    startYear = year(completeDf$DateTime[1])
    endYear = year(tail(completeDf$DateTime, 1))
    years = seq(startYear, endYear)
    NorwayHolidays = c(EasterMonday(years), 
                                  Ascension(years), 
                                  PentecostMonday(years), 
                                  LaborDay(years), 
                                  GoodFriday(years), 
                                  BoxingDay(years), 
                                  GoodFriday(years)-86400);
    completeDf = completeDf %>% mutate(Holiday= isHoliday(as.timeDate(DateTime), NorwayHolidays, wday=0:6)) %>%
                                mutate(ChristmasDay= isHoliday(as.timeDate(DateTime), ChristmasDay(years), wday=0:6)) %>%
                                mutate(ChristmasEve= isHoliday(as.timeDate(DateTime), ChristmasEve(years), wday=0:6)) %>%
                                mutate(NewYearsDay= isHoliday(as.timeDate(DateTime), NewYearsDay(years), wday=0:6)) %>%
                                mutate(DoW = factor(wday(DateTime))) %>%
                                mutate(ToY = as.numeric(strftime(DateTime, format = "%j"))
                                            +as.numeric(strftime(DateTime, format="%H"))/24)


    #Assume the zone and temperature here
    maxHorizon = max(horizons)
    predictions = rep(list(trainingDf), maxHorizon);
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
        featureDf = completeDf %>% dplyr::select(one_of(c("DateTime", 
                                            "Holiday", 
                                            "ChristmasDay", 
                                            "ChristmasEve", 
                                            "NewYearsDay", 
                                            "DoW", "ToY", "T", zone )))
        featureDf$E = featureDf[[zone]]
        featureDf$SmoT = featureDf$T
        for (i in 2:length(featureDf$T)){
            featureDf$SmoT[i] = 0.15*featureDf$T[i] + 0.85*featureDf$SmoT[i-1]
        }

        featureDf$LT = rep(0, nrow(featureDf))
        featureDf$Residuals = rep(0, nrow(featureDf)) #This is used to train an Arima on residuals
        for (period in seq(1, nTestingPeriods)){
            startTime = Sys.time()
            
            #Prepare for training model
            startDate = startDates[period]
            endDate = endDates[period]
            trainData = featureDf %>% filter(DateTime < startDate)
            trainData$LT = longTermTrend(trainData$E, trainData$T, trainData$DateTime)

            spec = E ~ LT + s(T) + s(SmoT) + s(ToY,bs="cc", k = 100) + 
                        DoW + ChristmasDay + ChristmasEve + NewYearsDay + Holiday
            for (hour in 0:23){ #different model for each hour
                #Becareful with logical vector index that not has the same length with dataframe, use which()
                trainingIdx = which(hour(trainData$DateTime)==hour) 
                trainDataAtH = trainData[trainingIdx, ]

                model = gam(spec, data=trainDataAtH)
                #gam.check(model)

                #Testing
                testingIdx = which((hour(featureDf$DateTime)==hour) & (featureDf$DateTime >= startDate) & (featureDf$DateTime <= endDate))
                featureDf$LT[testingIdx] = rep(tail(trainData$LT, 1), length(testingIdx))
                testData = featureDf[testingIdx, ]
                prediction = predict(model, testData)
                for (h in horizons){ #prediction is the same for all
                    predictions[[h]][testingIdx, zone] = prediction
                }
                featureDf$Residuals[trainingIdx] = model$model$E - model$fitted.values #residuals used for training arima
                featureDf$Residuals[testingIdx] = featureDf$E[testingIdx] - prediction #residuals used for testing arima
            }
            #Run Arima here
            startPoint = startPoints[period]
            endPoint = endPoints[period]
            startTrainingPoint = startPoint - 12*7*24 #Only get 3 months of data for training
            xts = xts(featureDf$Residuals, featureDf$DateTime)
            trainXts = xts[startTrainingPoint:(startPoint-1)]
            model = Arima(trainXts, order = c(3, 0, 3), seasonal=list(order=c(3, 0, 3), period = 24))
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
            
            prettyPrint(paste0("semiParametric|", zone, "|period ", period, "|Done in ", (Sys.time()-startTime)[[1]]));
        }  
    }
    
    for (h in horizons){
        if (saveResult) {
            savePredictions("SemiParametricArima", predictions, horizons, outputDir)
        }
        if (plotResult){
            pdf(paste0(outputDir, "Visualizations/", "SemiParametricArima_horizon_", as.character(h), ".pdf"),width=7,height=5)
            zoneDf = predictions[[h]]
            for (zone in zones){
                strip(x = zoneDf[[zone]], 
                      date = zoneDf$DateTime,
                      cond = year(zoneDf$DateTime),
                      arrange = "wide",
                      main = paste("SemiParametricArima Prediction", zone, "horizon", h))
            }
            dev.off()
        }
    }
    return (predictions)
}

longTermTrend <- function(E, T, DateTime, BANDWIDTH=12){
    E.xts = xts(E, DateTime)
    T.xts = xts(T, DateTime)
    E.month.xts = apply.monthly(E.xts, FUN="mean", na.rm = TRUE)
    I = as.numeric(format(index(E.month.xts), "%m"))
    #I = quarters (index(E.month.xts), "%m")
    #I = factor(I)
    T = apply.monthly(T.xts[index(E.xts)], FUN="mean", na.rm = TRUE)
    E.model = gam(E.month.xts ~ I + s(T))#ERROR!!
    E.est = E.model$fitted.values
    E.residuals = E.model$residuals
    a = NadarayaWatsonkernel(1:length(E.residuals), E.residuals, BANDWIDTH, 1:length(E.residuals))
    a$mh
    month = as.numeric(format(index(E.xts), "%m"))
    year = as.numeric(format(index(E.xts), "%y"))

    idx = (year-year[1])*12+(month-month[1])+1

    days = rep(0, length(E.xts))
    for (i in 1:length(a$mh)){
        days[which(idx == i)]  = sum(idx==i)
    }

    fraction = as.numeric(format(index(E.xts), "%d"))*24 + as.numeric(format(index(E.xts), "%H"))

    trend.month = c(a$mh[1]-(a$mh[2]-a$mh[1]), a$mh)
    trend = rep(0, length(E.xts))
    trend = (trend.month[idx+1]-trend.month[idx])*fraction/days + trend.month[idx]
    trend.xts = xts(trend, order.by = index(E.xts))
    return(drop(coredata(trend.xts)))
}