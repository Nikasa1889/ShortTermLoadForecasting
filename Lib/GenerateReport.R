##This function scan the whole predictionDir for prediction out file with format "methodname_horizon_n.csv"
##Output is saved into predictionDir
generateReport <- function (completeDf, 
                            trainingDf, 
                            mainDir, 
                            predictionDir, 
                            zones, 
                            fileClasses){
    reportDf = data.frame('files'=list.files(path = predictionDir, pattern = "*.csv"))
    reportDf = reportDf %>% mutate(method=substr(files, 1, regexpr("(_horizon_|.csv)", files)-1)) %>%
                        mutate(horizon_start = regexpr("_horizon_", files) ) %>%
                        mutate(horizon_end = regexpr(".csv", files)-1) %>%
                        mutate(horizon = as.numeric(
                            ifelse(horizon_start > -1, substr(files, horizon_start+9, horizon_end), -1))) %>%
                        select(-horizon_start, -horizon_end) %>%
                        arrange(method, horizon)

    testingIdx = !complete.cases(trainingDf)
    for (zone in zones) {
        reportDf[[zone]] = rep(0, nrow(reportDf))
    }
    for (i in seq(1, nrow(reportDf))){
        file = reportDf$files[i]
        print(paste("Processing", paste0(predictionDir, file), "..."))
        predictionDf = read.csv(paste0(predictionDir, file), stringsAsFactors=FALSE, colClasses=fileClasses)
        for (zone in zones){
            if (sum(is.na(predictionDf[[zone]]))<0.2*sum(testingIdx)){
            MAPE = mean(abs(predictionDf[[zone]][testingIdx] - completeDf[[zone]][testingIdx])/completeDf[[zone]][testingIdx],
                        na.rm = TRUE)
            } else {
                MAPE = NA
            }
            reportDf[[zone]][i] = MAPE
        }
    }
    write.csv(reportDf, paste0(mainDir, "performance_report.csv"), row.names=FALSE)
    return(reportDf);
}