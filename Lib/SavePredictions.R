savePredictions <- function (methodName, predictions, horizons, outputDir){
    for (h in horizons){
        csvFile = paste0(outputDir, methodName, "_horizon_", as.character(h), ".csv")
        write.csv(predictions[[h]], csvFile, row.names=FALSE)
    }
}