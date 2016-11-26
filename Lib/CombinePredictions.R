combinePredictions <- function (horizons, zones, predictions1, predictions2){
    result = predictions1
    for (h in horizons){
        for (zone in zones){
            result[[h]][[zone]] = ifelse(!is.na(predictions1[[h]][[zone]]), predictions1[[h]][[zone]], predictions2[[h]][[zone]])
        }
    }
    return(result)
}