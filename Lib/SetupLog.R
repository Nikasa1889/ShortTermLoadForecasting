#Setup loging file
sink("RunExperiment.log", append = TRUE, type = c("output", "message"))
prettyPrint <- function (message){
    workername = paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
    print(paste0(Sys.time(), " " ,workername, ": ", message))
}
prettyPrint("Starting")