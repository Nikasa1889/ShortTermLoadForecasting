#!/bin/bash
#R CMD BATCH --no-save --no-restore '--args datasets=c("GEFCOM2012") methodNames=c("RandomForest", "DSHW","ModifiedDSHW","AverageARIMA","SemiParametric","TBATS") nzones=20' RunExperiment.R RunExperiment.out &
R CMD BATCH --no-save --no-restore '--args datasets=c("Hvaler") methodNames=c("RandomForest") nzones=20' RunExperiment.R RunExperiment.out &