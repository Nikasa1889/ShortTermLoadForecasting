#!/bin/bash
# R CMD BATCH --no-save --no-restore '--args datasets=c("GEFCOM2012") methodNames=c("DSHW","ModifiedDSHW","AverageARIMA","SemiParametric","TBATS") nzones=8' RunExperiment.R RunExperiment.out
R CMD BATCH --no-save --no-restore '--args datasets=c("GEFCOM2012") methodNames=c("TBATS") nzones=8' RunExperiment.R RunExperiment.out