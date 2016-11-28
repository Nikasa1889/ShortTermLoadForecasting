# Automatic approaches for Short-Term Load Forecasting
This is an experiment to test accuracy and speed of various forecasting method for Short-Term Load Forecasting (STLF). The experiment use 40 time-series, 20 from GEF2012Com and 20 from Hvaler substations. Code is written in R, tested in jupyter, and run on [r-notebooks docker](https://hub.docker.com/r/jupyter/r-notebook/). R packages can be downloaded easily using install.packages() command.

To easily distingush different zones, we name zones in the two datasets differently:
* Zones in GEF2012Com dataset are called zone.1 to zone.20
* Zones in Hvaler dataset are called subs.1 to subs.20

## Project Structure:
* **GEFCom2012**: contains data, predictions, and prediction visualizations for GEFCom2012 dataset
  * **load_raw.csv**: original dataset provided by GEF2012Com
  * **temp.csv**: original temperature timeseries provided by GEF2012Com
  * **n034_ensemble.csv**: prediction of the winner of GEF2012Com, used to impute the missing values in GEF2012Com
  * **complete.csv**: complete dataset with all 20 zones and 11 temperature timeseries (after running *TidyGEFCom2012.ipynb*)
  * **training_set.csv**: complete dataset with testing periods masked by NA values (after running *MarkTestingPeriods.ipynb*)
  * **performance_report.csv**: report MAPE for all predictions made for GEFCom2012
  * **Predictions/**: contains all predictions stored in csv files (and potentially visualization for predictions)
* **Hvaler**: contains data, predictions, and prediction visualizations for GEFCom2012 dataset
  * **top_20.csv**: original dataset collected from MySQL with many missing values
  * **temperature_2010_2014.csv**: raw temperature for Hvaler region (only one timeseries)
  * **complete.csv**: complete dataset with all 20 zones and 1 temperature timeseries (after running TidyHvaler.ipynb)
  * **imputed_complete.csv**: using Amelia to impute missing values from complete.csv (after running HvalerImputation.ipynb)
  * **training_set.csv**: complete dataset with testing periods masked by NA values (after running MarkTestingPeriods.ipynb)
  * **Predictions/**: contains all predictions stored in csv files (and potentially visualization for predictions)
* **Lib**: contains all the R functions that used in the experiment
* **Visualizations**: contains all visualizations of raw data and imputed data
* **Reports**: contains all report and plots used in the paper, moved here manually
* **TidyGEFCom2012.ipynb** and **TidyHvaler.ipynb**: tidy dataset
* **HvalerImputation.ipynb**: impute missing values in Hvaler dataset
* **MarkTestingPeriod.ipynb**: mark the testing period by NA values
* **VisualizeData.ipynb**: visualize data
* **MakePrediction.ipynb**: call various predict functions in Lib/ to make prediction
* **RunExperiment.R**: script that receive arguments and run prediction based on selected dataset, zones, and methods. Everything report to RunExperiment.out and RunExperiment.log
* **RunExperiment.sh**: sh script to demo how you can call RunExperiment.R
* **ReportPerformance.ipynb**: scan predictions folder in GEFCom2012 and Hvaler to automatically report performance and plots
* **ReportRunningTime.ipynb**: scan the RunExperiment.log files to produce report and plot on running time of different methods
* **subs.1.traingdata.pdf** and **zone.1.trainingdata.pdf**: plot shows data example with testing period marked
