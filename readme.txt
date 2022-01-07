The .zip file consists of 2 folders - Preprocessing_and_Exploration and Model_and_Prediction

The preprocessing folder contains code from the project consists of the following files:

dataprocessingcategoricalv3.R 	- Contains preprocessing steps that were executed on the original dataset. Produces IMDBexportcomplete.csv once complete, serving as the base data for our analysis.
IMDBexportcomplete.csv 			- The refined dataset afterdataprocessingcategoricalv3.Rhas been run onthe original dataset.
Modeltuningconsolidated.R 		- Contains code that follows the model-building process throughout the project.


The model folder contains code to be used for the purposes of prediction. 

countries and continents.csv 	- A lookup table mapping each country to its respective continent
IMDB_tier_lookup.csv 			- A lookup table containing the names of every actor/director/producer that appears in the original dataset along with their average score, frequency (i.e. how many times they appear), respective roles, and tier.
model.rds 						- Code containing the prediction module. 
Prediction_module.R 			- The R file used to run the model for prediction of new films.



Instructions for prediction:
1) Extract all files from the Model_and_Prediction folder into the same path.

2) In line 6 of prediction_module.R, replace the filename with the relevant file containing test data. 
NOTE: If an .xlsx file is provided, follow the additional instructions below, otherwise skip to step 3:
2a) run the following command (without double quotes): "install.packages('readxl')"
2b) run the following command (without double quotes): "library(readxl)"	
2c) replace line 6 with the following (without double quotes): IMDBdata <- read_excel("<NAME_OF_FILE>.xlsx")

3) source the file: prediction_module.r