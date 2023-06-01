########### ----------- Clean all variables -------- #####
rm(list=ls())

#### ------- Install dependencies ------ ####
path = getwd()
#### ---- Inside the folder: ---- ###
setwd(paste0(path,'/IRISS'))
source('tools/install.R')

#### ----- Compile documentation ---- ##
devtools::document()
devtools::build_manual()
#### ----- Install the IRISS library ----- ####
setwd(path)
#### ----- After generate changes in the library we need to delete it and reinstall ---- ####
remove.packages('IRISS')
install.packages('IRISS',repos = NULL, type = 'source')

library(IRISS)
########## ------- Load parameters --------- ###### 
LoadParameters(file = "Parameters.json")
##### ------ Create folders ------ #####
CreateFolders()
# #### ------ Create log file ------ #####
# logNm <- 'test.log'
# GenerateLog(name = logNm)

######### ---------- A vector with the path of each file ------- @@@@@@@@@@
LSAY_cohort = ParsingParameter('LSAY_cohort')
LSAY_waves = as.numeric(ParsingParameter('LSAY_waves'))
SurveyFiles = ParsingParameter('SurveyFiles')
TSP_year = ParsingParameter('TSP_year')
TSP_variables = ParsingParameter('TSP_variables')

####### -------------- 1. Loading data ----------- #########
###### -------- Load LSAY ------- ####
LSAY_2009 = LoadLSAY(wave = LSAY_waves,cohort = LSAY_cohort)
##### ---- Extract survey responses --- #####
SurveyResponses = LSAY_2009[['SurveyResponses']]
##### ---- Exctract GeospatialResponses --- #####
GeospatialResponses = LSAY_2009[['GeospatialResponses']]

### -------- Load TSP 2021  -------- #######
TSP_2021 = LoadTSP2021(year=LSAY_waves,variables = TSP_variables)
### -------- Extract data ------ #####
TSP_2021_data = TSP_2021[['data']]
### -------- Extract metada ------ #####
TSP_2021_metadata = TSP_2021[['metadata']]

####### -------------- 2. Geospatial data linkage ----------- #########
####### ----------- 2.1 POAS TO SA3 ----------- ######
SurveyResponses_SA3 = LSAY_POA_SA3(data=GeospatialResponses,concordances=concordances)
###### --------- SA3 ------- ########
SurveyResponses_SA3_IN = SurveyResponses_SA3[['sa3']]
###### --------- ABS Metric ------- ########
SurveyResponses_SA3_metric = SurveyResponses_SA3[['metric']]
###### --------- ABS Concordances ------- ########
SurveyResponses_SA3_concordances= as.data.frame(SurveyResponses_SA3[['concordances']])
###### --------- ABS Ratio ------- ########
SurveyResponses_SA3_ratio = as.data.frame(SurveyResponses_SA3[['ratio']])

####### ----------- STAGE 2.2: SA3 TO SA3 year ----------- ######
SurveyResponses_SA3_TSP = LSAY_PSA3_SA3(data=SurveyResponses_SA3_IN,concordances=concordances,year_out = TSP_year)
###### --------- SA3 ------- ########
SurveyResponses_SA3_TSP_OUT = SurveyResponses_SA3_TSP[['sa3']]
###### --------- ABS Metric ------- ########
SurveyResponses_SA3_TSP_metric = SurveyResponses_SA3_TSP[['metric']]
###### --------- ABS Concordances ------- ########
SurveyResponses_SA3_TSP_concordances = as.data.frame(SurveyResponses_SA3_TSP[['concordances']])
###### --------- ABS Ratio ------- ########
SurveyResponses_SA3_TSP_ratio = as.data.frame(SurveyResponses_SA3_TSP['ratio'])

####### ------- GeoSpatial join ------ #####
DataJoined = GeoSpatialJoin(year=LSAY_waves,GeospatialResponses=SurveyResponses_SA3_TSP_OUT,SurveyResponses=SurveyResponses,TSP_data = TSP_2021_data,TSP_metadata = TSP_2021_metadata)

############ --------- Geospatial report --------- ############
summaryResults = SummaryReport(concordances_POA= SurveyResponses_SA3_concordances,
                               concordances_SA3 = SurveyResponses_SA3_TSP_concordances,
                               metrics_POA = SurveyResponses_SA3_metric,
                               metrics_SA3 = SurveyResponses_SA3_TSP_metric)

######## -------- Write the summary report --------- #######
openxlsx::write.xlsx(x=summaryResults,file='outputs/Summary.xlsx')

####### ------- Write stata ------ #####
WriteStata(DataJoined = DataJoined,SurveyResponses = SurveyResponses,output_path = '/Users/gonzalezge/Downloads/', year=LSAY_waves)






