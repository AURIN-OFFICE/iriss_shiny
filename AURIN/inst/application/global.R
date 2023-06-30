rm(list=ls())
################## --------------- R libraries ------------- ##################
if(!require("shiny")){install.packages("shiny");library(shiny)}else{library(shiny)}
if(!require("shinydashboard")){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)}
if(!require("shinydashboardPlus")){install.packages("shinydashboardPlus");library(shinydashboardPlus)}else{library(shinydashboardPlus)}
if(!require("shinyalert")){install.packages("shinyalert");library(shinyalert)}else{library(shinyalert)}
if(!require("shinythemes")){install.packages("shinythemes");library(shinythemes)}else{library(shinythemes)}
if(!require("shinycssloaders")){install.packages("shinycssloaders");library(stringi)}else{library(stringi)}
if(!require("shinyWidgets")){install.packages("shinyWidgets");library(shinyWidgets)}else{library(shinyWidgets)}
if(!require("shinyjs")){install.packages("shinyjs");library(shinyjs)}else{library(shinyjs)}
if(!require("shinyBS")){install.packages("shinyBS");library(shinyBS)}else{library(shinyBS)}
if(!require("shinyAce")){install.packages("shinyAce");library(shinyAce)}else{library(shinyAce)}
if(!require("shinyFiles")){install.packages("shinyFiles");library(shinyFiles)}else{library(shinyFiles)}
if(!require("markdown")){install.packages("markdown");library(markdown)}else{library(markdown)}
if(!require("rjson")){install.packages("rjson");library(rjson)}else{library(rjson)}
if(!require("rmarkdown")){install.packages("rmarkdown");library(rmarkdown)}else{library(rmarkdown)}

#### ------- Load data Using in the dropdowns menu  ------ ######
LSAY = readRDS('LSAY_metadata_shiny.RDS')
TSP = read.csv('TSP_metadata_2021.csv')


###### ------- Create a Parameters file based on the input of the users ------ ####### 
CreateParameters = function(token_ada,path_files, longitudinal,long_waves,long_topic, TSP, TSP_years, TSP_variables){
  
  ##### ----- Parsing the user input ---- #####
  if(!is.null(token_ada)) {
    if (token_ada!=''){
      withAPI = TRUE
      path_files = ""
    } else {
      withAPI = FALSE
    }
  } else if (!is.null(path_files)){
    if (path_files!=''){
      withAPI = FALSE
      path_files = gsub(x=path_files,pattern="\\\\",replacement = "/")
      token_ada = ""
    } else {
      withAPI = TRUE
    }
  }
  #### ------ Define an empty list to storage all the parameters ------ ###### 
  parameters = list()
  #### ----- bolean: true/fasle ---- ####
  parameters[['with_API']] = withAPI
  
  #### ------- based on the user decision ------ #####
  ###### ---- with api ---- ####
  dataverse = list()
  if (token_ada==""){
    dataverse[['token']] = token_ada
    dataverse[['ADA_ID']] =  ""
    dataverse[['name']] = ""
    parameters[['dataverse']] = dataverse
  } else {
    dataverse[['token']] = token_ada
    dataverse[['ADA_ID']] =  "doi:10.4225/87/6BW27V"
    dataverse[['name']] = 'LSAY_2009'
    parameters[['dataverse']] = dataverse
  }
  
  ##### ------- alternative information ----- ######
  parameters[['Survey_files']] = path_files 
  parameters[['LSAY_cohort']] = 2009
  parameters[['TSP_year']] = as.numeric(TSP_years)
  parameters[['LSAY_waves']] = as.numeric(gsub(long_waves,pattern = "Wave.+-",replacement=""))
  parameters[['LSAY_topics']] = long_topic
  parameters[['TSP_variables']] = TSP_variables
  
  #### ------ Write to Json ------ ######
  json_file = toJSON(parameters)
  
  return(json_file)
  
}


############## -------------- CSS functions using in the shiny ----------- ############
####### ------- Create a color boxes ---- ######
CreateBox <- function(color,id,height,left,right,content) {
  content = fluidRow(div(style= paste0("
      border-top: none;
      border-radius: 10px;
      border-left: 5px solid ",color,";
      border-right: 5px solid ",color,";
      margin-left: ",left,"%;
      margin-right: ",right,"%;
      padding: 0px 0px;
      box-shadow: 0 1px 1px rgb(0 0 0 / 10%);overflow:hidden; 
      background: ",color,";
      width: 95%; "),div(style=paste0("overflow-y: scroll;  overflow-x: hidden;width: 110%;  height: ",height,";padding-right: 10%"),content)))
}


####### ------- Center content inside the box ---- ######
centerContent = function(content){
  return(div(style="font-size: 20px; padding-top: 5%; color: white; font-family: Arial;",content))
  
}
####### ------- Left content inside the box ---- ######
leftContent = function(content){
  return(div(style="font-size: 20px; padding-left: 5%; color: white; font-family: Arial; position: absolute;",content))
  
}
####### ------- Pading: Top and right and left ---- ######
BulletsContent = function(content){
  return(div(style="font-size: 18px; padding-left: 5%; padding-right: 5%; padding-bottom: 5%; color: white; font-family: Arial; position: relative;",content))
  
}

####### ------- Pading: bottom and right and left ---- ######
ButtonContent = function(content){
  return(div(style="font-size: 18px; padding-left: 5%; padding-right: 5%; padding-bottom: 5%; color: white; font-family: Arial; position: relative;",content))
  
}

####### ------- CSS Download downloadButton: color and size ---- ######
buttonDownload = function(outputId,label,color,width=NULL){
  return(tagList(downloadButton(outputId=outputId, class = paste0(outputId,'_style'), label = label),
                 tags$head(tags$style(paste0(".",paste0(outputId,'_style'),
                  "{background-color:",color,";color: #ffffff; width:",width,"}")))))
}

####### ------- CSS Download buttonAction: color and size ---- ######
buttonAction = function(outputId,label,color,width=NULL){
  return(tagList(actionButton(inputId = outputId,label = label,width=width,
                style=paste0("background-color:",color,";color: #ffffff;"))))
}

####### ------- Pading: Top and right and left ---- ######
centerContentLinks = function(content){
  return(div(style="font-size: 20px; padding-top: 0%; padding-bottom: 10%;  color: white; font-family: Arial;",
             fluidPage(tags$head(tags$style(HTML("a {color: #005BAA;}"))),content)))
}

