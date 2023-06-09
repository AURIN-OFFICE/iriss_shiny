rm(list=ls())
################## --------------- Paquetes Shiny ------------- ##################
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


LSAY = readRDS('LSAY_metadata_shiny.RDS')
TSP = read.csv('TSP_metadata_2021.csv')


boxd <- function(color,id,height,left,right,content) {
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

# 
boxg <- function(color,id,height,left,right,content) {
  return(tagList( tags$style(
    HTML(
      paste0("#",id," {
      border-top: none;
      border-radius: 10px;
      border-left: 5px solid ",color,";
      border-right: 5px solid ",color,";
      margin-left: ",left,"%;
      margin-right: ",right,"%;
      padding: 0px 0px;
      box-shadow: 0 1px 1px rgb(0 0 0 / 10%);
      background: ",color,";
      height:",height,";

    }")
    )
  ),
  shinydashboardPlus::box(
    id = id, width = 12,
    content
  )) )
}




centerContent = function(content){
  return(div(style="font-size: 20px; padding-top: 5%; color: white; font-family: Arial;",content))
  
}

leftContent = function(content){
  return(div(style="font-size: 20px; padding-left: 5%; color: white; font-family: Arial; position: absolute;",content))
  
}

BulletsContent = function(content){
  return(div(style="font-size: 18px; padding-left: 5%; padding-right: 5%; padding-bottom: 5%; color: white; font-family: Arial; position: relative;",content))
  
}

ButtonContent = function(content){
  return(div(style="font-size: 18px; padding-left: 5%; padding-right: 5%; padding-bottom: 5%; color: white; font-family: Arial; position: relative;",content))
  
}

buttonDownload = function(outputId,label,color,width=NULL){
  return(tagList(downloadButton(outputId=outputId, class = paste0(outputId,'_style'), label = label),
                 tags$head(tags$style(paste0(".",paste0(outputId,'_style'),"{background-color:",color,";color: #ffffff; width:",width,"}")))))
}

buttonAction = function(outputId,label,color,width=NULL){
  return(tagList(actionButton(inputId = outputId,label = label,width=width,style=paste0("background-color:",color,";color: #ffffff;"))))
}


centerContentLinks = function(content){
  return(div(style="font-size: 20px; padding-top: 0%; padding-bottom: 10%;  color: white; font-family: Arial;",fluidPage(tags$head(tags$style(HTML("a {color: #005BAA;}"))),content)))
  
}

