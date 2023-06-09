# library(bslib)

# custom_theme <- bs_theme(
#   version = 5,
#   bg = "#FFFFFF",
#   fg = "#1F1E2E",
#   primary = "#0199F8",
#   secondary = "#5C7E92",
#   base_font = "Maven Pro"
# )

ui <- fluidPage(  tags$style(".nav { display:none; }"), 
theme = shinytheme("sandstone"),navbarPage(id = "inTabset",
  title = tags$img(src = "logo.png",height = 30, width = 130,align = "center"),   
  position = c("fixed-top"),
  tabsetPanel(type = "tabs",
    tabPanel(tags$h5('Home'),value = "home",  uiOutput("home",align="center"), ),
    tabPanel(tags$h5('Resources'),value = "resources",  uiOutput("resources",align="center"), ),
    tabPanel(tags$h5('Parameters'), value = "parameters",  uiOutput("parameters",align="center")),
    tabPanel(tags$h5('Training'), value = "panel4", uiOutput("Tab_3",align="center")),
    tabPanel(tags$h5('Users'),value = "users",  uiOutput("users",align="center"), ),
    tabPanel('', value = "data", uiOutput("data",align="center")),
    tabPanel('', value = "local", uiOutput("local",align="center")),
    tabPanel('', value = "cloud", uiOutput("cloud",align="center")),
    tabPanel('', value = "success", uiOutput("success",align="center"))
    ,id = "tabselected"),
  
  ))







