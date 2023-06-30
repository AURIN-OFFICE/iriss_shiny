####### --------- Menu Tabs ------- ########
####### ------ Hide ------- #########
ui <- fluidPage(  tags$style(".nav { display:none; }"), 
  theme = shinytheme("sandstone"),navbarPage(id = "inTabset",
  title = tagList(tags$img(src = "logo_final.png",height = 30, width = 180,align = "left")), ## Logo
  position = c("fixed-top"), 
  tabsetPanel(type = "tabs", ## Main tabs
    tabPanel(tags$h5('Home'),value = "home", uiOutput("home",align="center"), ),
    tabPanel(tags$h5('Resources'),value = "resources",  uiOutput("resources",align="center"), ),
    tabPanel(tags$h5('Parameters'), value = "parameters",  uiOutput("parameters",align="center")),
    tabPanel(tags$h5('Users'),value = "users",  uiOutput("users",align="center"), ),
    tabPanel('', value = "data", uiOutput("data",align="center")),
    tabPanel('', value = "local", uiOutput("local",align="center")),
    tabPanel('', value = "cloud", uiOutput("cloud",align="center")),
    tabPanel('', value = "success", uiOutput("success",align="center"))
    ,id = "tabselected"), ))







