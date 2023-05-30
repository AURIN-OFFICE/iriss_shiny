################### -------------- Run: in a server --------------  ###################
require(shiny)
library(shiny)
### --- Path inst/application --- ###
folder_address = 'inst/application'
runApp(appDir =folder_address, port = 90,host = "0.0.0.0",
       launch.browser = getOption("shiny.launch.browser", interactive()), workerId = "",
       quiet = FALSE, display.mode = c("auto", "normal", "showcase"),
       test.mode = getOption("shiny.testmode", FALSE))
################### -------------------------------------------  ###################
