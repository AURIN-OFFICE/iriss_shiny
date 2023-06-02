################################## ---------------- Servidor: ----------------  ##################################

server = shinyServer(function(input, output, session) {
  
  ####### -------- Possibles options ------- ###### 
  longitudinal_choices = c("LSAY - Cohort 2009")
  waves_choices = paste0('Wave ',c(2:9),' - ', c(2010:2019))
  long_topic_choices = unique(LSAY$`Sub-major topic area`)
  TSP_choices = c("Time Series Profile 2021") 
  TSP_years_choices = c(2011,2016,2021)  
  TSP_variables_choices = unique(TSP$variable)
  
  #hideTab(inputId = "tabselected", target = "Home")
  
  
  #path <<-gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "Shiny/.+",replacement = "")
  #path 
  
  
  
  output$toolbox = renderUI(uiOutput("Tab_home",align="center"))
  
  # output$documentation <- downloadHandler(
  #   filename =  "IRISS_1.0.pdf",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     file.copy("IRISS_1.0.pdf", file, overwrite = TRUE)
  #    
  #   }
  # )
  
  
  
  output$toolbox <- downloadHandler(
    filename =  "Toolbox.tar.gz",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy("IRISS_1.0.tar.gz", file, overwrite = TRUE)
      
    }
  )
  
  output$manual <- downloadHandler(
    filename =  "Toolbox.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy("IRISS_1.0.pdf", file, overwrite = TRUE)
      
    }
  )
  
  
  output$script <- downloadHandler(
    filename =  "main.R",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy("main.R", file, overwrite = TRUE)
      
    }
  )
  
  
  output$config <- downloadHandler(
    filename =  "config.ylm",
    
    content = function(file) {

      write(x = input$someID, file = file)
      
    } 
    
    
  )
  
  
  # output$parameters <- downloadHandler(
  #   filename =  "parameters.ylm",
  #   
  #   content = function(file) {
  #     
  #     write(x = '{}', file = file)
  #     
  #   } 
  #   
  #   
  # )

  #### -------- Backup Propierties ---------#####
  shinyFileChoose(input, id = 'dir', roots=c(wd='~/'),  session=session)
  directo <<- reactive(input$dir)
  
  path_utilizar <<- reactive( parseFilePaths(roots=c(wd='~/'), input$dir))
  
  
  ################################# ---------------- tabs ----------------  ##################################
  observeEvent(input$start_home, {
    updateTabsetPanel(session, "tabselected",
                      selected = "resources")
  })
  
  observeEvent(input$start, {
    updatePickerInput(session,"longitudinal",selected = "")
    updatePickerInput(session,"long_waves",selected = "")
    updatePickerInput(session,"long_topic",selected = "")
    updatePickerInput(session,"TSP",selected = "")
    updatePickerInput(session,"TSP_years",selected = "")
    updatePickerInput(session,"TSP_variables",selected = "")
    
    updateTabsetPanel(session, "tabselected",
                      selected = "parameters")
  })
  
  observeEvent(input$training, {
    updateTabsetPanel(session, "tabselected",
                      selected = "panel4")
  })
  
  
  output$Tab_home  = renderUI( tagList(HTML('<br>'),HTML("<p style='text-align:left;'><b>  <font size='6'>Welcome to the GeoSocial Service</font> </b></p>"),
                                       boxg(color = '#96B243',id='subbox15',height= "380px",left='-1',right='-3',content=tagList(fluidRow(column(6,centerContent(tagList(fluidRow(HTML('<br>'),tags$img(src = "logn.png",height = 150, width = 350,align = "center")),
                                                                                                                                                                        fluidRow(HTML('Longitudinal survey'))))),
                                                                                                                                         column(6,div(style="font-size: 20px; color: white; font-family: Arial;",(tagList(fluidRow(tags$img(src = "intro.png",height = 250, width = 500,align = "center")),
                                                                                                                                                          fluidRow(HTML('Geospatial data'))))))),HTML('<br>'),
                                                                                                                                 div(style="font-size: 16px; padding-left: 5%; padding-right: 5%;padding-bottom: 10%;color: white; font-family: Arial;",HTML("<p style='text-align:center;'>The Geosocial solution allows researchers to augment Australia’s largest longitudinal surveys with geospatial statistical data derived from the Australian Census of Population and Housing. Geosocial will empower Australia's large cross-disciplinary social research community to identify patterns, make predictions, and inform social policy using rich integrated geosocial data.</p>")))
                                            ),
                                     div(boxg(color = '#45A0EF',id='subbox1s',height= "320px",left='-1',right='-3',
                                              content=tagList(column(8,div(style="font-size: 16px; padding-top: 5%; color: white; font-family: Arial;",HTML("<p style='text-align:justify;'><b>How works?
                                                                                                     </b> <br><br> Given the data's high confidence and the data custodians' restrictions,
                                                                                                                                                             the Geosocial solution condensates multiple capabilities 
                                                                                                                                                             and functions to the data linkage in a toolbox, 
                                                                                                                                                             which with a configuration file and R script executed the data linkage.
                                                                                                                                                             
                                                                                                                                                             <br><br><b> Toolbox:</b> R library that has everything needed to the data linkage.
                                                                                                                                                             <br><br><b> Parameters: </b>File with all information for data linkage, including data location, API credentials, wave and cohort info, etc.
                                                                                                                                                             <br><br><b> Script: </b>Executed the workflow and used the toolbox to read and merge the data based on user preferences.</p>"))),
                                                              column(3,div(style="font-size: 20px; padding-top: 5%; color: white; font-family: Arial;",tagList(fluidRow(tags$img(src = "tool.png",height = 200, width = 300,align = "center")),
                                                                                                      fluidRow(HTML('GeoSocial'))))),
                                                              column(1,div(style="font-size: 16px; padding-top: 250%; ", ButtonContent(buttonAction(outputId='start_home', label='Start',color = "#F5822B")))))))))
  
  
  

  output$resources <- renderUI( tagList(HTML('<br><br><br><br>'),div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Resources</b> "))),HTML('<br><br><br>'),div(fluidRow( column(8,
                        fluidRow(column(6,boxg(color = '#F5822B',id='subbox1',height= "400px",left='-1',right='-3',content=centerContent(tagList(fluidRow(column(12,HTML("TOOLBOX"))),
                            fluidRow(column(12,tags$img(src = "response.png",height = 150, width = 180,align = "center"))),HTML('<br><br>'),
                            fluidRow(column(12,buttonDownload(outputId = "toolbox",label = "Download",color = "#FDA563"))))))),
                        column(6,boxg(color = '#005BAA',id='subbox2',height= "400px",left='-3',right='-1',content=centerContent(tagList(fluidRow(column(12,HTML("DOCUMENTATION"))),
                            fluidRow(column(12,tags$img(src = "documentation.png",height = 150, width = 180,align = "center"))),HTML('<br><br>'),
                            fluidRow(column(12,buttonDownload(outputId = "manual",label = "Documentation",color = "#45A0EF")))))))),
                        fluidRow(column(12,boxg(color = '#DBB639',id='subbox5',height= "400px",left='-1',right='-1',content=centerContent(tagList(fluidRow(column(12,HTML("SCRIPT"))),
                            fluidRow(column(12,leftContent(tags$img(src = "personas.png",height = 150, width = 180,align = "center")))),HTML('<br><br>'),
                            fluidRow(column(12,buttonDownload(outputId = "script",label = "Download",color = "#E5C34F"))))))))),
                        column(4,boxg(color = '#5C7E92',id='subbox3',height= "810px",left='-5',right='-1',content=centerContent(tagList(fluidRow(column(12,HTML("PARAMETERS"))),
                            fluidRow(column(12,tags$img(src = "search.png",height = 150, width = 180,align = "center"))),HTML('<br><br>'),
                            fluidRow(column(12,BulletsContent(HTML(markdown::renderMarkdown(text = "- Easy access to the data.\n- Certainty regarding data meanings. \n- Less room for analytic errors \n- Increased data usability and utility to untrained users.  \n"))))),
                            fluidRow(column(12,ButtonContent(buttonAction(outputId='start', label='Create',color = "#759AAF"))))))))))))

  
  observeEvent(input$cloud, {
    #### ----- Restart ---- ####
    
    updateAceEditor(session, "token_ada", value = '')
    updateAceEditor(session, "path_files", value = '')
    
    updateTabsetPanel(session, "tabselected",
                      selected = "cloud")
  })
  
  observeEvent(input$local, {
    #### ----- Restart ---- ####
    updateAceEditor(session, "path_files", value = '')
    updateAceEditor(session, "token_ada", value = '')
    
    updateTabsetPanel(session, "tabselected",
                      selected = "local")
  })
  
  
  observeEvent(input$Back_data, {
    updateTabsetPanel(session, "tabselected",
                      selected = "parameters")
  })
  
  
  
  
  output$data <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
                          tagList(div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Where would you like your integrated data stored?</b> "))),HTML('<br><br><br>'),
                           HTML('The toolbox, allows the user to load the data linkage coming from the following sources:'),
                           fluidRow(column(2,''),
                           column(4,fluidPage(fluidRow(tags$h3('Australian Data Archive (ADA) API')),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(tags$img(src = "cloud.png",height = 240, width = 240,align = "center")),
                                              fluidRow(HTML('The longitudinal survey data is provided by ADA, a national service for collecting and preserving digital research data.')),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(buttonAction(outputId='cloud', width='400px',label='Cloud',color = "#E5C34F")))),
                           column(4,fluidPage(fluidRow(tags$h3('Local environment')),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(tags$img(src = "local.png",height = 240, width = 240,align = "center")),
                                              fluidRow(HTML('The user provides the longitudinal survey in the local environment where the toolbox is executed.')),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(buttonAction(outputId='local', width='400px', label='Local',color = "#E5C34F")))),
                           column(2,'')),
                           fluidRow(column(2,ButtonContent(buttonAction(outputId='Back_data', label='Back',color = "#759AAF"))),column(6,''),column(2,''))))))))
            
  
  observeEvent(input$back_ada, {
    updateTabsetPanel(session, "tabselected",
                      selected = "data")
  })
  
  observeEvent(input$continue_ada, {
    
    if(input$token_ada==''){
      shinyalert(title='Please insert a token.',html = T)
    } else {
      updateTabsetPanel(session, "tabselected",
                        selected = "success")
    }

  })
  
  output$cloud <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
                          tagList(div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Where you would like you integrated data stored?</b> "))),HTML('<br><br><br>'),
                                  fluidRow(column(12,
                                                  renderUI(fluidPage(shinydashboard::box(title = HTML('<strong><font color="#000000">Australian Data Archive (ADA) API</font></strong>'),
                                                                                            tagList(HTML('Before generating an API token to use the ADA API, it is necessary to obtain approval to access the LSAY 2009 data through ADA.'),tags$a(href="https://dataverse.ada.edu.au/loginpage.xhtml?redirectPage=dataverse.xhtml", "Click here for information.",style='text-decoration: underline;'),HTML('After getting the approval, you can create a token. Please refer to the image below to locate it.')),width = 1000,height = 1000,status = "success",solidHeader = T,
                                                                                         HTML('<br><br>'),   
                                                                                         tagList(fluidRow(tags$img(src = "token.png",height = 200, width = 800,align = "center")),
                                                                                                 HTML('<br>'),  
                                                                                            fluidRow(column(3,""),column(6,tagList(HTML('<strong><font color="#000000">Please copy and paste the ADA token into the designated field below:</font></strong>'),
                                                                                                                                   aceEditor(
                                                                                                                                     outputId = "token_ada",height = "50px",
                                                                                                                                     value = '',
                                                                                                                                     placeholder = "Please introduce your ADA token",debounce=0
                                                                                                                                   ))),column(3,""),
                                                                                            tagList(fluidRow(column(6,ButtonContent(buttonAction(outputId='back_ada', label='Back',color = "#759AAF"))),
                                                                                                             column(6,ButtonContent(buttonAction(outputId='continue_ada', label='Continue',color = "#759AAF"))))),
                                                                                            )))))))))))))
  observeEvent(input$back_local, {
    updateTabsetPanel(session, "tabselected",
                      selected = "data")
  })
  
  observeEvent(input$continue_local, {
    if(input$path_files==''){
      shinyalert(title='Please insert the path.',html = T)
    } else {
      updateTabsetPanel(session, "tabselected",
                        selected = "success")
    }
    
  })

  output$local <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
                          tagList(div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Where you would like you integrated data stored?</b> "))),HTML('<br><br><br>'),
                                  fluidRow(column(12,
                                                  renderUI(fluidPage(shinydashboard::box(title = HTML('<strong><font color="#000000">Local environment</font></strong>'),
                                                                                         tagList(HTML('In order to load the LSAY 2009, you need to indicate where it is located on your computer. Please indicate it in the designated field below.')),width = 1000,height = 1000,status = "success",solidHeader = T,
                                                                                         HTML('<br><br>'),   
                                                                                         tagList(fluidRow(tags$img(src = "folder.png",height = 150, width = 150,align = "center")),
                                                                                                 HTML('<br>'),   
                                                                                                 fluidRow(column(3,""),column(6,tagList(
                                                                                                   HTML('<strong><font color="#000000">Please indicates the folder where is located the LSAY 09 in Stata format:</font></strong>'),aceEditor(
                                                                                                   outputId = "path_files",height = "50px",
                                                                                                   value = '',,debounce=0,
                                                                                                   placeholder = "Please introduce your absolute path. For example: C:\\Users\\example\\Documents\\LSA09"
                                                                                                 ))),column(3,""),
                                                                                                 tagList(fluidRow(column(6,ButtonContent(buttonAction(outputId='back_local', label='Back',color = "#759AAF"))),
                                                                                                                  column(6,ButtonContent(buttonAction(outputId='continue_local', label='Continue',color = "#759AAF"))))),
                                                                                                 )))))))))))))
  
  output$Report = downloadHandler(
      filename =  "Geosocial.zip",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        #### ------ Cross sectional ------ #####
        if (length(input$long_waves)==1) {
          msg0 = paste0('The round (',toString(unique(input$TSP_years)),') of the TSP (2021) are going to linked with the wave ' ,toString(unique(input$long_waves)),' of the Longitudinal survey used in the linkage.')
        } else {
          msg0 = paste0('The rounds ',toString(unique(input$TSP_years)),' of the TSP (2021) are going with the waves: ',toString(unique(input$long_waves)),' of the Longitudinal survey used in the linkage.')
        }
        msg1 = "The Geographical unit used to do the data linkage was SA3 2021. More information about this: https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/latest-release"
        ###### ----- Concordances log -------- ####
        msg2 = "The Census information that are going linked to the longitudinal survey are:"
        msg3 = ""
        for (j in unique(input$TSP_variables)){
          msg3 = paste0(msg3, paste0('- ',j, ' \n'))
        }
        
        ###### ----- Concordances log -------- ####
        msg4 ="The longitudinal topic that are going linked are:"
        
        msg5 = ""
        for (j in unique(input$long_topic)){
          msg5 = paste0(msg5, paste0('- ',j, ' \n'))
        }
        

        
        # Set up parameters to pass to Rmd document
        params <- list(msg0=msg0,msg1=msg1,msg2=msg2,msg3=msg3,msg4=msg4,msg5=msg5)
        
      
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        
        path = getwd()
        
        tempReportpdf <- file.path(tempdir(), "Cookbook.pdf")
        
        
        relativepath = tempdir()
        
        file.copy("IRISS_1.0.tar.gz", relativepath, overwrite = TRUE)
        file.copy("IRISS_1.0.pdf", relativepath, overwrite = TRUE)
        file.copy("main.R", relativepath, overwrite = TRUE)

        write(CreateParameters(input$token_ada, input$path_files ,input$longitudinal, input$long_waves, input$long_topic, input$TSP, input$TSP_years, input$TSP_variables), paste0(relativepath,"/Parameters.json"))
        
        
        rmarkdown::render(tempReport, output_file = tempReportpdf,output_format = 'pdf_document',
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        setwd(relativepath)
        
        zip(zipfile = file, files = c("Cookbook.pdf","IRISS_1.0.tar.gz","IRISS_1.0.pdf","main.R",'Parameters.json'))
        setwd(path)
        
        
      }
    )
  
  
  
  CreateParameters = function(token_ada,path_files, longitudinal,long_waves,long_topic, TSP, TSP_years, TSP_variables){
    if(!is.null(token_ada)) {
      if (token_ada!=''){
        withAPI = TRUE
        path_files = ""
      } else {
        withAPI = FALSE
      }
    } else if (!is.null(path_files)){
      if (path_files!=''){
        withAPI = TRUE
        token_ada = ""
        
      } else {
        withAPI = FALSE

      }
    }
    
    
    # longitudinal_choices waves_choices long_topic_choices TSP_choices TSP_years_choices TSP_variables_choices
    

    parameters = list()
    parameters[['withAPI']] = withAPI
    
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

    
    parameters[['SurveyFiles']] = path_files 
    parameters[['LSAY_cohort']] = 2009
    parameters[['TSP_year']] = TSP_years
    parameters[['LSAY_waves']] = long_waves
    parameters[['TSP_variables']] = TSP_variables
    
    myfile = toJSON(parameters)
    
    return(myfile)
    
  }
  
  output$Dparameters = downloadHandler(
    filename =  "config.json",
    content = function(file) {write(CreateParameters(input$token_ada, input$path_files ,input$longitudinal, input$long_waves, input$long_topic, input$TSP, input$TSP_years, input$TSP_variables), file)}
    )
  
  

  observeEvent(input$back_parameters, {
    updateTabsetPanel(session, "tabselected",
                      selected = "data")
  })
  
  output$success <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
          tagList(div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Thank you, we generated the suitable parameters for the data linkage</b> "))),HTML('<br><br><br>'),
          fluidRow(column(12,boxg(color = '#dde6cd',id='subbox5',height= "400px",left='-1',right='-1',
          content=tagList(HTML('<br><br><br><br>'),fluidPage(column(8,fluidRow(
          column(4,fluidRow(tags$img(src = "download.png",height = 150, width = 180,align = "center"))),
          column(6,tagList(
          fluidRow(buttonDownload(outputId='Report', label='Download GeoSocial',color = "#27A3A0",width='200px')))))),
          column(4,centerContentLinks(fluidPage(fluidRow( tags$h3('Resources',style='color: black;font-weight: bold;',style='text-decoration: underline;')),
                                                fluidRow( tags$a(href="https://www.lsay.edu.au/publications/search-for-lsay-publications/2547","• LSAY 2009 cohort User guide",style='text-decoration: underline;')),
                                                fluidRow(tags$a(href="https://www.lsay.edu.au/publications/search-for-lsay-publications/2621", "• LSAY Data Dictionary",style='text-decoration: underline;')),
                                                fluidRow(tags$a(href="https://www.lsay.edu.au/publications/search/y09-questionnaires-and-frequency-tables", "• LSAY 2009 Questionnaires and frequency tables",style='text-decoration: underline;')))))))))),
          HTML('<br><br>')
          ,fluidRow(column(4,ButtonContent(buttonAction(outputId='back_parameters', label='Back',color = "#759AAF"))),
                    column(8,""))
          ))))))
          # output$Tab_1 <- renderUI(
          #   tagList( HTML('<br><br><br><br>'),
  #            tabBox(side = "left", height = 2000,width = 550,selected = "API",
  #                   tabPanel(title = "API",renderUI(fluidPage(
  #                     shinydashboard::box(title = HTML('<strong><font color="#000000">Credentials</font></strong>'),
  #                                         HTML('<font color="#000000">Please insert your credentials</font>'),width = 1000,height = 1000,status = "success",solidHeader = T,
  #                                         tagList(fluidRow(aceEditor(
  #                                           outputId = "someID",
  #                                           value = read.delim("config.yml"),
  #                                           placeholder = "Connection configuration file"
  #                                         ), 
  #                                         downloadButton("config", label = "Save Configuration"),
  #                                         )))))),
  #                   tabPanel(title = "Local",
  #                            box(title = HTML('<strong><font color="#ffffff"></font></strong>'),width = 1000,height = 1500,status = "primary",solidHeader = T,
  #                                tagList(tagList(HTML('<p align="justify" <p style="font-family:verdana;">Users can load local datasets<br>'),
  #                                                textOutput(outputId = "Please select your directory"),
  #                                                shinyFilesButton('dir', 'Please select your database', 'Select a database', FALSE,icon = icon("archive")),
  #                                                HTML('<br><br>'),
  #                                                renderText(ifelse(test = length(input$dir) == 1,no = paste0("You have chosen: ",path_utilizar(),'. Do you want to continue? '),yes = "")),
  #                                                downloadButton("local", label = "Save Configuration"),
  #                                                HTML('<br> '))))))))
  # 
  
  # output$files <- renderUI(
  #   tagList( HTML('<br><br><br><br>'),
  #            tabBox(side = "left", height = 2000,width = 550,selected = "API",
  #                   tabPanel(title = "API",renderUI(fluidPage(
  #                     shinydashboard::box(title = HTML('<strong><font color="#000000">Credentials</font></strong>'),
  #                                         HTML('<font color="#000000">Please insert your credentials</font>'),width = 1000,height = 1000,status = "success",solidHeader = T,
  #                                         tagList(fluidRow(aceEditor(
  #                                           outputId = "someID",
  #                                           value = read.delim("config.yml"),
  #                                           placeholder = "Connection configuration file"
  #                                         ), 
  #                                         downloadButton("config", label = "Save Configuration"),
  #                                         )))))),
  #                   tabPanel(title = "Local",
  #                            box(title = HTML('<strong><font color="#ffffff"></font></strong>'),width = 1000,height = 1500,status = "primary",solidHeader = T,
  #                                tagList(tagList(HTML('<p align="justify" <p style="font-family:verdana;">Users can load local datasets<br>'),
  #                                                textOutput(outputId = "Please select your directory"),
  #                                                shinyFilesButton('dir', 'Please select your database', 'Select a database', FALSE,icon = icon("archive")),
  #                                                HTML('<br><br>'),
  #                                                renderText(ifelse(test = length(input$dir) == 1,no = paste0("You have chosen: ",path_utilizar(),'. Do you want to continue? '),yes = "")),
  #                                                downloadButton("local", label = "Save Configuration"),
  #                                                HTML('<br> '))))))))
  observeEvent(input$Back_parameters, {
    updateTabsetPanel(session, "tabselected",
                      selected = "resources")
  })
  
  output$parameters <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
          tagList(div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>What data would you like to integrate?</b> "))),HTML('<br><br><br>'),
                  fluidRow(column(6,
                                  fluidRow(column(4,pickerInput(inputId = "longitudinal",label = "Longitudinal Survey:", choices =  longitudinal_choices, options =pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon-chevron-left',title = "Longitudinal survey",header = "Please select one" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "long_waves",label = "Years/Waves:", choices = waves_choices, multiple=TRUE, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon-chevron-left',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "long_topic",label = "Sub-major topic area:", choices = long_topic_choices, multiple=TRUE, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon-chevron-left',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))))),
                           column(6,boxg(color = '#dde6cd',id='subbox10',height= "200px",left='-3',right='-1',content=centerContentLinks(fluidPage(fluidRow( tags$h3('Resources',style='color: black;font-weight: bold;',style='text-decoration: underline;')),
                                                                                                                                                   fluidRow( tags$a(href="https://www.lsay.edu.au/publications/search-for-lsay-publications/2547","• LSAY 2009 cohort User guide",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(href="https://www.lsay.edu.au/publications/search-for-lsay-publications/2621", "• LSAY Data Dictionary",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(href="https://www.lsay.edu.au/publications/search/y09-questionnaires-and-frequency-tables", "• LSAY 2009 Questionnaires and frequency tables",style='text-decoration: underline;'))))))),
                  fluidRow(column(6,
                                  fluidRow(column(4,pickerInput(inputId = "TSP",label = "DataPack", choices = TSP_choices, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon-chevron-left',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "TSP_years",label = "Census", multiple=TRUE, choices = TSP_years_choices, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon-chevron-left',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "TSP_variables",label = " Variables:", multiple=TRUE, choices = TSP_variables_choices, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon-chevron-left',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))))),
                           column(6,boxg(color = '#c9cee8',id='subbox11',height= "200px",left='-3',right='-1',content=centerContentLinks(fluidPage(fluidRow( tags$h3('Resources',style='color: black;font-weight: bold;')),
                                                                                                                                              fluidRow( tags$a(href="https://www.abs.gov.au/census/2021-census-data-release-plans/2021-census-product-release-guide#datapacks","• ABS Time series profile",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(href="https://www.abs.gov.au/census/guide-census-data/geography", "• Understanding Census geography",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(href="https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/main-structure-and-greater-capital-city-statistical-areas/statistical-area-level-2", "• ASGS SA2 2021",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(href="https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/correspondences", "• Correspondences",style='text-decoration: underline;'))))))))
          ,HTML('<br><br><br><br>'),fluidRow(
            column(6,ButtonContent(buttonAction(outputId='Back_parameters', label='Back',color = "#759AAF"))),
            column(6,ButtonContent(buttonAction(outputId='Continue', label='Continue',color = "#759AAF")))))))))
  
  
  observeEvent(input$Continue, {
    if (is.null(input$longitudinal) | is.null(input$long_waves) | is.null(input$long_topic) | is.null(input$TSP) | is.null(input$TSP_years) | is.null(input$TSP_variables)) {
        shinyalert(title='Please select more than one variable',html = T)
    } else {
      updateTabsetPanel(session, "tabselected",
                        selected = "data")
    }
    
    

  })
  
  # observeEvent(input$Continue, {
  #   #if (length(input$TSP_years)==1) {
  #     shinyalert(text =buttonAction(outputId='ContinueParameters', label='Continue',color = "#759AAF"),
  #       title='Your ',html = T
  #     )
  #   # } else {
  #   #   shinyalert(text =buttonAction(outputId='ContinueParameters', label='Continue',color = "#759AAF"),
  #   #     title='Longitudinal',html=T
  #   #   )
  #   # }
  # 
  # })
        
        
        
        
        
  
  output$Tab_3 <- renderUI(
    tagList(HTML('<br><br><br><br>'),HTML('Menu 3')))
  
  
  ################################## ----------------  Server ----------------  ##################################
})
