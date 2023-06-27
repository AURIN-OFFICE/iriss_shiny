################################## ---------------- Servidor: ----------------  ##################################

server = shinyServer(function(input, output, session) {
  
  ####### -------- Possibles options ------- ###### 
  longitudinal_choices = c("LSAY - Cohort 2009")
  waves_choices = paste0('Wave ',c(2:9),' - ', c(2010:2019))
  long_topic_choices = unique(LSAY$`Sub-major topic area`)
  TSP_choices = c("Time Series Profile 2021") 
  TSP_years_choices = c(2011,2016,2021)  
  TSP_variables_choices = unique(TSP$variable)
  

  output$toolbox <- downloadHandler(
    filename =  "Toolbox.tar.gz",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy("geosocial_1.0.tar.gz", file, overwrite = TRUE)
      
    }
  )
  
  output$manual <- downloadHandler(
    filename =  "Toolbox.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy("geosocial_1.0.pdf", file, overwrite = TRUE)
      
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
  
  
  output$parameters_d <- downloadHandler(
    filename =  "parameters.json",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy("parameters.json", file, overwrite = TRUE)
      
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
                      selected = "users")
  })


  
  observeEvent(input$training, {
    updateTabsetPanel(session, "tabselected",
                      selected = "panel4")
  })
  

  observeEvent(input$start_guided, {
    updatePickerInput(session,"longitudinal",selected = "")
    updatePickerInput(session,"long_waves",selected = "")
    updatePickerInput(session,"long_topic",selected = "")
    updatePickerInput(session,"TSP",selected = "")
    updatePickerInput(session,"TSP_years",selected = "")
    updatePickerInput(session,"TSP_variables",selected = "")
    
    updateTabsetPanel(session, "tabselected",
                      selected = "parameters")
  })
  
  observeEvent(input$start_advance, {
    updateTabsetPanel(session, "tabselected",
                      selected = "resources")
  })
  
  
  
  output$home  = renderUI(fluidRow(div(style='width: 100%; height: 1000px; overflow: hidden; position: relative;',
                                           tagList(HTML('<br>'),HTML('<br>'),HTML('<br>'),HTML('<br>'),
                                                   fluidRow(column(6,boxd(color = '#96B243',id='subbox16',height= "calc(40vh)",left='3',right='3',
                                                                                   content=tagList(fluidRow(div(style="font-size: 16px; padding-top: 3%; color: white; font-family: Arial;",HTML("<p style='text-align:center;'><b>IRISS project</b></p>"))),fluidRow(column(12,centerContent(tagList(fluidRow(tags$img(src = "logo4.png",height = "100%", width = "90%",align = "center")),
                                                                                                                                           fluidRow(HTML('')))))),HTML('<br>'),
                                                                                                   div(style="font-size: 16px; padding-left: 5%; padding-right: 5%;color: white; font-family: Arial; overflow-y:hidden;",
                                                                                                       HTML('<br>'),HTML("<p style='text-align:center;'>The Integrated Research Infrastructure for the Social Sciences (IRISS) project addresses the fragmentation of the Australian social science research infrastructure, establishing a new foundation for integrating data, analysis and platforms for social science research in Australia. </p>"))))),
                                                            column(6,boxd(color = '#96B243',id='subbox15',height= "calc(40vh)",left='3',right='3',
                                                                          content=tagList(fluidRow(div(style="font-size: 16px; padding-top: 3%; color: white; font-family: Arial;",HTML("<p style='text-align:center;'><b>GeoSocial</b></p>")))
                                                                                          ,fluidRow(column(6,centerContent(tagList(fluidRow(tags$img(src = "people.png",height = "30%", width = "40%",align = "center")),
                                                                                                                                  fluidRow(HTML('Longitudinal survey'))))),
                                                                                                   column(6,centerContent(tagList(fluidRow(tags$img(src = "intro1.png",height = "28%", width = "40%",align = "center")),
                                                                                                                                  fluidRow(HTML('Geospatial data')))))),HTML('<br>'),
                                                                                          div(style="font-size: 16px; padding-left: 5%; padding-right: 5%;color: white; font-family: Arial; overflow-y:hidden;",
                                                                                              HTML("<p style='text-align:center;'>The GeoSocial solution allows 
                                                                            researchers to linkage Australia’s largest longitudinal surveys with geospatial statistical data derived 
                                                                                 from the Australian Census of Population and Housing. GeoSocial will 
                                                                                 empower Australia's large cross-disciplinary social research community 
                                                                                 to identify patterns, make predictions, and inform social policy using 
                                                                                 rich integrated GeoSocial data.</p>"))))
                                                            )),HTML('<br>'),
                                                   div(boxd(color = '#45A0EF',id='subbox1s',height= "calc(40vh)",left='3',right='3',
                                                        content=tagList(fluidRow(column(12,div(style="font-size: 16px; padding-top: 3%; padding-left: 2%;padding-right: 2%; color: white; font-family: Arial;",HTML("<p style='text-align:justify;'><b>How data linkage works?
                                                                                </b> <br><br> GeoSocial utilizes the geographical identifier from the longitudinal survey and converts it to a Statistical Areas Level 3 (SA3s) for linking with geospatial statistical data obtained from the Australian Census of Population and Housing. The Geosocial output retains the original format of the longitudinal survey, with the addition of geospatial variables as a new column.
                                                                                It is the responsibility of the user to:
                                                                                <br> &#8203 &#8203 • Request access to the Longitudinal Surveys of Australian Youth datasets.
                                                                                <br> &#8203 &#8203 • Set up a safe environment according to the data custodians' policies.
                                                                                <br> &#8203 &#8203 • <a href='https://cran.r-project.org'>Install R</a> and required dependencies")))),
                                                                        fluidRow(column(9,div(style="font-size: 16px; padding-top: 1%; padding-left: 3%;padding-right: 2%; color: white; font-family: Arial;",HTML("<p style='text-align:justify;'>
                                                                                The GeoSocial solution is composed of the following elements:
                                                                                <br><b> &#8203 &#8203 • Toolbox:</b> R library that has all the R functions you need for data linkage.
                                                                                <br><b> &#8203 &#8203 • Parameters:</b> File with all the relevant information for data linkage, including data locations, API credentials, wave and cohort information.
                                                                                <br><b> &#8203 &#8203 • Script:</b> Used to execute the workflow which will use the toolbox to read and merge the data based on user preferences.
                                                                                <br>GeoSocial does not collect or retain any personally identifying information.</p>"))),
                                                                                column(3,align="center",div(style="font-size: 20px; color: white; font-family: Arial;",
                                                                                                            tagList(fluidRow(tags$img(src = "tool.png",height = "80px", width = "120px",align = "center")),
                                                                                                                    fluidRow(HTML('GeoSocial'))))))))),HTML('<br>'),
                                                      fluidRow(column(2,''),column(8,''),column(2,align="center",ButtonContent(buttonAction(width = "100px",outputId='start_home', label='Start',color = "#F5822B"))))))))
                                                      
  

  output$users  = renderUI(fluidRow(div(style='width: 100%; height: 1000px; overflow: hidden; position: relative;',
                                        tagList(HTML('<br><br><br><br><br>'),
                                                fluidRow(column(6,boxd(color = '#96B243',id='s',height= "calc(70vh)",left='3',right='3',
                                                                       content=tagList(div(style="font-size: 20px; padding-left: 5%; padding-right: 5%;color: white; font-family: Arial; overflow-y:hidden;",
                                                                                           HTML('<br>'),HTML("<p style='text-align:center;'><b>Guided data linkage</b></p>")),
                                                                                       tags$img(src = "data.png",height = "110px%", width = "120px",align = "center"),HTML('<br>'),HTML('<br>'),
                                                                                       fluidRow(div(style="font-size: 18px; padding-left: 5%; padding-right: 5%;color: white; font-family: Arial; overflow-y:hidden;", HTML("<p style='text-align:center;'>We have developed a pipeline to guide you through the components involved in the linkage. The guided option provides:"))),
                                                                                       fluidRow(column(12,BulletsContent(HTML(markdown::renderMarkdown(text = "- Easy access to the data.\n- Certainty regarding data meanings.\n- Less room for analytic errors.\n- Increased data usability and utility to untrained users. \n"))))),
                                                                                       ButtonContent(buttonAction(outputId='start_guided', label='Select',color = "#799d0d"))))),
                                                         column(6,boxd(color = '#ed8333',id='s',height= "calc(70vh)",left='3',right='3',
                                                                       content=tagList(div(style="font-size: 20px; padding-left: 5%; padding-right: 5%;color: white; font-family: Arial; overflow-y:hidden;",
                                                                                           HTML('<br>'),HTML("<p style='text-align:center;'><b>Self-guided data linkage</b></p>")),
                                                                                       tags$img(src = "coding.png",height = "120px%", width = "120px%",align = "center"),HTML('<br>'),HTML('<br>'),
                                                                                       fluidRow(div(style="font-size: 18px; padding-left: 5%; padding-right: 5%;color: white; font-family: Arial; overflow-y:hidden;", HTML("<p style='text-align:center;'>We have allowed you to customise your data pipeline and personalize the data linkage. The self-guided option is suitable if you are:"))),
                                                                                       fluidRow(column(12,BulletsContent(HTML(markdown::renderMarkdown(text = "- Confident with using Python and/or R for data wrangling, integration, and analysis.\n- Familiar with geospatial data.\n- Adding new datasets. \n- Supporting other social science researchers."))))),
                                                                                       ButtonContent(buttonAction(outputId='start_advance', label='Select',color = "#eb6601")))))),
                                                HTML('<br>'),
                                                fluidRow(column(2,ButtonContent(buttonAction(outputId='Back_home', label='Back',color = "#759AAF"))),column(6,''),column(2,''))))))
  
  
  
  output$resources <- renderUI( tagList(HTML('<br><br><br>'),div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Resources</b> "))),HTML('<br><br><br>'),
                        div(fluidRow( column(12,
                        fluidRow(
                        column(4,boxd(color = '#91ae3b',id='subbox1s',height= "calc(50vh)",left='-1',right='-3',content=centerContent(tagList(fluidRow(column(12,HTML("TOOLBOX"),HTML('<br>'))),
                            fluidRow(column(12,tags$img(src = "response.png",height = "30%", width = "30%",align = "center"))),HTML('<br><br>'),
                            fluidRow(div(style="padding-left: 8%;padding-right: 8%;font-family: Arial;",
                                         HTML("<p style='text-align:center;'>R library with essential functions needed for data linkage. </p>"))),
                            HTML('<br>'),
                            div(style="padding-bottom: 50%;font-family: Arial;",fluidRow(column(12,buttonDownload(outputId = "toolbox",label = "Download",color = "#70930a")))),
                            )))),
                        column(4,boxd(color = '#005baa',id='subbox2s',height= "calc(50vh)",left='-1',right='-3',
                                      content=centerContent(tagList(fluidRow(column(12,HTML("DOCUMENTATION"),HTML('<br>'))),
                                fluidRow(column(12,tags$img(src = "doc.png",height = "30%", width = "30%",align = "center"))),HTML('<br><br>'),
                                fluidRow(div(style="padding-left: 8%; padding-right: 8%;font-family: Arial;",
                                             HTML("<p style='text-align:center;'>Information about each function in the library including a description, arguments, and value.</p>"))),
                                HTML('<br>'),
                                fluidRow(column(12,buttonDownload(outputId = "manual",label = "Documentation",color = "#023c6e"))),
                                )))),
                        column(4,boxd(color = '#5c7e92',id='subbox3s',height= "calc(50vh)",left='-1',right='-3',
                                      content=centerContent(tagList(fluidRow(column(12,HTML("PARAMETERS"),HTML('<br>'))),
                                fluidRow(column(12,tags$img(src = "search.png",height = "35%", width = "30%",align = "center"))),HTML('<br><br>'),
                                fluidRow(div(style="padding-left: 8%;padding-top:4%;  padding-right: 8%;font-family: Arial;",
                                             HTML("<p style='text-align:center;'>File with the definitions that are relevant to the data linkage process: survey, cohort etc. </p>"))),
                                HTML('<br>'),
                                fluidRow(column(12,buttonDownload(outputId = "parameters_d",label = "Download",color = "#445d6c"))),
                                )))))))),
                        fluidRow(column(12,boxg(color = '#e1b82e',id='subbox4s',height= "100%",left='-1',right='-1',
                                                content=centerContent(tagList(fluidRow(column(12,HTML("SCRIPT"),HTML('<br>'))),HTML('<br>'),
                                                                                fluidRow(column(3,leftContent(tags$img(src = "personas.png",height = "50%", width = "50%",align = "center"))),
                                                                                         column(6,div(style="padding-left: 8%; padding-right: 8%;font-family: Arial;",
                                                                                                      HTML("<p style='text-align:center;'>A template of the workflow which uses the toolbox to read and merge the data based on user preferences.</p>")))),
                                                                              HTML('<br>'),fluidRow(column(12,buttonDownload(outputId = "script",label = "Download",color = "#ac8c21"))),
                                                                              HTML('<br><br>')))))),
                        HTML('<br><br>'),fluidRow(column(2,ButtonContent(buttonAction(outputId='Back_resources', label='Back',color = "#759AAF"))),column(6,''),column(2,''))))

  
  observeEvent(input$cloud, {
    updateAceEditor(session, "token_ada", value = '')
    updateAceEditor(session, "path_files", value = '')
    updateTabsetPanel(session, "tabselected",
                      selected = "cloud")
  })
  
  observeEvent(input$local, {
    updateAceEditor(session, "path_files", value = '')
    updateAceEditor(session, "token_ada", value = '')
    updateTabsetPanel(session, "tabselected",
                      selected = "local")
  })
  
  
  observeEvent(input$Back_data, {
    updateTabsetPanel(session, "tabselected",
                      selected = "parameters")
  })
  
  observeEvent(input$Back_home, {
    updateTabsetPanel(session, "tabselected",
                      selected = "home")
  })
  
  observeEvent(input$back_ada, {
    updateTabsetPanel(session, "tabselected",
                      selected = "data")
  })
  
  observeEvent(input$Back_resources, {
    updateTabsetPanel(session, "tabselected",
                      selected = "users")
  })
  
  observeEvent(input$continue_ada, {
    
    if(input$token_ada==''){
      shinyalert(title='Please insert a token.',html = T)
    } else {
      updateTabsetPanel(session, "tabselected",
                        selected = "success")
    }
    
  })
  
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
  
  output$data <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
                          tagList(HTML("<p style='text-align:left;'><b>  <font size='6'>Where would you like your integrated data stored?</font> </b></p>"),HTML('<br>'),
                           HTML('The toolbox allows the user to load the survey data from one of the following sources:'),
                           fluidRow(column(1,''),
                           column(5,fluidPage(fluidRow(tags$h3('Australian Data Archive')),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(tags$img(src = "cloud.png",height = 240, width = 240,align = "center")),
                                              fluidRow(div(style="padding-left: 10%;padding-top:4%;  padding-right: 10%;font-family: Arial;",
                                                           HTML("<p style='text-align:center;'>The survey is provided by ADA, a national service for collecting and preserving digital research data.</p>"))),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(buttonAction(outputId='cloud', width='100px',label='Cloud',color = "#E5C34F")))),
                           column(5,fluidPage(fluidRow(tags$h3('Local environment')),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(tags$img(src = "local.png",height = 240, width = 240,align = "center")),
                                              fluidRow(div(style="padding-left: 15%;padding-top:4%;  padding-right: 15%;font-family: Arial;",
                                                           HTML("<p style='text-align:center;'>The user provides the survey in the local environment where the toolbox is executed.</p>"))),
                                              fluidRow(HTML('<br>')),
                                              fluidRow(buttonAction(outputId='local', width='auto', label='Local',color = "#E5C34F"))))),
                           fluidRow(column(2,ButtonContent(buttonAction(outputId='Back_data', label='Back',color = "#759AAF"))),column(6,''),column(2,''))))))))
            
  


  
  output$cloud <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
                          tagList(div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Where would you like your integrated data stored?</b> "))),HTML('<br><br><br>'),
                                  fluidRow(column(12,
                                                  renderUI(fluidPage(shinydashboard::box(title = HTML('<strong><font color="#759AAF">Australian Data Archive (ADA) API</font></strong>'),
                                                                                            tagList(HTML('Before generating an API token to use the ADA API, it is necessary to obtain approval to access the LSAY 2009 data through ADA.'),tags$a(target="_blank",href="https://dataverse.ada.edu.au/loginpage.xhtml?redirectPage=dataverse.xhtml", "Click here for information.",style='text-decoration: underline;'),HTML('After getting the approval, you can create a token. Please refer to the image below to locate it.')),width = 1000,height = 1000,status = "success",solidHeader = T,
                                                                                         HTML('<br><br>'),   
                                                                                         tagList(fluidRow(tags$img(src = "token.png",height = 200, width = 800,align = "center")),
                                                                                                 HTML('<br>'),  
                                                                                            fluidRow(column(1,""),column(10,tagList(HTML('<br><strong><font color="#759AAF">Please copy and paste the ADA token into the designated field below:</font></strong>'),
                                                                                                                                   aceEditor(
                                                                                                                                     outputId = "token_ada",height = "50px",
                                                                                                                                     value = '',
                                                                                                                                     placeholder = "Please introduce your ADA token",debounce=0
                                                                                                                                   )),
                                                                                                                         HTML('<strong><font color="#E5C34F">We do not collect or upload any information. The token is included in the parameters file that you execute on your computer. </font></strong>'))
                                                                                                     ,column(1,""),
                                                                                            fluidRow(column(6, HTML('<br>'))),
                                                                                            tagList(fluidRow(column(6,ButtonContent(buttonAction(outputId='back_ada', label='Back',color = "#759AAF"))),
                                                                                                             column(6,ButtonContent(buttonAction(outputId='continue_ada', label='Continue',color = "#759AAF"))))),
                                                                                            )))))))))))))


  output$local <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
                          tagList(div(style="position:absolute; padding-left: 0.5%; color: '#434349'; font-family: Arial;",tags$h2(HTML("<b>Where would you like your integrated data stored?</b> "))),HTML('<br><br><br>'),
                                  fluidRow(column(12,
                                                  renderUI(fluidPage(shinydashboard::box(title = HTML('<strong><font color="#759AAF">Local environment</font></strong>'),
                                                                                         tagList(HTML('In order to load the LSAY 2009 cohort, you need to indicate where it is located on your computer.')),width = 1000,height = 1000,status = "success",solidHeader = T,
                                                                                         HTML('<br><br>'),   
                                                                                         tagList(fluidRow(tags$img(src = "folder.png",height = 150, width = 150,align = "center")),
                                                                                                 HTML('<br>'),   
                                                                                                 fluidRow(column(1,""),column(10,tagList(
                                                                                                   HTML('<strong><font color="#759AAF">Please indicate the folder where the LSAY 2009 cohort in Stata format is located:</font></strong><br><br>'),
                                                                                                   aceEditor(
                                                                                                     outputId = "path_files",height = "50px",
                                                                                                     value = '',debounce=0,
                                                                                                     placeholder = "Please introduce your absolute path. For example: C:\\Users\\example\\Documents\\LSA09\\"
                                                                                                 )),
                                                                                                 HTML('<strong><font color="#E5C34F">We do not collect or upload any information. The absolute path is included in the parameters file that you execute on your computer..</font></strong>')
                                                                                                 ),column(1,""),fluidRow(column(6, HTML('<br>'))), 
                                                                                                 tagList(fluidRow(column(6,ButtonContent(buttonAction(outputId='back_local', label='Back',color = "#759AAF"))),
                                                                                                                  column(6,ButtonContent(buttonAction(outputId='continue_local', label='Continue',color = "#759AAF"))))),
                                                                                                 )))))))))))))
  
  output$Report = downloadHandler(
      filename =  "GeoSocial.zip",
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
        
        tempReportpdf <- file.path(tempdir(), "readme.pdf")
        
        
        relativepath = tempdir()
        
        file.copy("geosocial_1.0.tar.gz", relativepath, overwrite = TRUE)
        file.copy("geosocial_1.0.pdf", relativepath, overwrite = TRUE)
        file.copy("main.R", relativepath, overwrite = TRUE)

        write(CreateParameters(input$token_ada, input$path_files ,input$longitudinal, input$long_waves, input$long_topic, input$TSP, input$TSP_years, input$TSP_variables), paste0(relativepath,"/parameters.json"))
        
        
        rmarkdown::render(tempReport, output_file = tempReportpdf,output_format = 'pdf_document',
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        setwd(relativepath)
        
        zip(zipfile = file, files = c("readme.pdf","geosocial_1.0.tar.gz","geosocial_1.0.pdf","main.R",'parameters.json'))
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
        withAPI = FALSE
        path_files = gsub(x=path_files,pattern="\\\\",replacement = "/")
        token_ada = ""
      } else {
        withAPI = TRUE
      }
    }
    
    
    # longitudinal_choices waves_choices long_topic_choices TSP_choices TSP_years_choices TSP_variables_choices


    parameters = list()
    parameters[['with_API']] = withAPI

    
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

    
    parameters[['Survey_files']] = path_files 
    parameters[['LSAY_cohort']] = 2009
    parameters[['TSP_year']] = as.numeric(TSP_years)
    parameters[['LSAY_waves']] = as.numeric(gsub(long_waves,pattern = "Wave.+-",replacement=""))
    parameters[['LSAY_topics']] = long_topic
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
          tagList(HTML("<p style='text-align:left;'><b>  <font size='6'>Thank you, we have generated all the necessary components for the data linkage</font> </b></p>"),HTML('<br><br>'),
          fluidRow(column(12,boxd(color = '#dde6cd',id='subbox5',height= "calc(50vh)",left='-1',right='-1',
          content=tagList(HTML('<br><br><br>'),fluidPage(fluidRow(
          column(3,align="center",div(style="font-size: 20px; color: white; font-family: Arial;", 
                                        tagList(HTML('<br>'),HTML('<br>'),fluidRow(tags$img(src = "tool.png",height = "20%", width = "50%",align = "center")),
                                                fluidRow(HTML('<br><strong><font color="#27A3A0">Step 1: Download GeoSocial</font><strong>')),
                                                HTML('<br>'),
                                                fluidRow(buttonDownload(outputId='Report', label='Download',color = "#27A3A0",width='120px'))))),
          column(3,align="center",div(style="font-size: 20px; color: white; font-family: Arial;",
                                      tagList(HTML('<br>'),HTML('<br>'),fluidRow(tags$img(src = "doc.png",height = "10%", width = "40%",align = "center")),
                                              fluidRow(HTML('<strong><font color="#27A3A0">Step 2: Read "readme.pdf"</font><strong>')),
                                              fluidRow(HTML('It will introduce you to the code and explain each chunk of it.'))))),
          column(3,align="center",div(style="font-size: 20px; color: white; font-family: Arial;",
                                      tagList(HTML('<br>'),HTML('<br>'),fluidRow(tags$img(src = "personas.png",height = "30%", width = "50%",align = "center")),
                                              fluidRow(HTML('<br><strong><font color="#27A3A0">Step 3: Run the code</font><strong>')),
                                              fluidRow(HTML('To start the data linkage, it is necessary to execute the main.R'))))),
          column(3,align="center",div(style="font-size: 20px; color: white; font-family: Arial;",
                                        tagList(HTML('<br>'),HTML('<br>'),fluidRow(tags$img(src = "outputs1.png",height = "10%", width = "40%",align = "center")),
                                                fluidRow(HTML('<strong><font color="#27A3A0">Step 4: See the outputs</font><strong>')),
                                                fluidRow(HTML('The linked data is stored in a new file containing the GeoSpatial variables.')))))
          )),HTML('<br><br>'))))),
          HTML('<br><br><br>'),fluidRow(column(4,ButtonContent(buttonAction(outputId='back_parameters', label='Back',color = "#759AAF"))),
                   column(8,""))
          ))))))
          
  observeEvent(input$Back_parameters, {
    updateTabsetPanel(session, "tabselected",
                      selected = "users")
  })
  
  output$parameters <- renderUI(
    tagList(HTML('<br><br><br><br>'),renderUI(fluidPage(
      shinydashboard::box(width = 1000,height = 1000,status = "success",solidHeader = T,
          tagList(HTML("<p style='text-align:left;'><b>  <font size='6'>What data would you like to integrate?</font> </b></p>"),HTML('<br><br><br>'),
                  fluidRow(column(6,
                                  fluidRow(column(4,pickerInput(inputId = "longitudinal",label = "Longitudinal Survey:", choices =  longitudinal_choices, options =pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon glyphicon-ok',title = "Longitudinal survey",header = "Please select one" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "long_waves",label = "Years/Waves:", choices = waves_choices, multiple=TRUE, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon glyphicon-ok',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "long_topic",label = "Sub-major topic area:", choices = long_topic_choices, multiple=TRUE, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon glyphicon-ok',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))))),
                           column(6,boxd(color = '#dde6cd',id='subbox10',height= "calc(27vh)",left='-3',right='-1',content=centerContentLinks(fluidPage(fluidRow( tags$h3('Survey data documentation',style='color: black;font-weight: bold;',style='color: black;font-weight: bold; text-decoration: underline;')),
                                                                                                                                              fluidRow( tags$a(target="_blank",href="https://www.lsay.edu.au/data/access","• How to access LSAY data",style='text-decoration: underline;')),
                                                                                                                                              fluidRow( tags$a(target="_blank",href="https://www.lsay.edu.au/publications/search-for-lsay-publications/2547","• LSAY 2009 cohort user guide",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(target="_blank",href="https://www.lsay.edu.au/publications/search-for-lsay-publications/2621", "• LSAY variable listing and metadata",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(target="_blank",href="https://www.lsay.edu.au/publications/search/y09-questionnaires-and-frequency-tables", "• LSAY 2009 cohort questionnaires and frequency tables",style='text-decoration: underline;'))))))),
                           fluidRow(column(12,HTML("<br>"))),
                           fluidRow(column(6,
                                  fluidRow(column(4,pickerInput(inputId = "TSP",label = "DataPack", choices = TSP_choices, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon glyphicon-ok',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "TSP_years",label = "Census", multiple=TRUE, choices = TSP_years_choices, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon glyphicon-ok',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))),
                                           column(4,pickerInput(inputId = "TSP_variables",label = " Variables:", multiple=TRUE, choices = TSP_variables_choices, options = pickerOptions(actionsBox = TRUE,liveSearch=TRUE,showTick=TRUE,virtualScroll=TRUE,tickIcon='glyphicon glyphicon-ok',title = "Select more than one",header = "Years of the merge" ,style = "btn-light"))))),
                           column(6,boxd(color = '#c9cee8',id='subbox11',height= "calc(27vh)",left='-3',right='-1',content=centerContentLinks(fluidPage(fluidRow( tags$h3('Geospatial data documentation',style='color: black;font-weight: bold;text-decoration: underline;')),
                                                                                                                                              fluidRow( tags$a(target="_blank",href="https://www.abs.gov.au/census/2021-census-data-release-plans/2021-census-product-release-guide#datapacks","• ABS DataPacks",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(target="_blank",href="https://www.abs.gov.au/census/guide-census-data/geography", "• Understanding Census geography",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(target="_blank",href="https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/main-structure-and-greater-capital-city-statistical-areas/statistical-area-level-3", "• ASGS SA3s",style='text-decoration: underline;')),
                                                                                                                                              fluidRow(tags$a(target="_blank",href="https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/correspondences", "• Geographic correspondences",style='text-decoration: underline;'))))))))
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
