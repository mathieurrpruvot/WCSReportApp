#
# This application is meant to produce automated reports needed for regular reporting
# 
##
# TO DO:
# Add WCS logo
# Fix the pdf output 
# Creat the main panel with
#   -screenshot of what the report looks like
#   -explanation of what the report does
#   -file required to produce this report


library(shiny)

ui = fluidPage(
img(src = "WCS.png",height=100),    
titlePanel(strong("Report Generator")),
sidebarLayout(
    sidebarPanel(
        radioButtons('rmdselect', 'Select report type', c('test4loop', 'Report2', 'Report3', 'Report4', 'Report5'), inline = F),
        fileInput("datain", "Upload data", buttonLabel = "Choose file..."),
        radioButtons('format', 'Select document format', c('PDF', 'HTML', 'Word'),inline = TRUE),
        downloadButton("report", "Generate report")
    ),
    mainPanel(
       
        h2("Report description"),
        textOutput("report_descr"),
        h2("Data requirements"),
        textOutput("data_req"),
        
    )
)
)


server = function(input, output,session) {
    
           output$report_descr <- renderText({ 
               
               ifelse(input$rmdselect=='test4loop',
                   "This report is designed to produce a comprehensive overview of surveillance across several countries. The report goes through a few global statistics, and then produces a country-by-country summary of sampling and surveillance performance indicators",
                   ifelse(input$rmdselect=='Report2',
                          "test some text for report 2",
                          ifelse(input$rmdselect=='Report3',
                          "test some text for test4loop","No report selected")))
           })
           
           output$data_req <- renderText({ 
               
               ifelse(input$rmdselect=='test4loop',
                      "To generate this report, you will need a csv export of WHIP containing data from several countries.",
                      ifelse(input$rmdselect=='Report2',
                             "To generate report 2, you need to upload some data with these characteristics",
                             ifelse(input$rmdselect=='Report3',
                                    "To generate report 3, you need to upload some data with these characteristics","Not available")))
           })
           
           
           output$report <- downloadHandler(
            filename = function() {
                paste(switch(input$rmdselect, test4loop='test4loop',Report2='Report2',Report3='Report3'), sep = '.', switch(
                    input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
            },
            content = function(file) {
                report<-paste(switch(input$rmdselect, test4loop='test4loop',Report2='Report2',Report3='Report3'), sep = '.',"Rmd")
                src <- normalizePath(report)
                if (input$rmdselect=='test4loop'){
                    countryrmd <- normalizePath('test4_countrygeneric.Rmd')
                    perfrmd <- normalizePath('test4_perfindic_generic.Rmd')
                }
                
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, report, overwrite = TRUE)
               
                 if (input$rmdselect=='test4loop'){
                    
                file.copy(countryrmd, 'test4_countrygeneric.Rmd', overwrite = TRUE)
                file.copy(perfrmd, 'test4_perfindic_generic.Rmd', overwrite = TRUE)
                     
                 }
                #try to do the same for the connected rmd
                
               
                # Set up parameters to pass to Rmd document
                #params <- list(n = input$slider)
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                library(rmarkdown)
                out <- render(report, switch(
                    input$format,
                    PDF = pdf_document(), HTML = html_document(), Word = word_document()
                ))
                file.rename(out, file)
            }
        )
    }

# Run the application 
shinyApp(ui = ui, server = server)
