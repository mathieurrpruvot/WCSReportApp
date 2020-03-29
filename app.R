#
# This application is meant to produce automated reports needed for regular reporting
# 
##
# TO DO:
# Add WCS logo
# Find a fix to allow the looped Rmd
# Fix the pdf output 
# Creat the main panel with
#   -screenshot of what the report looks like
#   -explanation of what the report does
#   -file required to produce this report


library(shiny)

ui = fluidPage( 
sidebarLayout(
    sidebarPanel(
        fileInput("datain", "Data", buttonLabel = "Upload..."),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE),
        radioButtons('rmdselect', 'Select report type', c('test4loop', 'Report2', 'Report3', 'Report4', 'Report5'), inline = F),
        downloadButton("report", "Generate report")
    ),
    mainPanel(
        
    )
)
)


server = function(input, output,session) {
    

           output$report <- downloadHandler(
            filename = function() {
                paste(switch(input$rmdselect, test4loop='test4loop',Report2='Report2',Report3='Report3'), sep = '.', switch(
                    input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
            },
            content = function(file) {
                report<-paste(switch(input$rmdselect, test4loop='test4loop',Report2='Report2',Report3='Report3'), sep = '.',"Rmd")
                src <- normalizePath(report)
                
                # temporarily switch to the temp dir, in case you do not have write
                # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, report, overwrite = TRUE)
                
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
