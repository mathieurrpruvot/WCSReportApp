#
# This application is meant to produce automated reports needed for regular reporting
# 
##
# TO DO:
# Fix the pdf output 
# Rename the Rmd test4loop to multicountry

library(shiny)

ui = fluidPage(
img(src = "WCS.png",height=100),    
titlePanel(strong("Report Generator")),
sidebarLayout(
    sidebarPanel(
        radioButtons('rmdselect', 'Select report type', choiceValues=c('test4loop', 'aMapAndAChart', 'Report3', 'Report4', 'Report5'),choiceNames = c('Multi-country', 'Map and Chart for Newsletter', 'Report 3', 'Report 4', 'Report 5'), inline = F),
        fileInput("datain", "Upload data", buttonLabel = "Choose file..."),
        radioButtons('format', 'Select document format', c('PDF', 'HTML', 'Word'),inline = TRUE),
        downloadButton("report", "Generate report")
    ),
    mainPanel(
       
        h2("Report description"),
        textOutput("report_descr"),
        
        h5("Example:"),
        fluidRow(
            column(4,imageOutput("img1")),
            column(4,imageOutput("img2"))        
            ),
   #     fluidRow(
   #         imageOutput("img1"),
   #         imageOutput("img2")        
   #     ),
        h2("Data requirements"),
        textOutput("data_req"),
        
    )
)
)


server = function(input, output,session) {

#add explanations on each report    
           output$report_descr <- renderText({ 
               
               ifelse(input$rmdselect=='test4loop',
                   "This report is designed to produce a comprehensive overview of surveillance across several countries. The report goes through a few global statistics, and then produces a country-by-country summary of sampling and surveillance performance indicators",
                   ifelse(input$rmdselect=='aMapAndAChart',
                          "This tool produces a very brief report with two figures to include in a newsletter. It is recommended to select the Word output format to allow copy and pasting of the images into the draft newsletter.",
                          ifelse(input$rmdselect=='Report3',
                          "test some text for Report 3","No report selected")))
           })

#add screenshot of reports    https://stackoverflow.com/questions/58942332/side-by-side-images-in-r-shiny       
         
          
           
          
           output$img1<-renderImage({
               pic1 <- normalizePath(file.path('./www',paste(switch(input$rmdselect, test4loop='multicountry1',aMapAndAChart='achartandamap1',Report3='Report3'), sep = '.',"png")))
               list(src = pic1,
                    height=300,
                    contentType = 'image/png',
                    alt = "Picture not available")
           }, deleteFile = FALSE)
           
           output$img2<-renderImage({
               pic2 <- normalizePath(file.path('./www',paste(switch(input$rmdselect, test4loop='multicountry2',aMapAndAChart='achartandamap2',Report3='Report3'), sep = '.',"png")))
               list(src = pic2,
                    height=300,
                    contentType = 'image/png',
                    alt = "Picture not available")
           }, deleteFile = FALSE)
           
           
           
           
           output$data_req <- renderText({ 
               
               ifelse(input$rmdselect=='test4loop',
                      "To generate this report, you will need a csv export of WHIP containing data from several countries. Make sure you clean up your data before attempting to generate this report",
                      ifelse(input$rmdselect=='aMapAndAChart',
                             "To generate these figures, you need to upload any csv data file that contains the coordinates of the specimen you want to map, and the 'Date Found' field. You should first make sure that your data is complete (includes all dates and locations), and that these were entered in the appropriate format. In case your file has missing values for 'Date Found', the date will be automatically replaced by the event 'Start date'. Regardless, it is recommended to ensure you have no missing value in your data. Warning: if the area to map is large, the map may take a long time to generate" ,
                             ifelse(input$rmdselect=='Report3',
                                    "To generate report 3, you need to upload some data with these characteristics","Not available")))
           })
           
           
           output$report <- downloadHandler(
            filename = function() {
                paste(switch(input$rmdselect, test4loop='test4loop',aMapAndAChart='amapandachart',Report3='Report3'), sep = '.', switch(
                    input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                ))
            },
            content = function(file) {
                report<-paste(switch(input$rmdselect, test4loop='test4loop',aMapAndAChart='aMapAndAChart',Report3='Report3'), sep = '.',"Rmd")
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
