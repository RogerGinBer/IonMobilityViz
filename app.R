library(shiny)
library(ggplot2)
# library(shinyWidgets)
library(cowplot)
library(DT)
library(Spectra)
library(opentimsr)
library(MsBackendTimsTof)
library(dplyr)

options(shiny.maxRequestSize=1000*1024^2)

so_folder <- tempdir()
so_file <- download_bruker_proprietary_code(so_folder, method = "wget")
setup_bruker_so(so_file)

# Define UI for application that draws a histogram
ui <- navbarPage("Ion mobility data visualizer",
                 tabPanel("Plot",
                          sidebarLayout(
                              sidebarPanel(
                                  fileInput("datafile", label = "Enter xcmsExperiment in an RDS format", multiple = FALSE, accept = "rds"),
                                  sliderInput("rt",
                                              "Retention time range:",
                                              min = 1,
                                              max = 50,
                                              step = 0.01,
                                              value = c(10, 20)),
                                  sliderInput("im",
                                              "Ion mobility range:",
                                              min = 1,
                                              max = 50,
                                              step = 1e-6,
                                              value = c(10, 20)),
                                  sliderInput("mz",
                                              "Mz range:",
                                              min = 1,
                                              max = 50,
                                              step = 1e-6,
                                              value = c(10, 20)),
                                  downloadButton('downloadPlot','Download Plot'),
                                  actionButton("removeSelection", "Reset peak selection"),
                                  plotOutput("lengthHistogram", brush = brushOpts(id = "plot_brush", fill = "#ccc", direction = "x")),
                                  width = 3
                              ),
                              mainPanel(
                                  DT::dataTableOutput("peaktable"),
                                  plotOutput("outputPlot", height = "700px")
                              ))
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    xcmsObject <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        xcmsExp <- readRDS(infile$datapath)
        xcmsExp
    })
    
    chromPks <- reactive({
        obj <- xcmsObject()
        if (is.null(obj)) return(NULL)
        return(xcms::chromPeaks(obj))
    })
    
    spectra <- reactive({
        obj <- xcmsObject()
        if (is.null(obj)) return(NULL)
        message("Calculating spectra")
        return(do.call(rbind,
                       peaksData(obj@spectra[msLevel(obj@spectra) == 1],
                                 c("mz", "intensity", "retention_time", "inv_ion_mobility"))))
    })
    
    proxy <- dataTableProxy('peaktable')
    
    observeEvent(input$removeSelection, {
        selectRows(proxy, NULL)
    })
    
    output$lengthHistogram <- renderPlot({
        pks <- chromPks()
        if(is.null(pks)){return(NULL)}
        ggplot(mutate(as.data.frame(pks), length = rtmax - rtmin)) +
            geom_histogram(aes(x=length)) + 
            ggtitle("Peak length distribution")
    })
    
    output$peaktable <- renderDataTable(chromPks())
    
    observeEvent(chromPks(),{
        updateSliderInput(inputId = "rt",
                          min = round(min(chromPks()[,"rtmin"]), 2),
                          max = round(max(chromPks()[,"rtmax"]), 2))
        updateSliderInput(inputId = "mz",
                          min = round(min(chromPks()[,"mzmin"]), 2),
                          max = round(max(chromPks()[,"mzmax"]), 2))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$peaktable_rows_selected,{
        rows <- input$peaktable_rows_selected
        if(is.null(rows)){return(NULL)}
        peaks <- chromPks()[rows,,drop=FALSE]
        updateSliderInput(inputId = "rt",
                          min = round(min(peaks[,"rtmin"]), 2)-10,
                          max = round(max(peaks[,"rtmax"]), 2)+10,
                          value = c(round(min(peaks[,"rtmin"]), 2),
                                    round(max(peaks[,"rtmax"]), 2)))
        updateSliderInput(inputId = "mz",
                          min = round(min(peaks[,"mzmin"]), 6)-1e-2,
                          max = round(max(peaks[,"mzmax"]), 6)+1e-2,
                          value = c(round(min(peaks[,"mzmin"]), 6),
                                    round(max(peaks[,"mzmax"]), 6)))
        updateSliderInput(inputId = "im",
                          min = round(min(peaks[,"immin"]), 3)-0.3,
                          max = round(max(peaks[,"immax"]), 3)+0.3,
                          value = c(round(min(peaks[,"immin"]), 3),
                                    round(max(peaks[,"immax"]), 3)))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    plot <- reactive({
        if(is.null(spectra())){return(NULL)}
        
        s <- as.data.frame(spectra())
        data <- dplyr::filter(s,
                              dplyr::between(retention_time, as.numeric(input$rt[[1]]), as.numeric(input$rt[[2]])),
                              dplyr::between(mz, as.numeric(input$mz[[1]]), as.numeric(input$mz[[2]])),
                              dplyr::between(inv_ion_mobility, as.numeric(input$im[[1]]), as.numeric(input$im[[2]])))
        
        if(nrow(data) == 0){return(NULL)}
        if(nrow(data) > 1e4){message("Too many datapoints"); return(NULL)}
        
        
        main_rtmz <- ggplot(data) + geom_point(aes(x=retention_time, y=mz, color = log10(intensity))) + theme_minimal() + theme(legend.position = "none") 
        main_immz <- ggplot(data) + geom_point(aes(x=inv_ion_mobility, y=mz, color = log10(intensity)))  + theme_minimal() +theme(legend.position = "none") 
        
        
        margin_rt <- do.call(cbind, summarize(group_by(data, retention_time), intensity = sum(intensity)))  
        margin_rt_plot <- ggplot(as.data.frame(margin_rt)) + geom_line(aes(x=retention_time, y=intensity)) + theme_minimal()
        
        margin_im <- do.call(cbind, summarize(group_by(data, inv_ion_mobility), intensity = sum(intensity))) 
        margin_im_plot <- ggplot(as.data.frame(margin_im)) + geom_line(aes(x=inv_ion_mobility, y=intensity)) + theme_minimal()
        
        margin_mz <- do.call(cbind, summarize(group_by(data, mz), intensity = sum(intensity))) 
        margin_mz_plot <- ggplot(as.data.frame(margin_mz)) + geom_point(aes(x=mz, y=intensity)) + coord_flip() + theme_minimal()
        
        
        plot <- cowplot::plot_grid(margin_im_plot, margin_rt_plot, NULL,
                                   main_immz,      main_rtmz,      margin_mz_plot,
                                   rel_heights = c(0.3, 0.7),
                                   rel_widths = c(0.4, 0.4, 0.2),
                                   nrow = 2, ncol = 3)
        return(plot)
    })
    
    output$outputPlot <- renderPlot(plot())
    
    output$downloadPlot <- downloadHandler(
        filename = function(){paste("IM_plot",'.svg',sep='')},
        content = function(file){
            ggsave(file, plot = plot(), device = "svg", width = 10, height = 7)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
