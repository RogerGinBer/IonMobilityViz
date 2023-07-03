library(shiny)
library(ggplot2)
library(cowplot)
library(DT)
library(Spectra)
library(opentimsr)
library(MsBackendTimsTof)
library(dplyr)
library(bslib)
library(shinyWidgets)


source("./utils.R")

options(shiny.maxRequestSize=1000*1024^2)

theme <- bslib::bs_theme(version = 4)



# Define UI for application that draws a histogram
ui <- navbarPage("Ion mobility data visualizer", 
                 tags$style(
                 "@import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro&display=swap');
                    * {
                        padding: 0;
                        margin: 0;
                        box-sizing: border-box;
                        font-family: 'Source Sans Pro', sans-serif;
                        font-size: 0.95rem
                    }"
                 ),
                 
                 tabPanel("Load your data (Start here!)", mainPanel(
                     h1("Hi there!"),
                     fileInput("datafile", label = "Enter a Spectra or xcmsExperiment object in an RDS format to get started",
                               multiple = FALSE, accept = "rds")
                 )),
                 tabPanel("Raw data", sidebarLayout(
                     sidebarPanel(
                         # fluidRow(column(5, numericInput("rt_raw_st", label = "Retention time range", value = 5,min = 0,max = 10,step = 0.01)),
                         #          column(5, numericInput("rt_raw_end", label = "\n", value = 5,min = 0,max = 10,step = 0.01))
                         # ),
                         sliderInput("rt_raw",
                                     "Retention time range",
                                     min = 1,
                                     max = 50,
                                     step = 0.01,
                                     value = c(10, 20)),
                         sliderInput("im_raw",
                                     "Ion mobility range:",
                                     min = 1,
                                     max = 50,
                                     step = 1e-6,
                                     value = c(10, 20)),
                         sliderInput("mz_raw",
                                     "Mz range:",
                                     min = 1,
                                     max = 50,
                                     step = 1e-6,
                                     value = c(10, 20)),
                         switchInput("peaks_plotBySample", "Plot different samples by color", value = FALSE, labelWidth = "60%"),
                     ),
                     mainPanel(
                         plotOutput("rawPlot", height = "900px")
                     )
                 )),
                 tabPanel("XCMS Peaks",
                          sidebarLayout(
                              sidebarPanel(
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
                 ), theme = theme
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dataObject <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        
        object <- readRDS(infile$datapath)
        if(!any(class(object) %in% c("Spectra", "MsExperiment", "XcmsExperiment"))){
            sendSweetAlert(title = "Error", 
                           text = "The data could not be loaded, please ensure that the format is correct")
            return(NULL)
        }
        sendSweetAlert(title = "Success!",
                       text = "IM data was loaded successfully, now you can explore it in the other tabs.")
        object
    })
    
    chromPks <- reactive({
        obj <- dataObject()
        if (is.null(obj)) return(NULL)
        if (!is(obj, "XcmsExperiment")) return(NULL)
        return(xcms::chromPeaks(obj))
    })
    
    spectra <- reactive({
        obj <- dataObject()
        if (is.null(obj)) return(NULL)
        message("Calculating spectra")
        if (!is(obj, "Spectra") & inherits(obj, "MsExperiment")) obj <- obj@spectra
        
        ms1_data <- obj[msLevel(obj) == 1]
        calculate_4d_matrix_from_Spectra(ms1_data)
    })
    
    observeEvent(spectra(),{
        df <- spectra()
        updateSliderInput(inputId = "rt_raw",
                          min = round(min(df[,"retention_time"], 2)),
                          max = round(max(df[,"retention_time"]), 2))
        updateSliderInput(inputId = "mz_raw",
                          min = round(min(df[,"mz"]), 2),
                          max = round(max(df[,"mz"]), 2))
        updateSliderInput(inputId = "im_raw",
                          min = round(min(df[,"inv_ion_mobility"]), 2),
                          max = round(max(df[,"inv_ion_mobility"]), 2))
    })
    
    rawPlot <- reactive({
        if(is.null(spectra())){return(NULL)}
        s <- as.data.frame(spectra())
        data <- dplyr::filter(s,
                              dplyr::between(retention_time, as.numeric(input$rt_raw[[1]]), as.numeric(input$rt_raw[[2]])),
                              dplyr::between(mz, as.numeric(input$mz_raw[[1]]), as.numeric(input$mz_raw[[2]])),
                              dplyr::between(inv_ion_mobility, as.numeric(input$im_raw[[1]]), as.numeric(input$im_raw[[2]])))
        if(nrow(data) == 0){return(NULL)}
        if(nrow(data) > 1e4){message("Too many datapoints"); return(NULL)}
        create_marginal_plot(data)
    })
    output$rawPlot <- renderPlot(rawPlot())
    
    output$lengthHistogram <- renderPlot({
        pks <- chromPks()
        if(is.null(pks)){return(NULL)}
        ggplot(mutate(as.data.frame(pks), length = rtmax - rtmin)) +
            geom_histogram(aes(x=length)) + 
            ggtitle("Peak length distribution")
    })
    
    proxy <- dataTableProxy('peaktable')
    output$peaktable <- renderDataTable(chromPks())
    # Reset peak table selection
    observeEvent(input$removeSelection, {selectRows(proxy, NULL)})
    
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
        create_marginal_plot(data)
    })
    output$outputPlot <- renderPlot(plot())
    
    output$downloadPlot <- downloadHandler(
        filename = function(){paste("IM_plot",'.svg',sep='')},
        content = function(file){
            ggsave(file, plot = plot(), device = "svg", width = 10, height = 7)
        })
}

#for shorter startup time ?
if(is.na(getOption("TIMSTOF_LIB", default = NA)) &
   is.na(Sys.getenv("TIMSTOF_LIB", unset = NA))) {
  so_folder <- tempdir()
  so_file <- download_bruker_proprietary_code(so_folder, method = "wget")
} else {
  so_file <- getOption("TIMSTOF_LIB", default = NA)
  if(is.na(so_file)) so_file <- Sys.getenv("TIMSTOF_LIB", unset = NA)
}
setup_bruker_so(so_file)
# Run the application 
shinyApp(ui = ui, server = server)
