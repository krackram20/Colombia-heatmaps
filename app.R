library(shiny)
library(dplyr)
library(colmaps)
library(ggplot2)
library(homicidios)


base_depto_id = departamentos@data

base_depto_id = base_depto_id %>% arrange(depto)

exm = base_depto_id

exm['valor'] = runif(n=33)


example = colmap(departamentos, subset(exm), var = "valor") +
  scale_fill_continuous(low = '#288dd1', high = '#073554', na.value = "wheat")


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Heatmap of Colombia"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel( 
          p('This app creates heatmaps of Colombia based on the package COLMAPS. The app reads csv files with two columns:
            the first column must be the list of Departments (33 total or it will not work)
            and the second column the values for the graph.', style = 'color:red'),
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            radioButtons("dec", "Decimal",
                         choices = c(Comma = ",",
                                     Dot = "."
                                   ),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(tabsetPanel(type = 'tabs',
            
            # Output: Data file ----
         tabPanel('Data',tableOutput("contents")),
           tabPanel('Heatmap', textInput('low','Low Color', value = "#d9ffd9" ),
                    textInput('high','High Color', value = "#069606" ),
                   br(), plotOutput('grafica'),textInput('title','Title', value = 'Este es un mapa de Colombia'),
                    downloadButton('save','save')),
         tabPanel('Example', br(), plotOutput('ex'),
                  textInput('low_ex','Low Color', value = '#288dd1' ),
                  textInput('high_ex','High Color', value = '#073554' ),
                  textInput('title_ex','Title', value = "Random values with normal distribution" ),
                  downloadButton('save_ex','save example'))
        
        )   
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    dat <- reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,
                               dec = input$dec)
                colnames(df) = c('depto','valor')
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        
        
        if(input$disp == "head") {
            return(df)
        }
        else {
            return(df)
        }
        
    })
    
 base_cruce = reactive(dat()  %>% arrange(depto))
 
 base_cruce1 = reactive(cbind(base_depto_id,base_cruce()[2]))
 

    
  output$contents = renderTable(base_cruce1())
  
  p = reactive(colmap(departamentos, subset(base_cruce1()), var = "valor") +
                   ggtitle(input$title)+
                   scale_fill_continuous(low = input$low, high = input$high, na.value = "wheat")+
                   theme(plot.title = element_text(hjust = 0.5)))
  
 example =  reactive(colmap(departamentos, subset(exm), var = "valor") + ggtitle(input$title_ex)+
    scale_fill_continuous(low = input$low_ex, high = input$high_ex, na.value = "wheat"))
  
  output$grafica = renderPlot(p())
  
  output$save <- downloadHandler(
      file = "save.png" , # variable with filename
      content = function(file) {
        ggsave(p(), filename = file)
         
      })
    
  output$ex = renderPlot(example())
  
  output$save_ex <- downloadHandler(
    file = "save_example.png" , # variable with filename
    content = function(file) {
      ggsave(example, filename = file)
      
    })
  
}

# Create Shiny app ----
shinyApp(ui, server)