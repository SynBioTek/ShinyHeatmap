# install.packages("tidyverse")

## app.R ##
library(shiny)
library(shinydashboard)
library(plotly) # for plotlyOutput
library(readxl) # to read excel
library(heatmaply)
library(gplots) # for greenred colors

ui <- dashboardPage(
      dashboardHeader(title = "Welcome to Heatmap!"), 
      dashboardSidebar(sidebarMenu(
      menuItem("Heatmap", tabName = "file", icon = icon("file")),
      menuItem("Heatmap_Instructions", tabName = "intro"))), # end of sidebar and sidebarMenu
    
      dashboardBody(tabItems(tabItem(tabName = "file",
      fluidRow(box(title = "", solidHeader = T,status = "primary",width = 3,
      fileInput("mydata", "File Input", multiple = F, placeholder = "Input excel file"),
      h5('Heatmap Parameters'),column(5,checkboxInput('Layout','Layout')),
                    br(),hr(),column(5,checkboxInput('Clustering','Clustering')),
                    br(),hr(),column(5,checkboxInput('HeatColors','Colors')),
                    br(),hr(),conditionalPanel('input.Layout==true',
                    br(), hr(),
      h5("Heatmap Layout"),sliderInput("mapHeight", "Height", min = 400, max = 1000,value = 500),
                           sliderInput("width", "Right Adjust", min = 500, max =900, value = 800),
                           sliderInput("R","Left Adjust", min = 90, max =600, value = 450),
                           sliderInput("L", "Row Adjust", min = 0, max =650, value = 80),
                           numericInput("colAngle", "X label Angle", min = -360, max=360, value=-45)),
                           
      conditionalPanel('input.Clustering==true',
                           br(), hr(),
                           selectInput("Dendo", "Dendogram", choices = c("col", "row", "both", "none"),selected ="both"),
                           selectInput("seriation", "Seriation(ordering)", choices= c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'mean')),
                          
      conditionalPanel('input.HeatColors==true',
                            selectInput("lowColor", "Low Value:", c("green", "white", "black","blue", "purple", "red", "orange", "yellow" )),
                            selectInput("highColor", "High Value:", c("red", "white", "black", "orange", "yellow", "green", "blue", "purple")))
                    # unable to get this working :( 
                    # ,br(),
                    # h5("Download Image as pdf"),
                    # downloadButton("download", label = "Download")
                    
        ),#end fileInput box, 
       box(title = "Your Heatmap!",solidHeader = T,status = "primary",width = 9,height = 1000,
       mainPanel(plotlyOutput("heatmap"))
        ) # end heatmap box
        )# end fluid row
        ),# end file tabItem
              
      tabItem(tabName = "intro",fluidRow(box(
                  h2("Welcome to Shiny Heatmap!"),
                  p("This is a basic app that can be utilized to visualize a representation of data in the form of 
                      a map or diagram in which data values are represented as colors.
                      Additionally, the data can be clustered using any of the four provided clustering options:"), 
                  p("1)	Mean"),
                  p("2)	OLO- Optimal Leaf Ordering"),
                  p("3)	GW - Gruvaeus and Wainer"),
                  p("4)	None"),
                  p("What separates Shiny Heatmap from other heatmap applications is that allows the user to control 
                    layout parameters such as the height and width thus being able to accommodate large data sets.
                    Try it out by uploading an .xlsl (excel) file."),
                  p("For technical information regarding the above algorithms see:",a(href = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4162509/','Sakai, Ryo et al.')),
                  p("The code to this app can be found at:",a(href="https://github.com/SynBioTek","GitHub"))
                    ),
                box(title = NULL,
                    h4("Figure 1. Example input"),
                    tableOutput("sampleData"),
                    tags$style("#sampleData table {border: 1px solid black; align: left}",
                               "#sampleData th {border: 1px solid black;}",
                               "#sampleData td {border: 1px solid black;}"),
                    hr(),
                    h4("Figure 2. Example output"),
                    img(src='heatmapExample.png', align = "left", width="400px", height="550px")
                    )
        )# end of fluidRow
        ) # end of intro tabItem
        )# end of tabItems
        ) # end of body
        ) # end of dashboardPage

server <- function(input, output) { 
  
  # render instructions
  output$sampleData <- renderTable({
    metadata <- read.delim(paste(getwd(),"/www/dataInput1.txt",sep=""), header=T)
    metadata}, rownames=F, align="l")
 
  # load data
  data.in = eventReactive(input$mydata,{
    infile<- input$mydata
    if (is.null(infile))
      return(NULL)
    else{
      data=read_excel(infile$datapath)
      data = data.frame(read_excel(infile$datapath), row.names = colnames(data)[1])
    }
     return(data) 
  })
 
  # build heatmap
  shinyHeatmap = function(){
   
      withProgress(message = 'Calculating', min = 0, max = 10, value = 9,{ 
       x=data.in()
      p<- heatmaply(x, plot_method = "plotly",
                     #layout
                     margins =c(l=input$L,r=input$R,t=30,b=90),
                     srtCol=input$colAngle,
                     # color schemes
                     col=colorpanel(15, low = input$lowColor, high = input$highColor),
                     #col=greenred(75),
                     #clustering
                     Rowv=T, #input$RowClust, 
                     Colv=T, #input$ColClust,
                    
                     dendrogram=input$Dendo,
                     seriate = input$seriation)%>% 
         layout(height = input$mapHeight, width=input$width)
       
       p$elementId <- NULL 
       p
      }) # end of withProgress
     
   }
  
  # call build heatmap function
  observeEvent(input$mydata, {
    output$heatmap <- renderPlotly({
          shinyHeatmap()})
  }) 
  

  }

shinyApp(ui, server)

