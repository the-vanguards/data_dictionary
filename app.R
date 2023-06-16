library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(shiny)
library(shinydashboard)
# library(googlesheets4)
# library(googledrive)

# Load data ---------------------------------------------------------------
# drive_auth(path = "googletest1-fe53c09ae2ba.json")
# gs4_auth(path = "googletest1-fe53c09ae2ba.json")

df <- read_csv("https://raw.githubusercontent.com/kenkoonwong/am/main/data_dictionary_en.csv")
df_ate <- read_csv("https://raw.githubusercontent.com/kenkoonwong/am/main/df_ate_48h_40mp_sfr_cohort_facet.csv")

cat <- df_ate |>
  pull(category) |>
  unique()

colorscale <- "Blackbody,Bluered,Blues,Cividis,Earth,Electric,Greens,Greys,Hot,Jet,Picnic,Portland,Rainbow,RdBu,Reds,Viridis,YlGnBu,YlOrRd" |> 
  str_split(pattern = ",") |>
  unlist()


# header ------------------------------------------------------------------



## header
header <- dashboardHeader(title = "The Council")


# sidebar -----------------------------------------------------------------


## sidebar
sidebar <- dashboardSidebar(
    tags$style(HTML(".sidebar-menu li { font-size: 20px; }")),
      sidebarMenu(
        menuItem(text = "Data Dictionary", tabName = "data_dict"),
        menuItem(text = "plot3d sepsis", tabName = "plot3d_sepsis"),
        menuItem(text = "plot3d custom scatter", tabName = "plot3d_resp"),
        menuItem(text = "plot3d custom mesh3d", tabName = "plot3d_cus"),
        selectInput(inputId = "plotlycustom", label = "Category", choices = cat, multiple = F),
        numericInput(inputId = "opacity", label = "Opacity/Alpha", value = 1),
        selectInput(inputId = "colorscale", label = "ColorScale", choices = colorscale, selected = "Viridis")
    ))

# body --------------------------------------------------------------------


## body
body <- dashboardBody(
    tabItems(
      tabItem("data_dict",
    fluidRow(DT::dataTableOutput("data_dict_en"))),
      tabItem("plot3d_sepsis",
            fluidRow(plotlyOutput("ateplot", width = "100%", height = "900px"))),
    tabItem("plot3d_resp",
           fluidRow(plotlyOutput("ateplot2", width = "100%", height = "900px"))),
    tabItem("plot3d_cus",
            fluidRow(plotlyOutput("ateplot3", width = "100%", height = "900px")))
))




# ui ----------------------------------------------------------------------



## ui
ui <- dashboardPage(header, sidebar, body)


# Define server logic required to draw a histogram
server <- function(input, output) {
    cat <- reactive({
      x <- input$plotlycustom
        return(x)
    })
    
    alpha <- reactive({
      x <- input$opacity
      return(x)
    })
    
    colorscale <- reactive({
      x <- input$colorscale
      return(x)
    })
  
    output$data_dict_en <- renderDataTable({
        DT::datatable(df, 
                      filter = list(position = 'top', clear = FALSE),
                      extensions = "FixedColumns", rownames = FALSE, options = list(
            pageLength = 20,
            scrollX = TRUE, 
            scrollY = "1000px",
            # dom = "t",
            search = list(regex = TRUE),
            fixedColumns = list(leftColumns = 3, rightColumns = 3)))
    })
    
    output$ateplot <- renderPlotly({
      fig <- plot_ly(df_ate |> filter(category == "is_sepsis"), x=~hr, z=~ate, y=~threshold, color=~hr, type = "mesh3d",intensity =~ate, colorscale="Viridis", colorbar = list(title=list(text="ATE")))
      
      fig
    })
    output$ateplot2 <- renderPlotly({
    
      
      fig <- plot_ly(df_ate |> filter(category == cat()), x=~hr, z=~ate, y=~threshold, color=~ate, alpha=alpha(), type = "scatter3d",colorbar = list(title=list(text="ATE")), marker=list(colorscale = colorscale()))
      
      fig
    })
    output$ateplot3 <- renderPlotly({
      fig <- plot_ly(df_ate |> filter(category == cat()), x=~hr, z=~ate, y=~threshold, color=~hr, type = "mesh3d",intensity =~ate, colorscale=colorscale(), colorbar = list(title=list(text="ATE")), opacity = alpha())
      
      fig
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
