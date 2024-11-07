# ENV
rm(list = ls())
gc()

# devtools::install_github("ropensci/plotly")

library(plotly)
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      plotlyOutput("plotA")
    ),
    column(
      width = 6,
      plotlyOutput("plotB"),
      actionButton('save', "Save")
    )
  ),
  
  verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("legendclick"),
  verbatimTextOutput("legend2click"),
  verbatimTextOutput("relayout"),
  verbatimTextOutput("relayout2")
)


plotFig <- function(src, eye){
  plot_ly(mtcars, x = ~wt, y = ~mpg, z = ~disp, color = ~factor(cyl), source = src) %>%
    add_markers() %>%
    event_register("plotly_legendclick") %>%
    event_register("plotly_legenddoubleclick")  %>%
    layout(
      scene = list(
        camera = list(
          eye = eye
        )
      ),
      margin = list(0,0,0,0)
    )
}





server <- function(input, output, session) {
  
  output$plotA <- renderPlotly({
    plot_ly(mtcars, x = ~wt, y = ~mpg, z = ~disp, color = ~factor(cyl), source = "A") %>%
      event_register("plotly_legendclick") %>%
      event_register("plotly_legenddoubleclick")  %>%
      layout(
        scene = list(
          camera = list(
            eye = list(1.25, 1.25, 1.25)
          )
        ),
        title = "Plot A"
      )
  })
  
  eye <- reactive({
    d <- event_data("plotly_relayout", source = "A")$scene.camera$eye
    if (is.null(d)){
      return(list(1.25, 1.25, 1.25))
    } else {
      return(d)
    }
  })
  
  plotB <- reactive({
    plotFig(src = "B", eye = eye())
  })
  
  output$plotB <- renderPlotly({
    plotB()
  })  

  output$hover <- renderPrint({
    d <- event_data("plotly_hover", source = "A")
    if (is.null(d)) "Hover events appear here" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click", source = "A")
    if (is.null(d)) "Click events appear here" else d
  })
  
  output$legendclick <- renderPrint({
    d <- event_data("plotly_legendclick", source = "A")$name
    if (is.null(d)) "Legend click" else d
  })
  
  output$legend2click <- renderPrint({
    d <- event_data("plotly_legenddoubleclick", source = "A")$name
    if (is.null(d)) "Legend double-click" else d
  })
  
  output$relayout <- renderPrint({
    d <- event_data("plotly_relayout", source = "B")$scene.camera$eye
    if (is.null(d)) "Camera eye info" else d
  })
  
  output$relayout2 <- renderPrint({
    eye()
  })
  
  observeEvent(input$save, {
    orca(plotB(), file = "plotB-from-eyeA.png")
  })
  
}

shinyApp(ui, server)
