library(shinypanels)
library(dsmodules)
library(parmesan)
library(hotr)
library(shinyinvoer)
library(hgchmagic)
library(ggmagic)


ui <- panelsPage(
  panel(
    title = "Upload Data", 
    width = 300,
    body = div(
      tableInputUI("initial_data",
                   choices = list(
                     "Sample Data"="sampleData",
                     "Copy & Paste"="pasted",
                     "CSV/XLS Upload"="fileUpload",
                     "Google Sheets" = "googleSheets"
                   ),
                   selected = "sampleData")
    )
  ),
  panel(
    title = "Dataset",
    width = 400,
    collapsed = TRUE,
    body = div(
     #verbatimTextOutput("debug"),
     uiOutput("select_var"),
     uiOutput("dataset") 
    )
  ),
  panel(
    title = "Edit viz",
    width = 350,
    body = div(
      uiOutput("controls")
    )
  ),
  panel(
    title = "Viz",
    body = div(
      uiOutput("vizView"),
      shinypanels::modal(id = 'test', title = 'Download plot',
            conditionalPanel(
              condition = "input.library_viz == 'highcharter'",
              downloadImagesUI("down_hgchmagic", "Descarga", c("html", "png", "jpeg", "pdf"))
            ),
            conditionalPanel(
              condition = "input.library_viz == 'ggplot'",
              downloadImagesUI("down_ggmagic", "Descarga", c( "jpeg", "pdf"))
            )
      ),
      shinypanels::modalButton(label = "Download plot", modal_id = "test")
    ),
    footer = uiOutput("viz_icons")
  )
)

config_path <- "parmesan/"
input_ids <- parmesan_input_ids(config_path = config_path)
input_ids_values <- lapply(input_ids, function(i){
  NA
})
names(input_ids_values) <- input_ids


server <-  function(input, output, session) {
  
  vals <- reactiveValues()
  vals$inputs <- input_ids_values
  react_env <- new.env()
  
  observe({
    lapply(input_ids, function(i){
      vals$inputs[[i]] <- input[[i]]
      vals
    })
  })
  
  output$viz_icons <- renderUI({
    graph <- c("bar", "line", "pie", "treemap")
  
    buttonImageInput('viz_selection', graph, graph, file = "img/svg/", format = "svg")
  })
  

  inputData <- callModule(tableInput, "initial_data",
                          sampleFile = list("Income"="data/sampleData/emisiones_c02.csv",
                                            "Population"="data/sampleData/poblacion.csv"),
                          infoList = list(
                            "pasted" = ("Esto es información sobre el paste"),
                            "fileUpload" = HTML("Esto es información sobre el fileUpload"),
                            "sampleData" = HTML("Info sample Data"),
                            "googleSheets" = HTML("IFO GGO")
                          ))
  
  
  output$select_var <- renderUI({
    names_data <- names(inputData())
    selectizeInput("var_order", "Choose order", names_data,  multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop')))
  })


  output$dataset <- renderUI({
    if(is.null(inputData()))return()
    order_var <- input$var_order
    suppressWarnings(
      hotr("data_input", data = inputData(), order = order_var, options = list(height = 470))
    )
  })

  # data_viz <- reactive({
  #   data <- input$data_input$data
  #   dic <- input$data_input$dic
  #   fringe(data, dic)
  # })

  output$controls <- renderUI({
    
    a <- parmesan_render_ui(config_path = config_path, input = input, env = react_env)
    a
    print(a)
  })


  vizHg <- reactive({
    #if (input$library_viz != "highchart") return()
    ctype <- "CatCatNum"#ctype()
    gtype <- input$viz_selection
    typeV <- paste0('hgch_', gtype, '_', ctype)
    data <- sampleData("Cat-Cat-Num")
    #print(typeV)
    #print(input$data_input$dic)
    viz <- do.call(typeV, c(list(data = data, opts = list(title = input$title_viz))))
    viz
    #hgch_bar_CatCatNum(sampleData("Cat-Cat-Num"))
  })

  output$vizViewHg <- renderHighchart({
    vizHg()
  })

  vizGg <- reactive({
    #if (input$library_viz != "highchart") return()
    ctype <- "CatCatNum"#ctype()
    gtype <- input$viz_selection
    typeV <- paste0('gg_', gtype, '_', ctype)
    data <- sampleData("Cat-Cat-Num")
    #print(typeV)
    #print(input$data_input$dic)
    viz <- do.call(typeV, c(list(data = data, opts = list(title = input$title_viz))))
    viz
  })

  output$vizViewGg <- renderPlot({
    vizGg()
  })

  output$vizView <- renderUI({
    if (is.null(input$library_viz)) return()
    
    if (input$library_viz == "highcharter") {
      highchartOutput("vizViewHg")
    } else {
      plotOutput("vizViewGg")
    }
  })



  callModule(downloadImages, "down_hgchmagic", graph = vizHg(), lib = "highcharter", formats = c("html", "png", "jpeg", "pdf"))
  callModule(downloadImages, "down_ggmagic", graph = vizGg(), lib = "ggplot", formats = c( "jpeg", "pdf"))
}


shinyApp(ui, server)