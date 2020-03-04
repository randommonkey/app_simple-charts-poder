library(shinypanels)
library(dsmodules)
library(parmesan)
library(hotr)
library(shinyinvoer)
library(colourpicker)
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
      verbatimTextOutput('blabla'),
      shinypanels::modal(id = 'test', title = 'Download plot',
                         conditionalPanel(
                           condition = "input.library == 'highcharter'",
                           downloadImagesUI("down_hgchmagic", "Descarga", c("html", "png", "jpeg", "pdf"))
                         ),
                         conditionalPanel(
                           condition = "input.library == 'ggplot'",
                           downloadImagesUI("down_ggmagic", "Descarga", c( "svg", "png", "jpeg", "pdf"))
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
    selectizeInput("var_order", 
                   "Choose order",
                   names_data,  
                   multiple = TRUE,
                   options = list(plugins= list('remove_button', 'drag_drop'))
    )
  })
  
  
  output$dataset <- renderUI({
    if(is.null(inputData()))return()
    order_var <- input$var_order
    suppressWarnings(
      hotr("data_input", data = inputData(), order = order_var, options = list(height = 470), enableCTypes = TRUE)
    )
  })
  
  
  firstCat <- reactive({
    unique(input$data_input$data$a)
  }, env = react_env)
  
  secondCat <- reactive({
    unique(input$data_input$data$b)
  }, env = react_env)
  
  
  
  ftype <- reactive({
    if (is.null(input$data_input$dic$ctype)) return()
    ftype_riddle <- input$data_input$dic$ctype
    ftype_riddle <- gsub("Yea", "Cat", paste(ftype_riddle, collapse = ""))
    ftype_riddle 
  }, env = react_env)
  
  
  
  ftype_image_recommendation <- reactive({
    ftype_end <- ftype()
    if (is.null(ftype_end)) return()
    all_ftypes <- yaml::read_yaml("data/aux/ftypes.yaml")
    all_ftypes[[ftype_end]]
  }, env = react_env)
  
  
  actual_but <- reactiveValues(active = 'bar')
  
  observe({
    viz_rec <- ftype_image_recommendation()
    if (is.null(viz_rec)) return()
    if (is.null(input$viz_selection)) return()
    if (!( input$viz_selection %in% viz_rec)) {
      actual_but$active <- viz_rec[1]  
    } else {
      actual_but$active <- input$viz_selection
    }
  })
  
  output$viz_icons <- renderUI({
    buttonImageInput('viz_selection',
                     'Viz type', 
                     images = ftype_image_recommendation(),
                     path = 'img/svg/',
                     format = 'svg',
                     active = actual_but$active)
  })
  
  
  data_viz <- reactive({
    data <- input$data_input$data
    dic <- input$data_input$dic
    fringe(data, dic)
  })
  
  output$controls <- renderUI({
    sections <- yaml::read_yaml('parmesan/layout.yaml')
    sections <- setdiff(names(sections), 'Viz types')
    map(sections, function(section) {
      parmesan_render_ui(section = section, config_path = config_path, input = input, env = react_env)
    })
  })
  
  
  opts_viz <- reactive({
    params <- vals$inputs[-1]
    params$marks <- strsplit(input$marks, '&')[[1]]
    params[1:10]
  })
  
  vizHg <- reactive({
    
    ctype <- ftype()
    gtype <- actual_but$active
    typeV <- paste0('hgch_', gtype, '_', ctype)
    data <- data_viz()
    
    viz <- do.call(typeV, c(list(data = data, opts = opts_viz())))
    viz
    
  })
  
  output$vizViewHg <- renderHighchart({
    suppressWarnings(
      vizHg()
    )
  })
  
  vizGg <- reactive({
    #if (input$library_viz != "highchart") return()
    ctype <- ftype()
    gtype <- actual_but$active
    typeV <- paste0('gg_', gtype, '_', ctype)
    data <- data_viz()
    viz <- suppressMessages(
      do.call(typeV, c(list(data = data, opts = opts_viz())))
    )
    viz
  })
  
  output$vizViewGg <- renderPlot({
    vizGg()
  })
  
  output$vizView <- renderUI({
    if (is.null(input$library)) return()
    
    if (input$library == "highcharter") {
      highchartOutput("vizViewHg")
    } else {
      plotOutput("vizViewGg")
    }
  })
  
  
  callModule(downloadImages, "down_hgchmagic", graph = vizHg(), lib = "highcharter", formats = c("html", "png", "jpeg", "pdf"))
  callModule(downloadImages, "down_ggmagic", graph = vizGg(), lib = "ggplot", formats = c("svg", "png", "jpeg", "pdf"))
}


shinyApp(ui, server)