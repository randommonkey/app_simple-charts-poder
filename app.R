library(shinypanels)
library(dsmodules)
library(parmesan)
library(hotr)
library(shinyinvoer)
library(colourpicker)
library(hgchmagic)
library(ggmagic)

config_path <- "parmesan/"
input_ids <- parmesan_input_ids(config_path = config_path)
input_ids_values <- lapply(input_ids, function(i){
  NA
})
names(input_ids_values) <- input_ids
all_sections <- names(yaml::read_yaml('parmesan/layout.yaml'))
default_themes <- yaml::read_yaml('data/aux/themes.yaml')

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
    body = list(
      uiOutput("select_var"),
      uiOutput("dataset") 
    )
  ),
  panel(
    title = "Edit viz",
    color = "chardonnay",
    #collapsed = TRUE,
    width = 350,
    body = div(
      class = 'controls'
    )
  ),
  panel(
    title = "Viz",
    color = "chardonnay",
    can_collapse = FALSE,
    body = div(
      uiOutput("vizView"),
      shinypanels::modal(id = 'test', title = 'Choose any format',
                         conditionalPanel(
                           condition = "input.library == 'highcharter'",
                           downloadImagesUI("down_hgchmagic", "Download", c("html", "png", "jpeg", "pdf"))
                         ),
                         conditionalPanel(
                           condition = "input.library == 'ggplot'",
                           downloadImagesUI("down_ggmagic", "Download", c( "svg", "png", "jpeg", "pdf"))
                         )
      ),
      div(style="text-align:right;", shinypanels::modalButton(label = "Download", modal_id = "test"))
    ),
    footer = uiOutput("viz_icons")
  )
)



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
                            "sampleData" = infomessage(type = "info", p("You can start with a test data, choose a data sample and ")),
                            "googleSheets" = infomessage(type = "info", p("You can connect one spreed sheet "))
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
      hotr("data_input", data = inputData(), order = order_var, options = list(height = 570))
    )
  })
  
  
  firstCat <- reactive({
    unique(input$data_input$data$a)
  }, env = react_env)
  
  secondCat <- reactive({
    unique(input$data_input$data$b)
  }, env = react_env)
  
  tables_has_na <- reactive({
    sum(is.na(input$data_input$data$a)) >= 1
  }, env = react_env)
  
  
  numCat <- reactive({
   n <- length(unique(input$data_input$data$a))
   n
  }, env = react_env)
  
  
  ftype <- reactive({
    if (is.null(input$data_input$dic$ctype)) return()
    ftype_riddle <- input$data_input$dic$ctype
    ftype_riddle <- gsub("Yea", "Cat", paste(ftype_riddle, collapse = ""))
    ftype_riddle 
  }, env = react_env)
  
  legend_has_na <- reactive({
  # if (ncol(input$data_input$data) < 2) l_na <- FALSE
  #  l_na <- sum(is.na(input$data_input$data$b)) >= 1 & ftype() %in%  c('CatCat', 'CatCatNum')
  #  l_na
    TRUE
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
                     HTML('<div class = "style_section">Choose a visualization type</div>'), 
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
  
  
map(all_sections, function(section){
 output[[gsub('[[:space:]]', '_',section)]] <- renderUI({
   parmesan_render_ui(section = section, config_path = config_path, input = input, env = react_env)
 })
})

map(all_sections, function(section){
  insertUI(".controls", ui = uiOutput(gsub('[[:space:]]', '_',section)))
})
  
  
  opts_viz <- reactive({
    params <- vals$inputs
    params <- params[setdiff(names(params), c('library'))]
    params$marks <- strsplit(input$marks, '&')[[1]]
    theme_select <- input$theme
    if (is.null(theme_select)) theme_select <- 'datasketch'
    params$theme <- default_themes[[theme_select]]
    params
  })
  
  

  observeEvent(input$theme, {
    tema <- input$theme
    if (is.null(tema)) tema <- 'datasketch'
    if (tema == "datasketch") {
      updateColourInput(session, "background", value = "#ffffff")}
    if (tema == "dark_roboto") {
      updateColourInput(session, "background", value = "#2d2d2d")}
    if (tema == 'gray_monserrat') {
      updateColourInput(session, "background", value = "#f2f2f2")}
  })
  
  
  observeEvent(input$viz_selection, {
    updateSelectizeInput(session, "theme", selected = input$theme)
    updateColourInput(session, "background", value = input$background)
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
    suppressWarnings(
    vizGg()
    )
  })
  
  output$vizView <- renderUI({
    if (is.null(input$library)) return()
    
    if (input$library == "highcharter") {
      highchartOutput("vizViewHg", height = 530)
    } else {
      plotOutput("vizViewGg", height = 530)
    }
  })
  
  
  callModule(downloadImages, "down_hgchmagic", graph = vizHg(), lib = "highcharter", formats = c("html", "png", "jpeg", "pdf"))
  callModule(downloadImages, "down_ggmagic", graph = vizGg(), lib = "ggplot", formats = c("svg", "png", "jpeg", "pdf"))
}


shinyApp(ui, server)