library(yaml)
library(hgchmagic)
library(ggmagic)
library(shiny)
library(dsAppWidgets)
library(dsAppLayout)
library(colourpicker)
library(shinycustomloader)

layout <- read_yaml('data/yaml/layout.yaml')
params <- read_yaml('data/yaml/params.yaml')
ftypes <- read_yaml('data/yaml/ftypes.yaml')


random_name <- function(n = 10) {
  paste0(sample(c(LETTERS, letters, 0:9), n, TRUE), collapse = "")
}

styles <- '

.shiny-output-error {
  visibility: hidden;
  }
.shiny-output-error:before {
  visibility: hidden; }


.style_section {
    font-weight: 700;
    margin-bottom: 7px;
    letter-spacing: 1px;
}
.form-group label {
  font-size: 15px;
  font-weight: 500;
  letter-spacing: 1px;
}

.buttonDown {
  border: 1px solid;
  margin-left: 5px;
  display: inline-block;
}

'

ui <- 
  dsAppPage(
    dataControls = div(
      tableInputUI("dataIn",  
                   selected = "sampleData",
                   choices = list(
                     "Sample Data"="sampleData",
                     "Copy & Paste"="pasted",
                     "Load"="fileUpload",
                     "GoogleSheet" = "googleSheet",
                     "My library" = "dsLibrary"
                   )
      )
    ), 
    dataPreview = withLoader(uiOutput("data_preview"), type = 'image', loader = 'loading.svg'),
    vizControls = div(uiOutput("library"),
                      uiOutput("layout_test")),
    vizPreview = withLoader(uiOutput('vizEnd'), type = 'image', loader = 'loading.svg'),
    vizIcons = uiOutput("viz_icons"), 
    plotModal = uiOutput('downOptions'),#uiOutput("vizModal"),
    downloadModal =  "blablabla",
    styles = styles
  )

server <- function(input, output, session) {
  

  inputData <- callModule(tableInput, "dataIn",
                          sampleFile = list("Emission per capita C02"="data/sampleData/emisiones_c02.csv", "Femicide in Colombia 2017" = "data/sampleData/feminicidios_col.csv" ,"South America population"="data/sampleData/poblacion.csv"))
  
  
  output$data_preview <- renderUI({
    if(is.null(inputData()))return()
    list(
      dsAppWidgets::dsHot("dataTable", data = inputData())
    )
  })
  
  dt0 <- reactive({
    data <- input$dataTable$data
    dic <- input$dataTable$dic
    fringe(data, dic)
  })
  
  #Tipo de columna
  ctype <- reactive({
    ct <- input$dataTable$dic$ctype
    ct <- paste0(ct, collapse = '')
    gsub('Yea', 'Cat', ct)
  })
  
  
  viz_type <- reactive({
    f_value <- ctype()
    if (is.null(f_value)) return()
    ftypes_ind <- names(ftypes) %in% f_value
    if (sum(ftypes_ind) == 0) return()
    ftypes_inf <- ftypes[ftypes_ind]
    ftypes_inf[[f_value]]
  })
  
  
  # libreria
  output$library <- renderUI({
    viz_lib <- viz_type()
    opts_lib <- names(viz_lib$library_settings)
    radioButtons('id_library', 'Library', opts_lib)
  })
  
  
  viz_info <- reactive({
    lib <- input$id_library
    if (is.null(lib)) return()
    viz_lib <- viz_type()
    viz_lib$library_settings[[lib]]
  })
  
  # visualización
  output$viz_icons <- renderUI({
    graph <- viz_info()
    graph <- names(graph)
    buttonImage('viz_selection', graph, graph, file = "img/svg/", format = "svg")
  })
  
  
  viz_params <- reactive({
    viz_info <- viz_info()
    graph <- input$viz_selection
    if (is.null(graph)) return()
    viz_info <- viz_info[[graph]]
    viz_all <- unname(layout) %>% unlist()
    viz_all[viz_all %in% viz_info]
  })
  
  
  info_params <- reactiveValues()
  
  observe({
    params_viz <- viz_params()
    if (is.null(params_viz)) return()
    l <- params[names(params) %in% params_viz]
    map(params_viz, function(param_i) {
      if (param_i == 'order' | param_i == 'highlight_value') {
        l[[param_i]]$input_info$input_params$choices <- c('cat1', 'cat2', 'cat3')
        l[[param_i]]$input_info$input_params$options <- list(plugins= list('remove_button'))}
      if (param_i == 'order1') {
        l[[param_i]]$input_info$input_params$choices <- unique(input$dataTable$data[[1]])
        l[[param_i]]$input_info$input_params$options <- list(plugins= list('remove_button'))}
      if (param_i == 'order2') {
        l[[param_i]]$input_info$input_params$choices <- unique(input$dataTable$data[[2]])
        l[[param_i]]$input_info$input_params$options <- list(plugins= list('remove_button'))}
      if (param_i == 'sliceN') {
        l[[param_i]]$input_info$input_params$min <- 1
        l[[param_i]]$input_info$input_params$max <- length(unique(input$dataTable$data[[1]]))
        l[[param_i]]$input_info$input_params$value <- length(unique(input$dataTable$data[[1]]))
      }
      info_params[[param_i]] <- l[[param_i]]
    })
  })
  
  output$layout_test <- renderUI({
    
    params_viz <- viz_params()
    if (is.null(params_viz)) return()
    section_params <- names(layout)
    
    layout_filter <- map(section_params, function(section) {
      
      param_i <- layout[[section]]
      
      if (sum(params_viz %in% param_i) == 0) {
        return()
      } else {
        param_i <- param_i[param_i %in% params_viz]
      }
      param_i
    })
    names(layout_filter) <- section_params
    layout_filter <- layout_filter[lapply(layout_filter,length)>0]
    
    map(names(layout_filter), function(section) {
      div(
        div(id = section, class = 'style_section', section),
        map(layout_filter[[section]], function(param_i){
          p_inf <- info_params[[param_i]]
          if (is.null(p_inf)) return()
          if (p_inf$show) {
            do.call(p_inf$input_info$input, p_inf$input_info$input_params)
          } else {
            return()
          }
        })
      )
    })
  })
  
  
  observe({
    params_viz <- viz_params()
    if (is.null(params_viz)) return()
    map(params_viz, function(z) {
      l_param <- info_params[[z]]
      
      act <- l_param$update_param
      var_inp <- paste0(z, '_viz')
      if (!is.null(l_param$input_info$input))
        if (l_param$input_info$input == 'textInput') {
          isolate({
            if (!is.null(input[[var_inp]]))
              info_params[[z]]$input_info$input_params[[act]] <<- input[[var_inp]]
          })
        } else {
          if (!is.null(input[[var_inp]]))
            info_params[[z]]$input_info$input_params[[act]] <<- input[[var_inp]]
        }
      
      # ir cambiando el codigo según el tipo de hijo
      if('child_info' %in% names(info_params[[z]])) {
        child_i <- info_params[[z]]$child_info
        father_i <- info_params[[child_i$father]]
        if (is.null(father_i)) {
          if (child_i$other) {
            info_params[[z]]$show <- TRUE
          } else {
            info_params[[z]]$show <- FALSE
          }
        } else {
          if(father_i$input_info$input_params$value != child_i$filter) {
            info_params[[z]]$show <- FALSE
          } else {
            info_params[[z]]$show <- TRUE
          }
        }
      }
    })
  })
  
  opts <- reactive({
    params_viz <- viz_params()
    params_viz <- setdiff(params_viz, 'theme')
    l <- map(params_viz, function(i){
      p_i <- paste0(i, '_viz')
      inp <- input[[p_i]]
      if (p_i == 'marks_viz') {
        inp <- strsplit(inp, '&')[[1]]
      }
      inp
    })
    names(l) <- params_viz
    l
  })
  
  colores <- reactive({
    tema <- input$theme_viz
    print(tema)
    if (is.null(tema)) tema <- 'clasic'
    
      if (tema == 'clasic') paleta <- NULL
      if (tema == 'dark') paleta <-  c('#5d6ae9', '#b33f90', '#f75e64', '#ff9a2c', '#54419b', '#50c5b7', '#9cec5b', '#fe9583')
      if (tema == 'gray') paleta <- c('#6668e1', '#a052cb', '#ff9a2c', '#cae85d', '#1d4049', '#1e94b0', '#3be0c2', '#fed06e', '#e54b45')
      
    print(paleta)
    paleta
    
  })  

  observe({
    tema <- input$theme_viz
    if (is.null(tema)) tema <- 'clasic'
    if (tema == "dark") {
      updateColourInput(session, "background_viz", value = "#2d2d2d")}
    if (tema == 'gray') {
      updateColourInput(session, "background_viz", value = "#f2f2f2")}
  })
  
  options_list <- reactive({
    graph <- input$viz_selection
    if (is.null(graph)) return()
    opts_viz <- opts()
    opts_viz <- opts_viz[1:8]
    opts_viz$colors <- colores()
    opts_viz
  })
  
  

  
  #gráfica de ggmagic
  plot_ggmagic <- reactive({
    if (input$id_library != "ggplot") return()
    ctype <- ctype()#
    gtype <- input$viz_selection
    typeV <- paste0('gg_', gtype, '_', ctype)
    viz <- do.call(typeV, c(list(data = dt0(), opts = options_list())))
  })
  #   
  plot_hcmagic <- reactive({
    if (input$id_library != "highchart") return()
    ctype <- ctype()
    gtype <- input$viz_selection
    typeV <- paste0('hgch_', gtype, '_', ctype)
    data <- dt0()
    viz <- do.call(typeV, c(list(data = data, opts = options_list())))
  })
  
  # renderizando ggplot
  output$plotGg <- renderPlot({
    print(plot_ggmagic())
  })
  
  # renderizando hgcharts
  output$plotHc <- renderHighchart({
    print(plot_hcmagic())
  })
  
  output$vizEnd <- renderUI({
    idLib <- input$id_library
    if (is.null(idLib)) return()
    if (idLib == 'ggplot') {
      viz <- plotOutput('plotGg') }
    if (idLib == 'highchart') {
      viz <- highchartOutput('plotHc')
    }
    viz
  })
  
  # descargas
  output$downOptions <- renderUI({
    idLib <- input$id_library
    if (is.null(idLib)) return()
    if (idLib == 'ggplot') {
      tx <-  div(
        downloadButton('Gg_png', 'png', class = "buttonDown"),
        downloadButton('Gg_jpeg', 'jpeg', class = "buttonDown"),
        downloadButton('Gg_svg', 'svg', class = "buttonDown"),
        downloadButton('Gg_pdf', 'pdf', class = "buttonDown")
      ) }
    if (idLib == 'highchart') {
      tx <- div(
        downloadButton('Hc_html', 'html', class = "buttonDown"),
        downloadButton('Hc_png', 'png', class = "buttonDown"),
        downloadButton('Hc_jpeg', 'jpeg', class = "buttonDown"),
        downloadButton('Hc_pdf', 'pdf', class = "buttonDown")
      )}
    htmltools::tagList(
      tx
    )
    
  })
  
  
  tempDir <- reactive({
    last_ext <- input$last_btn
    #print(last_ext)
    if (is.null(last_ext)) return()
    dicTemp <- tempdir()
    n <- sample(1:10, 1)
    fileName <- random_name(n)
    x <-  list(
      'Dir' = dicTemp,
      'viz_id' = fileName,
      'ext' = paste0('.', gsub('.+_','' , last_ext))
    )
    x
  })
  
  observe({
    map(c('Gg_png', 'Gg_jpeg', 'Gg_svg', 'Gg_pdf'),
        function(z) {output[[z]] <- downloadHandler(
          filename = function() {
            paste0(tempDir()$Dir, tempDir()$viz_id, tempDir()$ext )},
          content = function(file) {
            ggmagic::save_ggmagic(plot_ggmagic(), file, tempDir()$ext)
          })
        })
  })
  
  
  observe({
    map(c('Hc_html','Hc_png', 'Hc_jpeg', 'Hc_svg', 'Hc_pdf'),
        function(z) {output[[z]] <- downloadHandler(
          filename = function() {
            paste0(tempDir()$Dir, tempDir()$viz_id, tempDir()$ext )},
          content = function(file) {
            #print(file)
            hgchmagic::save_hgchmagic(plot_hcmagic(), file, tempDir()$ext)
          })
        })
  })
  
  
}


shinyApp(ui, server)
