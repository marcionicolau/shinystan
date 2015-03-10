png_1 <- reactive({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  inFile <- input$my_plots_png_1
  
  if (is.null(inFile))
    return(NULL)
  
  #     width  <- session$clientData$output_plot2_width
  #     height <- session$clientData$output_plot2_height
#   width <- input$upload_plot_1_width
#   height <- input$upload_plot_1_height
  
  list(src = inFile$datapath,
       contentType = 'image/png',
       width = 600, height = 400
  )
#            width = width,
#            height = height)
})

gg_1 <- reactive({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  gg <- input$my_plots_gg_1
  
  if (gg == "") return(NULL)
  else get(gg, envir = .GlobalEnv)
})