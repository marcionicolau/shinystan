output$ui_upload_plot_out_1 <- renderUI({
  validate(need(!is.null(input$my_plots_png_1) | !is.null(input$my_plots_gg_1), message = "Waiting for plot"))
  if (!is.null(input$my_plots_png_1)) {
    return(imageOutput("png_1_out"))
  } else {
    if (!is.null(input$my_plots_gg_1)) {
      return(plotOutput("gg_1_out"))
    } else {
      return()
    }
  }
})