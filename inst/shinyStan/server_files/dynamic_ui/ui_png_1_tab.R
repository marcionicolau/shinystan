output$ui_png_1_tab <- renderUI({
  if (!is.null(input$png_1)) {
    tabPanel(title = input$png_1$name,
             renderImage("png_1_out")
    )
  }
})