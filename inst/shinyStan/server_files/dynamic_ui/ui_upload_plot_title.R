output$ui_upload_plot_title <- renderUI({
  title <- input$upload_plot_title_txt
  if (title == "Enter a title for your image") return() else return(h4(title))
})