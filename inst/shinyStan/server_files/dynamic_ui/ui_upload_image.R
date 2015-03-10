output$ui_upload_plot_png_1 <- renderUI({
  fileInput('my_plots_png_1', 
            label = 'Choose a .png file',
            accept = c(
              'image/png'
            )
  )
})

output$ui_upload_plot_gg_1 <- renderUI({
  choices <- objects(envir = .GlobalEnv)
  selectizeInput("my_plots_gg_1", 
                 label = "ggplot2 ojbect", 
                 choices = c("", choices), 
                 options = list(placeholder = "Select an object"))
})

# output$ui_upload_plot_png_2 <- renderUI({
#   fileInput('my_plots_png_2', 
#             label = 'Choose image to upload (.png)',
#             accept = c(
#               'image/png'
#             )
#   )
# })
# 
# output$ui_upload_plot_rdata_2 <- renderUI({
#   fileInput('my_plots_rdata_2', 
#             label = 'Choose a file containing a ggplot2 object(.RData)'
#   )
# })
# 
