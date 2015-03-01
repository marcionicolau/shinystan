output$ui_upload_images <- renderUI({
  fileInput('pngs', 'Choose image to upload (.png)',
            multiple = TRUE,
            accept = c(
              'image/png'
            )
  )
})
