output$ui_my_plots_tabset <- renderUI({
  pngs <- input$pngs
  
  ## Dynamically render tabset based on the user selected samples               
  do.call(tabsetPanel,
          ## Create a set of tabPanel functions dependent on input$samples
          lapply(1:nrow(pngs),function(i){
            ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
            ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
            call("tabPanel",title = pngs[[i, 'name']],call('imageOutput',paste0("png_",i)))
          })
  )
})
