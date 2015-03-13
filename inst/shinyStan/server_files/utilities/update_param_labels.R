# requires new slot @param_labels, set to empty character vector by default
update_param_labels <- reactive({
  to_update <- param_names %in% input$param_labels_params
  replace <- .hasSlot(shinystan_demo_object, "param_labels")
  new_labels <- if (replace) shinystan_object@param_labels else param_names
  new_labels[to_update] <- input$param_labels_labels
  shinystan_object@param_labels <<- new_labels
})






