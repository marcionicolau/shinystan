# requires new slot @param_labels, set to empty character vector by default
update_param_labels <- reactive({
  to_update <- which(param_names %in% input$param_labels_params)
  replace <- length(object@param_labels) == length(param_names)
  new_labels <- if (replace) object@param_labels else param_names
  new_labels[to_update] <- input$param_labels_labels
  new_labels
})







