update_shinystan_object <- function(sso) {
  new_sso <- new("shinystan")
  slot_names <- slotNames("shinystan")
  slot_names <- slot_names[-which(slot_names == "param_labels")]
  for (s in seq_along(slot_names)) {
    slot_name <- slot_names[s]
    slot(new_sso, slot_name) <- slot(sso, slot_name)
  }
  new_sso
}
