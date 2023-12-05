wrap_selector <- function(label, label_title, selector) {
  div(
    class = "form-group row",
    style = "margin-bottom: 0.25rem;",
    tags$label(
      class = "col-sm-4 col-form-label",
      label,
      title = label_title
    ),
    div(
      class = "col-sm-8",
      selector
    )
  )
}
