wrap_selector <- function(label, label_title, selector) {
  div(
    class = "form-group row",
    style = "margin-bottom: 0.25rem;",
    tags$label(
      class = "col-sm-5 col-form-label",
      div(
        label,
        icon(
          "circle-question",
          title = label_title
        )
      )
    ),
    div(
      class = "col-sm-7",
      selector
    )
  )
}
