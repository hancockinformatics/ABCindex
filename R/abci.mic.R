#' INTERNAL Calculate MIC values
#'
#' @param data Data frame containing the concentrations of two drugs, and the
#'   assay output/measurement
#' @param x.drug Character; Column name of concentrations of the first drug
#' @param y.drug Character; Column name of concentrations of the second drug
#' @param col.data Character; Column name which contains the measured value
#' @param threshold Numeric; cutoff for determining MIC
#'
#' @return A data frame with the following columns:
#'   \item{XMIC}{MIC value for `x.drug`}
#'   \item{YMIC}{MIC value for `y.drug`}
#'
#' @export
#'
#' @description Uses the provided data frame to determine the Minimum Inhibitory
#'   Concentration (MIC), based on a desired threshold.
#'
abci.mic <- function(
    data,
    x.drug,
    y.drug,
    col.data,
    threshold = 0.5
) {

  if (any(c("spec_tbl_df", "tbl_df", "tbl") %in% class(data))) {
    data <- as.data.frame(data)
  }

  stopifnot(
    "Column given by 'x.drug' must be a factor" = is.factor(data[[x.drug]])
  )
  stopifnot(
    "Column given by 'y.drug' must be a factor" = is.factor(data[[y.drug]])
  )

  data.clean <- data[data[, col.data] < threshold, ]
  x.data.clean <- data.clean[data.clean[, y.drug] == 0, ]
  y.data.clean <- data.clean[data.clean[, x.drug] == 0, ]

  x.mic <-
    if (nrow(x.data.clean) == 0) {
      max(as.numeric(as.character(levels(droplevels(data.clean[[x.drug]])))))
    } else {
      min(as.numeric(as.character(x.data.clean[[x.drug]])))
    }

  y.mic <-
    if (nrow(y.data.clean) == 0) {
      max(as.numeric(as.character(levels(droplevels(data.clean[[y.drug]])))))
    } else {
      min(as.numeric(as.character(y.data.clean[[y.drug]])))
    }

  mic.table <- data.frame(XMIC = x.mic, YMIC = y.mic)

  return(mic.table)
}
