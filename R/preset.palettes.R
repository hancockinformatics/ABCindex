#' While these could (and maybe should) be stored as an ".Rds" object, this
#' makes it much easier to change the colours, which may happen over the course
#' of ShinyABCi's development.

preset.palettes <- list(
  "OP"  = c("#FF8800", "#FFA000", "#FFB638", "#F2C36F", "#E3E3E3", "#F1BEF1", "#D17DD1", "#D17DD1"),
  "YP"  = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3", "#F1BEF1", "#D17DD1", "#D17DD1"),
  "YB"  = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF"),
  "SUN" = c("#FF6D00", "#FFAE00", "#FAE040", "#FFE98F", "#E3E3E3", "#F1BEF1", "#D17DD1", "#D17DD1"),
  "PAN" = c("#FF005D", "#FFD500", "#FAE040", "#FFE98F", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF"),
  "BOB" = c("#D40000", "#FF5000", "#FF7E42", "#FAE040", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF"),
  "RB"  = c("#FF0000", "#FF5000", "#FF7E42", "#F7A279", "#E3E3E3", "#A9D5FD", "#6FC0FF", "#6FC0FF")
)

preset.palettes.split <- list(
  "YP" = list(
    up   = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3"),
    down = c("#B935B9", "#D46AD4", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "YB" = list(
    up   = c("#FFC107", "#FFDE07", "#FAE040", "#FFE98F", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "OP" = list(
    up   = c("#FF8800", "#FFA000", "#FFB638", "#F2C36F", "#E3E3E3"),
    down = c("#B935B9", "#D46AD4", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "RB" = list(
    up   = c("#FF0000", "#FF5000", "#FF7E42", "#F7A279", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "PAN" = list(
    up   = c("#FF005D", "#FFD500", "#FAE040", "#FFE98F", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "SUN" = list(
    up   = c("#FF6D00", "#FFAE00", "#FAE040", "#FFF0B3", "#E3E3E3"),
    down = c("#0062AC", "#0077D0", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  ),
  "BOB" = list(
    up   = c("#D40000", "#FF5000", "#FF7E42", "#FAE040", "#E3E3E3"),
    down = c("#B935B9", "#D46AD4", "#6FC0FF", "#A9D5FD", "#E3E3E3")
  )
)
