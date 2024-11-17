install.packages("pak")

pak::pkg_install(
  c(
    c("shiny", "bslib", "tidyverse", "data.table", "fst", "arrow", "profvis"), # For app
    c("rsconnect", "cpp11", "progress") # For deployment
  )
)
