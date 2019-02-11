



# First remove all but the CGT_parent
vapply(dir(path = ".", pattern = "\\.tex$"), function(x) {
  !grepl("parent", x) && file.remove(x)
}, FALSE)

cat(as.character(Sys.time()), "\t", "hutils\n")
install.packages("hutils", repos = "https://cran.rstudio.com", quiet = TRUE)
cat(as.character(Sys.time()), "\t", "grattan\n")
install.packages("grattan", repos = "https://cran.rstudio.com", quiet = TRUE)
cat(as.character(Sys.time()), "\t", "TeXCheckR\n")
install.packages("TeXCheckR", repos = "https://cran.rstudio.com", quiet = TRUE)

cat(as.character(Sys.time()), "\t", "hutilscpp\tgrattanCharts\n")
devtools::install_github(paste0("hughparsonage/", 
                                c("hutilscpp",
                                  # "hildaExtra",
                                  "grattanCharts")), 
                         quick = TRUE,
                         quiet = TRUE)
cat(as.character(Sys.time()), "\t", "taxstats\n")
if (!requireNamespace("taxstats", quietly = TRUE) || 
    !requireNamespace("taxstats1516", quietly = TRUE)) {
  install.packages(c("taxstats", "taxstats1516"),
                   repos = "https://hughparsonage.github.io/tax-drat",
                   quiet = TRUE,
                   type = "source")
}

packages_ <-
  c("bindrcpp", "hutils", "ggrepel", "testthat", 
    "magrittr", "tidyr", "usethis", "devtools", "expm", "profmem",
    "Formula", "lattice", "foreign", "survey", "survival", "Matrix", 
    "grid", "zoo", "httr", "rsdmx", "readr", "openxlsx", "readxl", 
    "xtable", "grattan", "directlabels", "scales", "ggplot2", "gridExtra", 
    "dplyr", "haven", "knitr", 
    "lubridate",
    "showtext", "sysfonts",
    "data.table", "sessioninfo")

# For shinytest
shiny_app_deps <-
  c("BH", "DT", "MASS", "Matrix", "R6", "RColorBrewer", "Rcpp", 
    "askpass", "assertthat", "base64enc", "bindr", "bindrcpp", "callr", 
    "cli", "colorspace", "crayon", "crosstalk", "curl", "data.table", 
    "debugme", "digest", "dplyr", "fansi", "fastmatch", "ggplot2", 
    "glue", "gtable", "hexbin", "htmltools", "htmlwidgets", "httpuv", 
    "httr", "hutils", "jsonlite", "labeling", "later", "lattice", 
    "lazyeval", "magrittr", "mgcv", "mime", "munsell", "nlme", "openssl", 
    "parsedate", "pillar", "pingr", "pkgconfig", "plogr", "plotly", 
    "plyr", "png", "praise", "processx", "promises", "ps", "purrr", 
    "rematch", "reshape2", "rlang", "scales", "shiny", "shinytest", 
    "showimage", "sourcetools", "stringi", "stringr", "sys", "testthat", 
    "tibble", "tidyr", "tidyselect", "utf8", "viridisLite", "webdriver", 
    "withr", "xtable", "yaml")

left_remaining <- Filter(f = function(x) !requireNamespace(x, quietly = TRUE),
                         x = c(packages_, shiny_app_deps))

if (length(left_remaining) > 0) {
  if (left_remaining > 50) {
    install.packages(left_remaining, quiet = FALSE)
  } else {
    install.packages(packages_, quiet = TRUE)
  }
}

if (!requireNamespace("shinytest", quietly = TRUE) || 
    !requireNamespace("plotly", quietly = TRUE) ||
    length(left_remaining)) {
  cat("Installing ", length(left_remaining), " packages and shinytest...\n")
  if (!requireNamespace("devtools", quietly = TRUE)) {
    if (requireNamespace("crayon", quietly = TRUE)) {
      cat(crayon::red("devtools not installed\n"))
    } else {
      cat("devtools not installed\n")
    }
    install.packages("devtools", quiet = TRUE)
  }
  devtools::install_github("rstudio/shinytest", quick = TRUE, quiet = TRUE)
}


# Wait until we work out hilda
# knitr::knit("CGT_and_neg_gearing_parent.Rnw")






