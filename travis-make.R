



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

# Remove already installed packages
packages_to_install <- packages_
for (i in seq_along(packages_)) {
  packages_to_install[i] <- if (!length(find.package(packages_[i], quiet = TRUE))) "" else "(installed)"
}
packages_ <- packages_[packages_to_install == ""]

cat("packages_\t", paste0(packages_, collapse = " "), "\n")
if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext", quiet = TRUE)
}

if (length(packages_) > 0) {
  install.packages(packages_, repos = "https://cran.rstudio.com", quiet = TRUE)
}

# For shinytest
if (!requireNamespace("shinytest", quietly = TRUE) || 
    !requireNamespace("plotly")) {
  cat("Installing shinytest...\n")
  if (!requireNamespace("devtools", quietly = TRUE)) {
    if (requireNamespace("crayon", quietly = TRUE)) {
      cat(crayon::red("devtools not installed\n"))
    } else {
      cat("devtools not installed\n")
    }
    install.packages("devtools", quiet = TRUE)
  }
  devtools::install_github("rstudio/shinytest", quick = TRUE, quiet = TRUE)
  install.packages("plotly", quick = TRUE)
}


# Wait until we work out hilda
# knitr::knit("CGT_and_neg_gearing_parent.Rnw")






