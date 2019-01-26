

# First remove all but the CGT_parent
vapply(dir(path = ".", pattern = "\\.tex$"), function(x) {
  !grepl("parent", x) && file.remove(x)
}, FALSE)

install.packages("hutils", repos = "https://cran.rstudio.com")
install.packages("grattan", repos = "https://cran.rstudio.com", quiet = TRUE)
install.packages("TeXCheckR", repos = "https://cran.rstudio.com", dependencies = TRUE)

devtools::install_github(paste0("hughparsonage/", 
                                c("hutilscpp",
                                  # "hildaExtra",
                                  "grattanCharts")), 
                         quick = TRUE,
                         quiet = TRUE)
if (!requireNamespace("taxstats", quietly = TRUE) || 
    !requireNamespace("taxstats1516", quietly = TRUE)) {
  install.packages(c("taxstats", "taxstats1516"),
                   repos = "https://hughparsonage.github.io/tax-drat",
                   type = "source")
}

packages_ <-
  c("bindrcpp", "hutils", "ggrepel", "testthat", 
    "magrittr", "tidyr", "usethis", "devtools", "expm", "Hmisc", 
    "Formula", "lattice", "foreign", "survey", "survival", "Matrix", 
    "grid", "zoo", "httr", "rsdmx", "readr", "openxlsx", "readxl", 
    "xtable", "grattan", "directlabels", "scales", "ggplot2", "gridExtra", 
    "dplyr", "haven", "devEMF", "knitr", 
    "showtext", "sysfonts",
    "data.table", "sessioninfo")

# Remove already installed packages
packages_to_install <- packages_
for (i in seq_along(packages_)) {
  packages_to_install[i] <- if (!length(find.package(packages_[i], quiet = TRUE))) "" else "(installed)"
}
packages_ <- packages_[packages_to_install == ""]

cat("packages_\t", packages_, "\n")
if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")
}

if (length(packages_) > 0) {
  install.packages(packages_, repos = "https://cran.rstudio.com")
}

# Wait until we work out hilda
# knitr::knit("CGT_and_neg_gearing_parent.Rnw")






