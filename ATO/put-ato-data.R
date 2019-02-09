library(readxl)
library(hutils)
library(magrittr)
library(data.table)
stopifnot(file.exists("put-ato-data.R"))

Table14B_orig <- 
  read_excel("Individuals_table14_201516.xlsx", 
             sheet = "Individual Table 14B",
             skip = 3,
             na = c("na", "* Blank")) %>%
  as.data.table

Table14B <- copy(Table14B_orig)
stopifnot(identical(names(Table14B),
                    c("Occupation - unit group1",
                      "Occupation1",
                      "Gender",
                      "Number of individuals", 
                      "Average taxable income4\r\n$",
                      "Median taxable income4\r\n$")))
setnames(Table14B, 1L, "Occupation")
setnames(Table14B, "Number of individuals", "nIndividuals")
setnames(Table14B, grep("Average", names(Table14)), "AvgTaxableIncome")
setnames(Table14B, grep("Median", names(Table14)), "MedianTaxableIncome")
fwrite(Table14B, "Individuals_table14B_201516.tsv", sep = "\t")


