#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(data.table)
library(magrittr)
library(hutils)
library(ggplot2)
library(plotly)

library(shiny)

NegativeGearing_by_Occupation <-
    fread("ATO/NegativeGearing_by_Occupation.csv") %>%
    .[variable %ein% c("Number of individuals"), unit := "number"] %>%
    .[, TaxBracketA := sub("^([a-e]).*$", "\\1", TaxBracket)] %>%
    .[]

Indviduals_Table14_201516 <- 
    fread("ATO/Individuals_table14B_201516.tsv", sep = "\t")

avgIncome_vs_Occupation_201516 <-
    Indviduals_Table14_201516[Gender %ein% "Total",
                              .(avgIncome = weighted.mean(AvgTaxableIncome, nIndividuals, na.rm = TRUE)),
                              keyby = .(Occupation)] %>%
    .[]

nIndividuals_by_Occupation <- 
  Indviduals_Table14_201516[Gender %ein% "Total",
                            .(nIndividuals = sum(nIndividuals)),
                            keyby = .(Occupation)] %>%
  .[]

avgMarginalRate_vs_TaxBracket <-
    fread("ATO/avgMarginalRate_vs_TaxBracket.csv") %>%
    setnames("TaxBracket", "TaxBracketA") %>%
    .[]

NG_Benefit_vs_Occupation <- 
  NegativeGearing_by_Occupation %>%
  .[, .(Occupation, TaxBracket, Gender, variable, value, unit)] %>%
  dcast.data.table(... ~ variable + unit) %>%
  setnames("Net rent - loss_dollar", "NetRentDollar") %>%
  setnames("Number of individuals_number", "nIndividuals") %>%
  .[TaxBracket %enotin% "All"] %>% # some small occupations have 'all' rather than brackets
  .[, TaxBracketA := sub("^([a-e]).*$", "\\1", TaxBracket)] %>%
  .[avgMarginalRate_vs_TaxBracket, on = "TaxBracketA"] %>%
  # minus because losses are reported as negatives
  .[, benefit := -avgMarginalRate * NetRentDollar] %>%
  .[] %>%
  .[, .(totBenefit = sum(benefit)), 
    keyby = "Occupation"] %>%
  .[avgIncome_vs_Occupation_201516, on = "Occupation", 
    nomatch = 0L] %>%
  .[nIndividuals_by_Occupation, on = "Occupation", nomatch = 0L] %>%
  .[, avgBenefit := totBenefit / nIndividuals] %>%
  # Shorten
  .[, Occupation := sub("^[0-9]+ ", "", Occupation)] %>%
  .[, Occupation := sub("- type not specified", 
                        " (other)",
                        Occupation,
                        fixed = TRUE)] %>%
  .[grep("legal prof", Occupation, ignore.case = TRUE),
    Occupation := paste0("Lawyers/", Occupation)] %>%
  .[]


grattan.palette <- readRDS("col/grattan-palette.rds")
heading <- "Negative gearing 2015-16"

gpal <- function(n, dark = TRUE, reverse = FALSE) {
  
  if(n > 6) {
    if(n > 9) {
      gpalx <- function(n) grDevices::colorRampPalette(colors = grattan.palette[[6]])(n)
      if (reverse) 
        return(gpalx(n)) 
      else 
        return(rev(gpalx(n)))
    } 
  }
  if (!dark) {
    if (n == 2){
      out <- pal.2
    } else {
      # warning("no light option for palette ", n)
      out <- grattan.palette[[n]]
    }             
  } else {
    out <- grattan.palette[[n]]
  }
  if (reverse){
    rev(out)
  } else {
    out
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("styles.css"),
  
  tags$head(tags$style("
                       table.dataTable thead th {
                       padding: 8px 10px !important;
                       }
                       ")),
  
  tags$head(HTML('<link rel="icon" type="image/png" href="https://grattan.edu.au/wp-content/themes/grattan_responsive/images/fav.jpg" sizes="16x16">'),
            HTML('<link rel="icon" type="image/png" href="https://grattan.edu.au/wp-content/themes/grattan_responsive/images/fav.jpg" sizes="32x32">')),
  
  #tagList(tags$head(HTML('<h1><div style="vertical-align:text-bottom;"><div style="float:left;">Hospital complication calculator</div><div style="float:right;vertical-align:text-bottom;"><img src="GrattanLogo_right.png" type = "image/png" align="right; vertical-align:bottom;"/></div></div></h1>'))),
 
  # Application title 
  tagList(tags$head(tags$title(heading))),
  
  fluidRow(
    column(7),
    column(3,
           div(img(src = "GrattanLogo_right.png", align = "right", type = "image/png")))
  ),
  br(),
  
 
 
  
    # Sidebar with a slider input for number of bins 
  fluidRow(
    column(3, 
           shiny::textInput("search_q", label = "Search for occupations (comma or space separated):", value = ""),
           offset = 1)
  ),
  br(),
  # Show a plot of the generated distribution
  fluidRow(
    column(DT::dataTableOutput("Table1"),
           width = 8,
           offset = 1)
  ),
  br(),
  fluidRow(
    column(width = 8, offset = 1,
           plotlyOutput("Plot1")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    focused_table <- function(.pattern) {
      o <- copy(NG_Benefit_vs_Occupation)
      if (is_focused <- nzchar(.pattern)) {
        
        .patterns <- strsplit(.pattern, split = ",| ")[[1L]]

        o[, Occupation := coalesce(Occupation, "(Unknown)")]
        if (length(.patterns) > 1L) {
          o <- o[grep(paste0(.patterns, collapse = "|"), Occupation, ignore.case = TRUE, perl = TRUE)]
        } else {
          o <- o[grep(.pattern, Occupation, ignore.case = TRUE, perl = TRUE)]
        }
        
      } else {
        o <- copy(o)
        o[, Occupation := coalesce(Occupation, "(Unknown)")]
      }
      
      
      o[, totBenefit := round(totBenefit, -3)]
      o[, avgBenefit := round(avgBenefit, 0)]
      list(all = NG_Benefit_vs_Occupation, 
           is_focused = is_focused,
           focus = o[])
    }
    
    output$Table1 <- 
        DT::renderDataTable(
            DT::datatable({
                .pattern <- input$search_q
                need(length(.pattern) == 1L, label = "Occupation:")
                o1 <- focused_table(.pattern)[[3L]]
                o1 <- copy(o1)
                setnames(o1, "totBenefit", "Total benefit from NG")
                setnames(o1, "avgBenefit", "Average benefit from NG")
                setnames(o1, "avgIncome", "Average taxable income")
                setnames(o1, "nIndividuals", "Number of taxpayers")
                setcolorder(o1, 
                            c("Occupation", 
                              "Number of taxpayers",
                              "Average taxable income",
                              "Average benefit from NG",
                              "Total benefit from NG"))
                o1
            },
            rownames = FALSE,
            options = list(dom = "t"),
            selection = "none") %>%
                DT::formatCurrency(., columns = "Total benefit from NG", currency = "$", digits = 0) %>%
                DT::formatCurrency(., columns = "Average taxable income", currency = "$", digits = 0) %>%
                DT::formatCurrency(., columns = "Average benefit from NG", currency = "$", digits = 0) %>%
                DT::formatRound(columns = "Number of taxpayers", digits = 0))
    
    output$Plot1 <- 
      renderPlotly({
        .pattern <- input$search_q
        FT <- focused_table(.pattern)
        p <- 
          if (FT[[2L]]) {
            if (grepl(" ", .pattern) || grepl(",", .pattern)) {
              .dat <- FT[[3L]]
              .patterns <- strsplit(.pattern, split = ",| ")[[1L]]
              .dat[, color := NA_character_]
              for (p in seq_along(.patterns)) {
                .dat[grep(.patterns[p], Occupation, perl = TRUE, ignore.case = TRUE),
                     color := gpal(length(.patterns))[p]]
              }
              highlight_dat <- 
                FT[[3L]] %>%
                copy
              for (i in seq_along(.patterns)) {
                highlight_dat[grepl(.patterns[i], Occupation),
                              Occupation_label := .patterns[i]]
              }
              # highlight_dat <- 
              #   highlight_dat %>%
              #   .[complete.cases(Occupation_label)] %>%
              #   .[, .(avgIncome = mean(avgIncome),
              #         maxIncome = max(avgIncome),
              #         minIncome = min(avgIncome),
              #         avgBenefit = mean(avgBenefit),
              #         minBenefit = min(avgBenefit),
              #         maxBenefit = max(avgBenefit)),
              #     keyby = .(Occupation_label)]
              
              
              ggplot(FT[[1L]],
                     aes(avgIncome, avgBenefit, size = nIndividuals,
                         text = Occupation)) + 
                geom_point(color = "#6A737B") + 
                geom_point(data = FT[[3L]],
                           mapping = aes(fill = color),
                           color = "black") +
                annotate("text", 
                         x = double(length(.patterns)), 
                         y = FT[[1L]][, max(avgBenefit)] - FT[[1L]][, max(avgBenefit) / 3] * (seq_along(.patterns) - 1L), 
                         label = .patterns,
                         hjust = 0,
                         color = gpal(length(.patterns))) +
                scale_color_identity() +
                scale_fill_identity() +
                theme_bw() + 
                theme(legend.position = "none")
            } else {
              ggplot(FT[[1L]],
                     aes(avgIncome, avgBenefit, size = nIndividuals,
                         text = Occupation)) + 
                geom_point(color = "#6A737B") + 
                geom_point(data = FT[[3L]],
                           color = "#A02226") +
                # geom_label_repel(data = FT[[3L]],
                #                  mapping = aes(label = sub("^[0-9]+ ([A-Za-z]+).*?$",
                #                                            "\\1",
                #                                            Occupation,
                #                                            perl = TRUE)),
                #                  fill = "#A02226",
                #                  color = "white") +
                theme_bw()
            }
          } else {
            ggplot(FT[[1L]],
                   aes(avgIncome, avgBenefit, size = nIndividuals,
                       text = Occupation)) + 
              geom_point(color = "#6A737B")
          }
        p <- p + scale_y_continuous("Average benefit", labels = scales::dollar)
        p <- p + scale_x_continuous("Average income", labels = scales::dollar)
        print(highlight(ggplotly(p, tooltip = "text"),
                        on = "plotly_hover",
                        color = "rgba(255,0,0,1)",
                        debounce = 10))
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
