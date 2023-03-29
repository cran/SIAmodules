library(ShinyItemAnalysis)
library(difNLR)
library(lme4)
library(plotly)
library(dplyr)
library(ggplot2)
library(stringr)
library(bslib)
library(DT)

options(sass.cache = FALSE) # only needed at ICS servers, probably due to some file permissions


# total score summary table module ----------------------------------------

total_scores_summary_tab_iu <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      12,
      tableOutput(ns("tab"))
    ))
  )
}

total_scores_summary_tab_server <- function(id, dif_matching, group) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      d <- reactive({
        d <- tibble(
          score = dif_matching(),
          group = group() %>% factor(labels = c("reference (0)", "focal (1)"))
        )

        d %>%
          group_by(group) %>%
          summarise(
            n = n(),
            min = min(score, na.rm = TRUE),
            max = max(score, na.rm = TRUE),
            mean = mean(score, na.rm = TRUE),
            median = median(score, na.rm = TRUE),
            SD = sd(score, na.rm = TRUE),
            skeewness = ShinyItemAnalysis:::skewness(score),
            kurtosis = ShinyItemAnalysis:::kurtosis(score)
          )
      })

      output$tab <- renderTable(d(), digits = 2, colnames = TRUE)
    }
  )
}


# total scores histogram module -------------------------------------------

total_scores_hist_iu <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      12,
      plotlyOutput(ns("plot"))
    ))
  )
}

total_scores_hist_server <- function(id, dif_matching, group, xlab = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      plt <- reactive({
        d <- tibble(
          score = dif_matching(),
          group = group() %>% factor(labels = c("reference (0)", "focal (1)"))
        )

        d %>%
          ggplot(aes(x = score, fill = group, col = group)) +
          geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
          xlab(xlab) +
          ylab("Number of respondents") +
          scale_fill_manual(
            values = c("dodgerblue2", "goldenrod2"),
            labels = c("Reference", "Focal")
          ) +
          scale_colour_manual(
            values = c("dodgerblue2", "goldenrod2"),
            labels = c("Reference", "Focal")
          ) +
          theme_app()
      })

      output$plot <- renderPlotly({
        plt() %>%
          ggplotly() %>%
          config(displayModeBar = FALSE)
      })
    }
  )
}


# total scores t-test module -------------------------------------------


total_scores_t_test_iu <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      12,
      tableOutput(ns("tab"))
    ))
  )
}

total_scores_t_test_server <- function(id, dif_matching, group) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      d <- reactive({
        d <- tibble(
          score = dif_matching(),
          group = group() %>% factor(labels = c("reference (0)", "focal (1)"))
        )

        res <- t.test(score ~ group, d)

        data.frame(
          `Diff.` = res$estimate[1] - res$estimate[2],
          LCI = res$conf.int[1],
          UCI = res$conf.int[2],
          `\\(t\\)` = res$statistic,
          df = res$parameter,
          `\\(p\\)` = res$p.value,
          check.names = FALSE
        )
      })

      output$tab <- renderTable(d(), digits = 3, colnames = TRUE, rownames = FALSE)
    }
  )
}
