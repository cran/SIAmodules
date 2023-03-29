library(ShinyItemAnalysis)
library(difNLR)
library(lme4)
library(plotly)
library(dplyr)
library(tibble)
library(ggplot2)
library(purrr)
library(stringr)
library(bslib)
library(shinyjs)
library(DT)

# only needed at ICS servers, probably due to some file permissions
options(sass.cache = FALSE)


# utils -------------------------------------------------------------------

p_val <- function(x, digits = 3) {
  sapply(x, function(x) {
    if (x < .001) {
      return("< .001")
    }
    substring(format(round(x, 3)), 2)
  })
}

tm <- function(x) {
  attr(x, "time")
}

it_set <- function(x) {
  attr(x, "item_set")
}


# static summaries --------------------------------------------------------

ts_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("title")),

    # descriptives
    uiOutput(ns("summary_tab_title")),
    fluidRow(column(12, tableOutput(ns("summary_tab")))),

    # t test
    uiOutput(ns("t_test_tab_title")),
    fluidRow(column(12, tableOutput(ns("t_test_tab")))),

    # histogram
    uiOutput(ns("hist_title")),
    fluidRow(column(12, plotlyOutput(ns("hist"))))
  )
}

ts_summary <- function(id, group, score) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session


      # section title -----------------------------------------------------------

      # conditional part for later use
      cond_title <- reactive(paste(it_set(score()), tm(score())))

      output$title <- renderUI({
        h5(cond_title())
      })


      # main data ---------------------------------------------------------------

      d <- reactive(tibble(group = group, score = score()))


      # descriptives ------------------------------------------------------------

      summary_tab <- reactive({
        d() %>%
          group_by(group) %>%
          summarise(
            n = n(),
            min = min(score, na.rm = TRUE),
            max = max(score, na.rm = TRUE),
            mean = mean(score, na.rm = TRUE),
            median = median(score, na.rm = TRUE),
            SD = sd(score, na.rm = TRUE),
            # maybe use rather getFromNamespace(), but it's only necessary for CRAN
            skeewness = ShinyItemAnalysis:::skewness(score),
            kurtosis = ShinyItemAnalysis:::kurtosis(score)
          )
      })

      # table of descriptives
      output$summary_tab <- renderTable(summary_tab(), colnames = TRUE, digits = 2)

      # table of descriptives title
      output$summary_tab_title <- renderUI({
        h6("Summary of", cond_title(), "total scores for groups")
      })


      # t test ------------------------------------------------------------------

      t_test_tab <- reactive({
        t_test_res <- t.test(score ~ group, d())

        tibble(
          `\\(M_{diff}\\)` = diff(t_test_res$estimate) * -1,
          LCI = t_test_res$conf.int[1],
          UCI = t_test_res$conf.int[2],
          `\\(t\\)` = t_test_res$statistic,
          df = t_test_res$parameter,
          `\\(p\\)` = p_val(t_test_res$p.value)
        )
      })

      output$t_test_tab <- renderTable(t_test_tab(),
        digits = 2, colnames = TRUE, rownames = FALSE
      )

      output$t_test_tab_title <- renderUI({
        h6("Between-group comparison of", cond_title(), "total scores")
      })


      # histogram ---------------------------------------------------------------

      hist <- reactive({
        d() %>%
          ggplot(aes(x = score, fill = group, col = group)) +
          geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
          xlab(paste(cond_title(), "total score")) +
          ylab("Number of respondents") +
          scale_fill_manual(
            values = c("dodgerblue2", "goldenrod2"),
            labels = c("Control", "Experimental")
          ) +
          scale_colour_manual(
            values = c("dodgerblue2", "goldenrod2"),
            labels = c("Control", "Experimental")
          ) +
          theme_app()
      })

      output$hist <- renderPlotly({
        hist() %>%
          ggplotly() %>%
          config(displayModeBar = FALSE)
      })

      output$hist_title <- renderUI({
        h6("Histogram of", cond_title(), "total scores for groups")
      })
    }
  )
}




# timepoint differences ---------------------------------------------------

time_diff_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("title")),
    fluidRow(
      div(
        class = "col-sm-auto",
        h6("Difference in testing sessions for groups"),
        tableOutput(ns("time_diff_tab"))
      ),
      div(
        class = "col-sm-auto",
        h6("Intergroup difference in time"),
        tableOutput(ns("bg_time_diff_tab"))
      )
    )
  )
}

time_diff <- function(id, t1, t2, group) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      output$title <- renderUI({
        h5(paste(str_to_sentence(tm(t1())), "–", tm(t2()), it_set(t1()), "difference in time"))
      })

      d <- reactive(tibble(
        t1 = t1(),
        t2 = t2(),
        diff = t1 - t2,
        group = group
      ))

      time_diff_tab <- reactive({
        d() %>%
          group_by(group) %>%
          summarise(t_test = list(t.test(t1, t2, paired = TRUE))) %>%
          mutate(
            `\\(M_{diff}\\)` = map_dbl(t_test, "estimate") * -1,
            LCI = map_dbl(t_test, ~ .x %>% pluck("conf.int", 1)),
            UCI = map_dbl(t_test, ~ .x %>% pluck("conf.int", 2)),
            `\\(t\\)` = map_dbl(t_test, "statistic"),
            df = map_dbl(t_test, "parameter"),
            `\\(p\\)` = p_val(map_dbl(t_test, "p.value"))
          ) %>%
          select(-t_test)
      })

      output$time_diff_tab <- renderTable(time_diff_tab(),
        digits = 2, colnames = TRUE, rownames = FALSE
      )


      bg_time_diff_tab <- reactive({
        res <- t.test(diff ~ group, d())

        tibble(
          `\\(M_{diff}\\)` = diff(res$estimate) * -1,
          LCI = res$conf.int[1],
          UCI = res$conf.int[2],
          `\\(t\\)` = res$statistic,
          df = res$parameter,
          `\\(p\\)` = p_val(res$p.value)
        )
      })

      output$bg_time_diff_tab <- renderTable(bg_time_diff_tab(),
        digits = 2, colnames = TRUE, rownames = FALSE
      )
    }
  )
}


# cumulative DIF summary & items module -----------------------------------

dif_ui <- function(id, lab = "") {
  ns <- NS(id)

  tagList(

    # model specification -----------------------------------------------------

    h4("Model specification"),
    p(
      "In their study, Kolek et al. (2021) used the group-specific cumulative logit model to detect DIF on Pretest, and
      DIF-C in Posttest and in Delayed posttest. They tested the hypthesis of any DIF/DIF-C against the alternative
      of any type of DIF/DIF-C (uniform or nonuniform). They used the Pretest total score as a matching criterion and
      the Benjamini-Hochberg correction for multiple comparisons. Item purification was not applied.
      Here we offer the DIF/DIC-C analysis with the same settings as in Kolek et al. (2021).
      You can also change the ", strong("type"), " of DIF to be tested, the ", strong("matching criterion", .noWS = "outside"),
      ", and the ", strong("parametrization"), "- either the IRT or the classical intercept/slope. You can also
        select a ", strong("correction method"), " for a multiple comparison and/or ", strong("item purification. ")
    ),
    fluidRow(
      column(
        3,
        radioButtons(
          ns("type"), "Type",
          c(
            "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Any DIF" = "both",
            "\\(H_{0}\\): No DIF vs. \\(H_{1}\\): Uniform DIF" = "udif",
            "\\(H_{0}\\): Uniform DIF vs. \\(H_{1}\\): Non-uniform DIF" = "nudif"
          ),
          "both"
        )
      ),
      column(
        3,
        selectInput(
          ns("dif_matching"), "Matching criterion",
          if (id == "pretest") {
            c(
              "Pretest total score" = "score",
              "Pretest standardized total score" = "zscore"
            )
          } else {
            setNames(
              c("pretest_ts", "pretest_sts", "score", "zscore"),
              c(
                "Pretest total score", "Pretest standardized total score",
                paste(lab, "total score"), paste(lab, "standardized total score")
              )
            )
          }, "pretest_ts"
        )
      ),
      column(
        3,
        selectInput(
          ns("parametrization"), "Parametrization",
          c(
            "Intercept/slope" = "classic",
            "IRT" = "irt"
          ), "classic"
        )
      ),
      column(
        3,
        selectInput(
          ns("correction"), "Correction method",
          c(
            "Benjamini-Hochberg" = "BH",
            "Benjamini-Yekutieli" = "BY",
            "Bonferroni" = "bonferroni",
            "Holm" = "holm",
            "Hochberg" = "hochberg",
            "Hommel" = "hommel",
            "None" = "none"
          ),
          selected = "BH"
        ),
        checkboxInput(
          ns("purification"),
          tagList(
            span("Item purification", .noWS = "outside"),
            if (id != "pretest") {
              span(
                class = "text-muted ml-1",
                "(allowed only for", tolower(lab), "scores)", .noWS = "inside"
              )
            }
          ),
          FALSE
        )
      )
    ),
    h4("Equation"),
    p(
      "The probability that respondent ", strong("\\(p\\)"), " with the pretest score (matching criterion) ", strong("\\(X_p\\)"), " and the group membership variable ", strong("\\(G_p\\)"), " obtained at least ",
      strong("\\(k\\)"), " points in item ", strong("\\(i\\)"), " is given by the following equation: "
    ),
    fluidRow(column(12, align = "center", uiOutput(ns("eq_cumulative")))),
    p(
      "The probability that respondent ", strong("\\(p\\)"), " with the pretest score (matching criterion) ", strong("\\(X_p\\)"), " and group membership ", strong("\\(G_p\\)"), " obtained exactly ", strong("\\(k\\)"),
      " points in item ", strong("\\(i\\)"), " is then given as the difference between the probabilities of obtaining at least",
      strong("\\(k\\)"), " and ", strong("\\(k + 1\\)"), "points: "
    ),
    fluidRow(column(12, align = "center", uiOutput(ns("eq_category")))),
    tabsetPanel(
      tabPanel(
        "Summary",
        h4("Results"),
        strong(textOutput(ns("summary_dif_items"))),
        uiOutput(ns("na_warning_summary")),
        h4("Test statistic and model parameter estimates"),
        p("This summary table contains information about \\(\\chi^2\\)-statistics of the likelihood ratio test, corresponding
        \\(p\\)-values considering selected correction method, and significance codes. The table also provides estimated parameters
        for the best fitted model for each item."),
        fluidRow(
          column(12,
            align = "left",
            tableOutput(ns("summary_coef_tab")),
            style = "overflow-x: scroll;"
          )
        ),
        fluidRow(
          column(8,
            align = "left",
            em(uiOutput(ns("summary_coef_tab_note")))
          ),
          column(4,
            align = "right",
            downloadButton(ns("summary_coef_tab_db"), "Download table")
          ),
          class = "d-flex justify-content-between mt-3"
        ),
        h4("Purification process"),
        fluidRow(column(
          12,
          textOutput(ns("purification_info"))
        )),
        conditionalPanel(
          "output.purification_state",
          fluidRow(column(12,
            class = "mt-3",
            align = "left",
            tableOutput(ns("purification_tab")),
            style = "overflow-x: scroll;"
          )),
          fluidRow(column(12,
            align = "right", class = "mt-3",
            downloadButton(ns("purification_tab_db"), "Download table")
          )),
          ns = ns
        )
      ),
      tabPanel(
        "Items",
        h4("Plot with estimated DIF curves"),
        p(
          "Points represent a proportion of the obtained score with respect to
        the matching criterion. Their size is determined by the count of
        respondents who achieved a given level of the matching criterion and who
        selected given option with respect to the group membership.",
          uiOutput(ns("na_warning_items"))
        ),
        fluidRow(column(
          12,
          sliderInput(ns("item"), "Item",
            min = 1,
            max = 10,
            value = 1,
            step = 1,
            animate = animationOptions(interval = 1600)
          )
        )),
        fluidRow(
          column(
            6,
            plotlyOutput(ns("plot_items_cumulative"))
          ),
          column(
            6,
            plotlyOutput(ns("plot_items_category"))
          )
        ),
        fluidRow(
          class = "d-flex justify-content-between mt-3",
          column(
            6,
            align = "right",
            downloadButton(ns("plot_items_cumulative_db"), label = "Download figure")
          ),
          column(
            6,
            align = "right",
            downloadButton(ns("plot_items_category_db"), label = "Download figure")
          )
        ),
        h4("Table of parameters"),
        p("This table summarizes estimated item parameters together with the standard errors. "),
        fluidRow(column(12, align = "center", tableOutput(ns("items_coef"))))
      )
    )
  )
}

dif_server <- function(id, items, group, dif_matching) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      item_names <- reactive(names(items()))

      na_warning <- reactive({
        if (any(is.na(items()))) {
          txt <- "<font color = 'orange'>
				For this analysis, observations with missing values have been omitted.
				</font>"
        } else {
          txt <- ""
        }
        txt
      })

      output$na_warning_summary <- renderUI(HTML(na_warning()))
      output$na_warning_items <- renderUI(HTML(na_warning()))

      # fit model ---------------------------------------------------------------

      fit <- reactive({
        # because difNLR cannot recognize score or zscore that it needs to
        # conduct item purification
        # do not supply vector if the input is score or zscore

        match <- if (input$dif_matching %in% c("score", "zscore")) {
          input$dif_matching
        } else {
          dif_matching()
        }

        # check if purification is allowed, else reset, disable the checkbox and pass FALSE
        if (!input$dif_matching %in% c("score", "zscore")) {
          purification <- FALSE
          reset("purification")
          disable("purification")
        } else {
          purification <- input$purification
          enable("purification")
        }

        # fit and return
        difORD(
          items(),
          group,
          focal.name = "experimental",
          model = "cumulative",
          match = match,
          type = input$type,
          purify = purification,
          p.adjust.method = input$correction # ,
          # parametrization = input$parametrization
        )
      })

      output$eq_cumulative <- renderUI({
        txt1 <- "X_p"
        txt2 <- if (input$parametrization == "irt") {
          paste0(
            "(a_{i} + a_{i\\text{DIF}} G_p)",
            "(", txt1, " - b_{ik} - b_{ik\\text{DIF}} G_p)"
          )
        } else {
          paste0("\\beta_{i0k} + \\beta_{i1} ", txt1, " + \\beta_{i2} G_p + \\beta_{i3} ", txt1, ":G_p")
        }
        txt3 <- "X_p, G_p"

        txt <- paste0("$$\\mathrm{P}(Y_{pi} \\geq k|", txt3, ") = \\frac{e^{", txt2, "}}{1 + e^{", txt2, "}}$$")
        txt
      })

      output$eq_category <- renderUI({
        txt1 <- "X_p"
        txt3 <- "X_p, G_p"

        txt <- paste0("$$\\mathrm{P}(Y_{pi} = k|", txt3, ") = \\mathrm{P}(Y_{pi} \\geq k|", txt3, ") - \\mathrm{P}(Y_{pi} \\geq k + 1|", txt3, ")$$")
        txt
      })

      output$summary_dif_items <- renderPrint({
        DIFitems <- fit()$DIFitems
        if (DIFitems[1] == "No DIF item detected") {
          txt <- "No item was detected as DIF."
        } else {
          txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
        }
        HTML(txt)
      })

      # ** DIF statistic and parameter table ####
      summary_coef_tab <- reactive({
        fit <- fit()

        # only one p-value, based on model specifications
        pval <- if (fit$p.adjust.method == "none") {
          fit$pval
        } else {
          fit$adj.pval
        }

        pval_symb <- symnum(pval,
          c(0, 0.001, 0.01, 0.05, 0.1, 1),
          symbols = c("***", "**", "*", ".", "")
        )

        blank <- character(ncol(fit$Data))

        # estimated coefficients for all items with standard errors
        coefs <- coef(fit, SE = TRUE, simplify = TRUE, IRTpars = (input$parametrization == "irt"), CI = 0)

        # adding missing columns if necessary / ordering columns
        if (input$parametrization == "classic") {
          new_cols <- c(group = 0, "x:group" = 0)
          coefs <- coefs %>% add_column(!!!new_cols[setdiff(names(new_cols), names(coefs))])
        } else {
          coefs <- cbind(
            coefs[, grepl("a", colnames(coefs))],
            coefs[, !grepl("a", colnames(coefs))]
          )
        }
        estims <- coefs[c(TRUE, FALSE), ]
        ses <- coefs[c(FALSE, TRUE), ]

        pars_zigzag <-
          cbind(estims, ses)[, order(c(seq(ncol(estims)), seq(ncol(ses))))]

        # renaming item parameters based on parametrization
        pars_digits_only <- stringr::str_extract(colnames(estims), "\\d")

        if (input$parametrization == "irt") {
          txt <- stringr::str_replace(colnames(pars_zigzag), "\\.1", "")
          txt[grepl("DIF", txt)] <- paste0(gsub("DIF", "", txt[grepl("DIF", txt)]), "\\text{DIF}")
          txt <- stringr::str_replace(txt, "(?=\\d)", "}_{")
          txt[txt == "a\\text{DIF}"] <- "a}_{\\text{DIF}"
          txt <- stringr::str_replace(txt, ".*(?=b|a)", "\\\\(\\\\mathit{")
          colnames(pars_zigzag) <- paste0(c("", "SE("), txt, c("}\\)", "}\\))"))
        } else {
          txt <- c(
            paste0("\\(\\mathit{\\beta}_{0", pars_digits_only[!is.na(pars_digits_only)], "}"),
            "\\(\\mathit{\\beta}_1", "\\(\\mathit{\\beta}_2", "\\(\\mathit{\\beta}_3"
          )
          colnames(pars_zigzag) <- paste0(c("", "SE("), rep(txt, each = 2), c("\\)", "\\))"))
        }

        # table
        tab <-
          data.frame(
            check.names = FALSE, # in R 4.x.x, this changes parentheses
            fit$Sval, # to dots (to ensure "syntactical validity")
            pval,
            pval_symb,
            blank,
            pars_zigzag
          )

        colnames(tab)[1:4] <- c(
          "LR (\\(\\mathit{\\chi^2}\\))",
          ifelse(
            fit$p.adjust.method == "none",
            "\\(\\mathit{p}\\)-value",
            "adj. \\(\\mathit{p}\\)-value"
          ),
          "",
          ""
        )

        rownames(tab) <- item_names()
        tab
      })

      output$summary_coef_tab <- renderTable(
        {
          summary_coef_tab()
        },
        rownames = TRUE,
        colnames = TRUE
      )


      summary_coef_tab_note <- reactive({
        res <- NULL
        fit <- fit()

        res$matching <- paste(
          "Matching criterion:",
          if (fit$match[1] == "score") {
            "total score"
          } else if (fit$match[1] == "zscore") {
            "standardized total score"
          } else {
            "standardized uploaded"
          }
        )
        res$type <- paste(
          "DIF type tested:",
          switch(fit$type,
            "both" = "any DIF ",
            "udif" = "uniform DIF ",
            "nudif" = "non-uniform DIF "
          )
        )
        res$correction <- paste(
          "P-value correction method:",
          switch(fit$p.adjust.method,
            "BH" = "Benjamini-Hochberg",
            "BY" = "Benjamini-Yekutieli",
            "bonferroni" = "Bonferroni",
            "holm" = "Holm",
            "hochberg" = "Hochberg",
            "hommel" = "Hommel",
            "none" = "none"
          )
        )
        res$purification <- paste(
          "Item purification:",
          ifelse(fit$purification, "used", "unutilized")
        )

        res
      })

      output$summary_coef_tab_note <- renderUI({
        note <- summary_coef_tab_note()

        HTML(
          paste(
            "Notes:",
            note$matching,
            note$type,
            note$correction,
            note$purification,
            "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
            sep = "</br>"
          )
        )
      })


      output$summary_coef_tab_db <- downloadHandler(
        filename = function() {
          "coefficients_table.csv"
        },
        content = function(file) {
          table <- summary_coef_tab()
          note <- summary_coef_tab_note()

          # remove all math-format characters -->> plaintext
          colnames(table) <- stringr::str_remove_all(
            colnames(table),
            "\\\\\\(\\\\\\mathit\\{|\\\\text\\{|\\}_\\{|\\}+\\\\\\)|\\\\"
          )
          # FIXME issue with slope/intercept parametrisation; + anything more elegant?

          write.csv(table[, -4], file)
          write(paste(
            "Note:",
            note$matching,
            note$type,
            note$correction,
            note$purification,
            "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
            sep = "\n"
          ), file, append = TRUE)
        }
      )


      # item purification -------------------------------------------------------

      # make a JS variable output.purification_state that can be handled in conditionalPanel()
      output$purification_state <- reactive({
        if (is.null(fit()$difPur)) FALSE else TRUE
      })
      outputOptions(output, "purification_state", suspendWhenHidden = FALSE)



      # ** Purification info - number of iterations ####
      output$purification_info <- renderPrint({
        fit <- fit()
        if (input$purification & !is.null(fit$difPur)) {
          cat("The table below describes purification process. The rows correspond to the purification iteration and the columns
        correspond to items. Value of '1' in the \\(i\\)-th row means that an item was detected as DIF in (\\(i-1\\))-th step,
        while the value of '0' means that the item was not detected as DIF. The first row corresponds to the initial
        classification of the items when all items were used for the calculation of the DIF matching criterion. ")
          nrIter <- fit$nrPur
          cat(
            "In this case, the convergence was", ifelse(fit$conv.puri, "reached", "NOT reached even"), "after", nrIter,
            ifelse(nrIter == 1, "iteration.", "iterations.")
          )
        } else if (input$purification & is.null(fit$difPur)) {
          cat("No DIF item was detected whatsoever, nothing to show.")
        } else {
          cat("Item purification was not requested, nothing to show.")
        }
      })

      purification_tab <- reactive({
        tab <- fit()$difPur
        if (!is.null(tab)) {
          colnames(tab) <- item_names()
          rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
          tab
        }
      })

      output$purification_tab <- renderTable(
        {
          purification_tab()
        },
        rownames = TRUE,
        colnames = TRUE,
        digits = 0
      )

      output$purification_tab_db <- downloadHandler(
        filename = function() {
          "item_purification.csv"
        },
        content = function(file) {
          data <- purification_tab()
          write.csv(data, file)
        }
      )



      # items -------------------------------------------------------------------

      observeEvent(items(), {
        updateSliderInput(
          inputId = "item", min = 1, max = ncol(items()), step = 1, value = 1
        )
      })

      # ** Plot - cumulative ####
      plot_items_cumulative <- reactive({
        fit <- fit()
        item <- input$item


        g <- plot(fit, item = item, plot.type = "cumulative")[[1]] +
          theme_app() +
          ggtitle(item_names()[item]) +
          theme(
            legend.box.just = "top",
            legend.justification = c("right", "bottom"),
            legend.position = c(0.98, 0.02),
            legend.box = "horizontal",
            legend.margin = margin(0, 0, 0, 0, unit = "cm")
          )
        g
      })

      output$plot_items_cumulative <- renderPlotly({
        g <- plot_items_cumulative()
        p <- ggplotly(g)

        for (i in 1:length(p$x$data)) {
          text <- p$x$data[[i]]$text
          text <- gsub("size", "Count", text)
          text <- gsub("category", "Category", text)
          text <- gsub("probability", "Probability", text)
          text <- gsub("matching", "Z-score", text)
          p$x$data[[i]]$text <- text
        }

        for (i in 1:length(p$x$data)) {
          text <- p$x$data[[i]]$text
          text <- lapply(strsplit(text, split = "<br />"), unique)
          text <- unlist(lapply(text, paste, collapse = "<br />"))
          p$x$data[[i]]$text <- text
        }

        p$elementId <- NULL
        hide_legend(p) %>% plotly::config(displayModeBar = FALSE)
      })

      # ** Download plot - cumulative ####
      output$plot_items_cumulative_db <- downloadHandler(
        filename = function() {
          paste0("fig_DIF_cumulative_cumulative_", item_names()[input$item], ".png")
        },
        content = function(file) {
          ggsave(file,
            plot = plot_items_cumulative(),
            width = unit(7, "in"), height = unit(5, "in"), scale = 1.2
          )
        }
      )

      # ** Plot - category ####
      plot_items_category <- reactive({
        fit <- fit()
        item <- input$item

        g <- plot(fit, item = item, plot.type = "category")[[1]] +
          theme_app() +
          ggtitle(item_names()[item]) +
          theme(
            legend.box.just = "top",
            legend.justification = c("left", "top"),
            legend.position = c(0.02, 0.98),
            legend.box = "horizontal",
            legend.margin = margin(0, 0, 0, 0, unit = "cm")
          )
        g
      })

      output$plot_items_category <- renderPlotly({
        g <- plot_items_category()
        p <- ggplotly(g)

        for (i in 1:length(p$x$data)) {
          text <- p$x$data[[i]]$text
          text <- gsub("size", "Count", text)
          text <- gsub("category", "Category", text)
          text <- gsub("probability", "Probability", text)
          text <- gsub("matching", "Z-score", text)
          p$x$data[[i]]$text <- text
        }

        for (i in 1:length(p$x$data)) {
          text <- p$x$data[[i]]$text
          text <- lapply(strsplit(text, split = "<br />"), unique)
          text <- unlist(lapply(text, paste, collapse = "<br />"))
          p$x$data[[i]]$text <- text
        }

        p$elementId <- NULL
        hide_legend(p) %>% plotly::config(displayModeBar = FALSE)
      })

      # ** Download plot - category ####
      output$plot_items_category_db <- downloadHandler(
        filename = function() {
          paste0("fig_DIF_cumulative_category_", item_names()[input$item], ".png")
        },
        content = function(file) {
          ggsave(file,
            plot = plot_items_category()
          )
        }
      )


      # ** Table of coefficients ####
      items_coef <- reactive({
        tab <- summary_coef_tab()
        item <- input$item

        tab <- t(tab[item, -c(1:4)])
        tab <- data.frame(tab[!grepl("SE", rownames(tab)), ], tab[grepl("SE", rownames(tab)), ])
        colnames(tab) <- c("Estimate", "SE")

        tab
      })

      output$items_coef <- renderTable(
        {
          items_coef()
        },
        include.rownames = TRUE,
        include.colnames = TRUE
      )
    }
  )
}
