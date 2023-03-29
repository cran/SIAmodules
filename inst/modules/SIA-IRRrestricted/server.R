function(input, output, session) {

  # kills the local server as the window closes
  session$onSessionEnded(function(x) {
    stopApp()
  })

  k_max <-
    AIBS$ScoreRankAdj %>% max(na.rm = TRUE)


  # transform percentage to Ks (check against Ks, not percents)
  n_sel <- reactive({
    round(input$reliability_restricted_proportion * .01 * k_max)
  })

  # ** Update slider input ######
  observe({
    updateSliderInput(
      session,
      inputId = "reliability_restricted_proportion",
      min = ceiling(200 / k_max)
    )
  })


  # this chunk ensures slider is reset to 100% whenever direction or BS samples are changed
  # however, it is rather lenient and causes some issues, e.g. double estimation on BS num change
  # in addition, user request should be fulfilled without any unsolicited actions, such as slider change

  # observeEvent(c(
  #   input$data_toydata,
  #   input$reliability_restricted_clear,
  #   input$reliability_restricted_direction,
  #   25
  # ), {
  #   updateSliderInput(
  #     session,
  #     inputId = "reliability_restricted_proportion",
  #     value = 100
  #   )
  # })

  # ** Caterpillar plot input ######
  reliability_restricted_caterpillarplot_input <- reactive({
    AIBS %>%
      mutate(hl = case_when(
        input$reliability_restricted_direction == "top" & ScoreRankAdj <= n_sel() ~ "sol",
        input$reliability_restricted_direction == "bottom" & ScoreRankAdj > (k_max - n_sel()) ~ "sol",
        TRUE ~ "alp"
      ) %>% factor(levels = c("alp", "sol"))) %>%
      ggplot(aes(x = ScoreRankAdj, y = Score, group = ID, alpha = hl)) +
      geom_line(col = "gray") +
      geom_point(aes(text = paste0(
        "Proposal ID: ", ID, "\n", # TODO generalize
        "Rank: ", ScoreRankAdj, "\n", # TODO generalize
        "Score: ", Score, "\n" # add avg score? not in every dataset, computed only in ICCrestricted
      )), shape = 1, size = 1.5) +
      stat_summary(
        fun = mean, fun.args = list(na.rm = TRUE), geom = "point", col = "red",
        shape = 5, size = 2.5, stroke = .35
      ) +
      scale_alpha_discrete(range = c(.3, 1), drop = FALSE) +
      coord_cartesian(ylim = c(1, 5)) +
      labs(x = "Proposal rank", y = "Overall scientific merit score") +
      theme_app()
  })

  # ** Plotly output for caterpillar plot ######
  output$reliability_restricted_caterpillarplot <- renderPlotly({
    reliability_restricted_caterpillarplot_input() %>%
      ggplotly(tooltip = c("text")) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ** DB for caterpillar plot ######
  output$DB_reliability_restricted_caterpillarplot <- downloadHandler(
    filename = function() {
      "fig_reliability_caterpillar.png"
    },
    content = function(file) {
      ggsave(file,
        plot = reliability_restricted_caterpillarplot_input(), width = unit(7, "in"), height = unit(5, "in"), scale = 1.2
      )
    }
  )


  reliability_restricted_res <- reactiveValues(vals = NULL) # init ICCs bank

  observeEvent(
    c(
      input$reliability_restricted_clear,
      input$reliability_restricted_proportion,
      input$reliability_restricted_direction
      # 25 bootstrap input
    ),
    {
      isolate({
        entries <- reliability_restricted_res$vals %>%
          names()

        # propose a new entry
        new_entry <- paste0(
          "dir-", input$reliability_restricted_direction,
          "_bs-", 25,
          "_sel-", n_sel()
        )

        # check if proposed not already available, else compute
        if (new_entry %in% entries) {
          message("rICC computation: using already computed value...\n")
        } else {
          reliability_restricted_res[["vals"]][[new_entry]] <- ICCrestricted(
            AIBS,
            case = "ID",
            var = "Score",
            rank = "ScoreRankAdj",
            dir = input$reliability_restricted_direction,
            sel = n_sel(),
            nsim = 25
          )
        }
      })
    }
  )

  # ** Clearing ICC computed entries ######
  observeEvent(input$reliability_restricted_clear, {
    cat("Clearing ICC bank...\n")
    reliability_restricted_res$vals <- NULL
  })

  # ** ICC plot - current choice ######
  reliability_restricted_iccplot_curr <- reactive({
    plt_data <- reliability_restricted_res$vals %>%
      bind_rows(.id = "name") %>%
      filter(str_detect(
        .data$name,
        paste0(
          "dir-", input$reliability_restricted_direction,
          "_bs-", 25
        )
      ))

    # translate empty tibble to NULL for further use in req()
    if (nrow(plt_data) == 0) {
      NULL
    } else {
      plt_data
    }
  })

  # ** Reliability plot input ######
  reliability_restricted_iccplot_input <- reactive({
    # req(AIBS) # propagate validation msg to the plot
    req(reliability_restricted_iccplot_curr())

    curr_plt_name <- paste0(
      "dir-", input$reliability_restricted_direction,
      "_bs-", 25,
      "_sel-", n_sel()
    )

    reliability_restricted_iccplot_curr() %>%
      mutate(hl = if_else(name == curr_plt_name, "sol", "alp") %>%
        factor(levels = c("alp", "sol"))) %>%
      ggplot(aes(prop_sel, ICC1, ymin = ICC1_LCI, ymax = ICC1_UCI, alpha = hl)) + # TODO general
      geom_linerange() + # separate as plotly messes up otherwise
      geom_point(aes(text = paste0(
        ifelse(prop_sel == 1, "Complete range",
          paste0(
            "Proportion of ", dir, " proposals: ", scales::percent(prop_sel, .01)
          )
        ), "\n",
        "ICC1: ", round(ICC1, 2), "\n",
        "LCI: ", round(ICC1_LCI, 2), "\n",
        "UCI: ", round(ICC1_UCI, 2)
      ))) +
      scale_x_continuous(labels = scales::percent) +
      scale_alpha_discrete(range = c(.25, 1), drop = FALSE) +
      labs(
        x = paste0("Proportion of ", input$reliability_restricted_direction, " proposals"),
        y = "Inter-rater reliability"
      ) +
      coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) + # TODO general
      theme_app()
  })

  # ** Reliability plot render ######
  output$reliability_restricted_iccplot <- renderPlotly({
    reliability_restricted_iccplot_input() %>%
      ggplotly(tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ** DB for ICC reliability plot ######
  output$DB_reliability_restricted_iccplot <- downloadHandler(
    filename = function() {
      "fig_reliability_resctricted.png"
    },
    content = function(file) {
      ggsave(file,
        plot = reliability_restricted_iccplot_input(),
        width = unit(7, "in"), height = unit(5, "in"), scale = 1.2
      )
    }
  )

  # ** DB for ICC reliability table ######
  output$DB_reliability_restricted_iccdata <- downloadHandler(
    filename = function() {
      "range_restricted_reliability_data.csv"
    },
    content = function(file) {
      data <- reliability_restricted_iccplot_curr() %>% select(-name)
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$icc_text <- renderText({

    # isolate({
    full <- reliability_restricted_res[["vals"]][[paste0(
      "dir-", input$reliability_restricted_direction,
      "_bs-", 25,
      "_sel-", k_max
    )]]
    # })

    curr <- reliability_restricted_res[["vals"]][[paste0(
      "dir-", input$reliability_restricted_direction,
      "_bs-", 25,
      "_sel-", n_sel()
    )]]


    full_part <- if (is.null(full)) {
      "Please, set the slider to 100% in order to estimate and display inter-rater reliability the complete dataset."
    } else {
      paste0(
        "For the complete dataset, the estimated inter-rater reliability is ",
        round(full$ICC1, 2), ", with 95% CI of [", round(full$ICC1_LCI, 2), ", ", round(full$ICC1_UCI, 2), "]."
      )
    }
    curr_part <- if (identical(curr, full) | is.null(curr)) {
      "Set the slider to different value to see how the estimate changes for other subset of the data."
    } else {
      paste0(
        "For the ", round(curr$prop_sel * 100), "%",
        " (that is ", curr$n_sel, ") of ", curr$dir, " proposals,",
        " the estimated inter-rater reliability is ",
        round(curr$ICC1, 2), ", with 95% CI of [", round(curr$ICC1_LCI, 2), ", ", round(curr$ICC1_UCI, 2), "]."
      )
    }

    paste(full_part, curr_part)
  })
}
