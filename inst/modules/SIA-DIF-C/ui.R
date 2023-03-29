ui <- tagList(
  tags$head(
    # favicon
    tags$link(
      rel = "shortcut icon",
      href = "favicon.ico"
    ),

    # syntax highlighting
    tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.1.0/highlight.min.js"),
    tags$link(
      rel = "stylesheet",
      href = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.1.0/styles/stackoverflow-light.min.css"
    ),
    tags$script(
      "hljs.configure({cssSelector: 'pre', languages: ['r']}); hljs.highlightAll();"
    ),

    # math typesetting
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.css",
      integrity = "sha384-RZU/ijkSsFbcmivfdRBQDtwuwVqK7GMOw6IMvKyeWL2K5UAlyp6WonmB8m7Jd0Hn",
      crossorigin = "anonymous"
    ),
    tags$script(
      defer = "defer",
      src = "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.js",
      integrity = "sha384-pK1WpvzWVBQiP0/GjnvRxV4mOb0oxFuyRxJlk6vVw146n3egcN5C925NCP7a7BY8",
      crossorigin = "anonymous"
    ),
    tags$script(
      defer = "defer",
      src = "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/contrib/auto-render.min.js",
      integrity = "sha384-vZTG03m+2yp6N6BNi5iM4rW4oIwk5DfcNdFfxkk9ZWpDriOkXX8voJBFrAO7MpVl",
      crossorigin = "anonymous",
      onload = "renderMathInElement(document.body);"
    ),

    # custom math typesetting for dynamic content
    includeScript("www/katex_dynamic.js"),

    # custom CSS
    includeCSS("www/custom.css")
  ),
  navbarPage(
    windowTitle = "DIF-C with LtL Module",
    title = tags$div(
      class = "d-flex align-items-center",
      tags$div(
        class = "pr-3",
        tags$img(src = "sia_logo_trans.svg", height = "60px")
      ),
      tags$div(
        style = "line-height: 25px;",
        tags$b("ShinyItemAnalysis Modules", style = "font-size: x-large;"),
        br(),
        tags$i("Differential Item Functioning in Change (DIF-C)")
      )
    ),
    position = "fixed-top", # fixed header
    collapsible = TRUE,
    # custom footer
    footer = tags$footer(
      class = "row fixed-bottom bg-light p-3",
      tags$div(
        class = "col ml-1 align-self-center",
        style = "-ms-flex: 0 0 0px; flex: 0 0 0px;",
        tags$img(src = "sia_logo.svg", height = "43px")
      ),
      tags$div(
        class = "col pl-0 text-muted align-self-center",
        tags$span(
          "Powered by",
          tags$a("ShinyItemAnalysis: Test and Item Analysis via Shiny",
            href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/"
          ),
          paste0(
            "(version ",
            as.character(packageVersion(
              "ShinyItemAnalysis"
            )), ")"
          ),
          class = "d-block"
        ),
        tags$span("©", format(Sys.Date(), "%Y"), "ShinyItemAnalysis"),
        style = "line-height: 16pt;"
      )
    ),
    theme = bs_theme(bootswatch = "cerulean"),
    selected = "", # select the tab with empty title (the main content)
    tabPanel(
      tags$a(
        style = "white-space: nowrap;",
        icon("home"), "Go to the main ShinyItemAnalysis app",
        href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/",
        target = "_blank"
      )
    ),
    tabPanel(
      "",

      # general description -------------------------------------------------------------

      # hr(class = "mb-5"),
      h3("Differential Item Functioning in Change (DIF-C)"),
      p(
        "This ", code("ShinyItemAnalysis"), " module provides interactive display of
        Differential Item Functioning in Change (DIF-C) with binary
        Learning to Learn data, as more closesly described in ",
        a(
          "Martinková, Hladká, and Potužníková (2020)",
          href = "http://doi.org/10.1016/j.learninstruc.2019.101286",
          target = "_blank",
          .noWS = "after"
        ),
        ". In their paper, Martinková et al. demonstrate that this more detailed
        item-level analysis is able to detect between-group differences in
        pre-post gains even in case when no difference is observable in gains in
        total scores. DIF analysis is implemented with generalized logistic
        regression models in the ", code("difNLR"), " package ",
        a(
          "(Hladká & Martinková, 2020)",
          href = "http://doi.org/10.32614/RJ-2020-014",
          target = "_blank",
          .noWS = "after"
        ),
        ". This module is a part of the ", code("ShinyItemAnalysis"), " package ",
        a(
          "(Martinková & Drabinová, 2018).",
          href = "https://doi.org/10.32614/RJ-2018-074",
          target = "_blank",
          .noWS = "after"
        ),
      ),


      # tabs --------------------------------------------------------------------

      tabsetPanel(
        tabPanel(
          "Total scores",
          h4("Summary of total scores"),
          p(
            "DIF analysis may come to a different conclusion than a test of group differences in total scores.
                        Two groups may have the same distribution of total scores, yet, some items may function differently
                        for the two groups. Also, one of the groups may have a significantly lower total score, yet, it may
                        happen that there is no DIF item ",
            a("(Martinková et al., 2017)",
              href = "https://doi.org/10.1187/cbe.16-10-0307",
              target = "_blank", .noWS = "after"
            ),
            ". This section examines the differences in observed scores only. Explore further DIF sections to analyze
                        differential item functioning."
          ),

          # total scores module (see global.R)
          h5("Grade 6"),
          h6("Summary of total scores for groups"),
          total_scores_summary_tab_iu("total_scores_summary_tab_dm"),
          h6("Comparison of total scores"),
          total_scores_t_test_iu("total_scores_t_test_dm"),
          h6("Histograms of total scores for groups"),
          total_scores_hist_iu("total_scores_hist_dm"),
          h5("Grade 9"),
          h6("Summary of total scores for groups"),
          total_scores_summary_tab_iu("total_scores_summary_tab_ts"),
          h6("Comparison of total scores"),
          total_scores_t_test_iu("total_scores_t_test_ts"),
          h6("Histograms of total scores for groups"),
          total_scores_hist_iu("total_scores_hist_ts")
        ),
        tabPanel(
          "DIF-C Summary",
          br(),
          p(
            "In DIF analysis, the groups are compared in functioning of items with respect to respondent ability.
                        In many methods, observed ability such as the standardized total score is used as the matching criterion.
                        DIF can also be explored with respect to other observed score or criterion.
                        For example, to analyze instructional sensitivity, ",
            a("Martinková et al. (2020)",
              href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank"
            ),
            " analyzed differential item functioning in change (DIF-C) by analyzing DIF on Grade 9 item answers
                          while matching on Grade 6 total scores of the same respondents in a longitudinal setting
                          (see toy data ", code("Learning to Learn 9"), " in the Data section)."
          ),
          h4("Method specification"),
          p(
            "Here you can specify the assumed",
            strong("model", .noWS = "after"),
            ". In 3PL and 4PL models, the abbreviations \\(c_{g}\\) or
          \\(d_{g}\\) mean that parameters \\(c_i\\) or \\(d_i\\) are assumed
          to be the same for both groups, otherwise they are allowed to
          differ. With", strong("type"), "you can specify the type of DIF to
          be tested by choosing the parameters in which a difference between
          groups should be tested. You can also select", strong("correction
          method"), "for multiple comparison or",
            strong("item purification", .noWS = "after"), "."
          ),
          p(
            "Finally, you may change the",
            strong("Observed score", .noWS = "after"),
            ". While matching on the standardized total score is typical, the
          upload of other Observed scores is possible in the ",
            strong("Data"), "section. Using a pre-test (standardized) total
          score allows for testing differential item functioning in change
          (DIF-C) to provide proofs of instructional sensitivity",
            a(
              "(Martinková et al., 2020)",
              href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank",
              .noWS = "after"
            ),
            ", also see", code("Learning To Learn 9"), " toy dataset."
          ),
          fluidRow(
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_model_print",
                label = "Model",
                choices = c(
                  "Rasch" = "Rasch",
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PLcg" = "3PLcg",
                  "3PLdg" = "3PLdg",
                  "3PLc" = "3PLc",
                  "3PLd" = "3PLd",
                  "4PLcgdg" = "4PLcgdg",
                  "4PLcgd" = "4PLcgd",
                  "4PLcdg" = "4PLcdg",
                  "4PL" = "4PL"
                ),
                selected = "3PLcg"
              )
            ),
            column(
              1,
              checkboxGroupInput(
                inputId = "DIF_NLR_type_print",
                label = "Type",
                choices = c(
                  "\\(a\\)" = "a",
                  "\\(b\\)" = "b",
                  "\\(c\\)" = "c",
                  "\\(d\\)" = "d"
                ),
                selected = c("\\(a\\)", "b")
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_correction_method_print",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput(
                inputId = "DIF_NLR_purification_print",
                label = "Item purification",
                value = FALSE
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_summary_matching",
                label = "Observed score",
                choices = c("Standardized total score" = "zscore"),
                selected = "zscore"
              )
            )
          ),
          h4("Equation"),
          p("The displayed equation is based on the model selected below"),
          fluidRow(
            column(12, uiOutput("DIF_NLR_equation_print"), align = "center")
          ),
          h4("Summary table"),
          p(
            "This summary table contains information about DIF test statistic
          \\(LR(\\chi^2)\\), corresponding \\(p\\)-values considering selected
          adjustement, and significance codes. This table also provides
          estimated parameters for the best fitted model for each item. Note
          that \\(a_{iG_p}\\) (and also other parameters) from the equation
          above consists of a parameter for the reference group and a
          parameter for the difference between focal and reference groups,
          i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\), where \\(G_{p} = 0\\)
          for the reference group and \\(G_{p} = 1\\) for the focal group, as
          stated in the table below. "
          ),
          uiOutput("DIF_NLR_na_alert"),
          strong(textOutput("nlr_dif_items")),
          br(),
          tags$head(tags$style("#coef_nlr_dif  {white-space: nowrap;}")),
          fluidRow(column(12, align = "left", DTOutput("coef_nlr_dif", width = "100%"))),
          fluidRow(column(12, align = "left", uiOutput("note_nlr"))),
          br(),
          fluidRow(column(
            2,
            downloadButton("download_nlr_dif", "Download table")
          )),
          br(),
          h4("Purification process"),
          textOutput("dif_nlr_puri_info"),
          br(),
          tags$head(tags$style(
            "#dif_nlr_puri_table  {white-space: nowrap;}"
          )),
          fluidRow(column(
            12,
            align = "center", DTOutput("dif_nlr_puri_table")
          )),
          conditionalPanel(
            "input.DIF_NLR_purification_print == 1",
            downloadButton("download_nlr_dif_puri", "Download table"),
            class = "mb-5"
          )
        ),
        tabPanel(
          "DIF-C Items",
          h4("Method specification"),
          p(
            "Here you can specify the assumed", strong("model", .noWS = "after"),
            ". In 3PL and 4PL models, the abbreviations \\(c_{g}\\) or
          \\(d_{g}\\) mean that parameters \\(c\\) or \\(d\\) are assumed to
          be the same for both groups, otherwise they are allowed to differ.
          With", strong("type"), "you can specify the type of DIF to be tested
          by choosing the parameters in which a difference between groups
          should be tested. You can also select", strong("correction method"),
            "for multiple comparison or",
            strong("item purification", .noWS = "after"), "."
          ),
          p(
            "Finally, you may change the",
            strong("Observed score", .noWS = "after"),
            ". While matching on the standardized total score is typical, the
          upload of other observed scores is possible in the", strong("Data"),
            "section. Using a pre-test (standardized) total score allows for
          testing differential item functioning in change (DIF-C) to provide
          proofs of instructional sensitivity",
            a(
              "(Martinková et al., 2020)",
              href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank",
              .noWS = "after"
            ),
            ", also see",
            code("Learning To Learn 9"),
            "toy dataset. For selected", strong("item"), "you can display plot
          of its characteristic curves and table of its estimated parameters
          with standard errors."
          ),
          fluidRow(
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_model_plot",
                label = "Model",
                choices = c(
                  "Rasch" = "Rasch",
                  "1PL" = "1PL",
                  "2PL" = "2PL",
                  "3PLcg" = "3PLcg",
                  "3PLdg" = "3PLdg",
                  "3PLc" = "3PLc",
                  "3PLd" = "3PLd",
                  "4PLcgdg" = "4PLcgdg",
                  "4PLcgd" = "4PLcgd",
                  "4PLcdg" = "4PLcdg",
                  "4PL" = "4PL"
                ),
                selected = "3PLcg"
              )
            ),
            column(
              1,
              checkboxGroupInput(
                inputId = "DIF_NLR_type_plot",
                label = "Type",
                choices = c(
                  "\\(a\\)" = "a",
                  "\\(b\\)" = "b",
                  "\\(c\\)" = "c",
                  "\\(d\\)" = "d"
                ),
                selected = c("a", "b")
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_correction_method_plot",
                label = "Correction method",
                choices = c(
                  "Benjamini-Hochberg" = "BH",
                  "Benjamini-Yekutieli" = "BY",
                  "Bonferroni" = "bonferroni",
                  "Holm" = "holm",
                  "Hochberg" = "hochberg",
                  "Hommel" = "hommel",
                  "None" = "none"
                ),
                selected = "none"
              ),
              checkboxInput(
                inputId = "DIF_NLR_purification_plot",
                label = "Item purification",
                value = FALSE
              )
            ),
            column(
              2,
              selectInput(
                inputId = "DIF_NLR_items_matching",
                label = "Observed score",
                choices = c("Standardized total score" = "zscore"),
                selected = "zscore"
              )
            ),
            column(
              2,
              sliderInput(
                inputId = "DIF_NLR_item_plot",
                label = "Item",
                min = 1,
                value = 1,
                max = 10,
                step = 1,
                animate = animationOptions(interval = 1600)
              )
            )
          ),
          h4("Plot with estimated DIF generalized logistic curve"),
          p(
            "Points represent a proportion of the correct answer (empirical
          probabilities) with respect to the observed score. Their size is
          determined by the count of respondents who achieved a given level of
          observed score with respect to the group membership."
          ),
          plotlyOutput("plot_DIF_NLR"),
          downloadButton("DP_plot_DIF_NLR", "Download figure"),
          h4("Equation"),
          fluidRow(
            column(12, uiOutput("DIF_NLR_equation_plot"), align = "center")
          ),
          h4("Table of parameters"),
          p(
            "This table summarizes estimated item parameters together with their
          standard errors. Note that \\(a_{iG_p}\\) (and also other
          parameters) from the equation above consists of a parameter for the
          reference group and a parameter for the difference between focal and
          reference groups, i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\),
          where \\(G_{p} = 0\\) for the reference group and \\(G_{p} = 1\\)
          for the focal group, as stated in the table below. "
          ),
          fluidRow(
            column(12, tableOutput("tab_coef_DIF_NLR"), align = "center")
          )
        )
      ),


      # sample R code -----------------------------------------------------------

      h4("Selected R code"),
      pre(includeText("sc/difcLtL.R"), class = "mb-4 language-r"),

      # references --------------------------------------------------------
      h4("References"),
      HTML('<ul class = "biblio">
              <li>Martinková, P., Drabinová, A., & Potužníková, E. (2020).
              Is academic tracking related to gains in learning competence?
              Using propensity score matching and differential item change functioning analysis for better understanding
              of tracking implications.
              <i>Learning and Instruction 66</i>(April).
              <a href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank">doi:10.1016/j.learninstruc.2019.101286</a>
              </li>

              <li>Hladká A., & Martinková, P. (2020).
              difNLR: Generalized logistic regression models for DIF and DDF detection.
              <i>The R Journal, 12</i>(1), 300-323.
              <a href = "https://doi.org/10.32614/RJ-2020-014", target = "_blank">doi:10.32614/RJ-2020-014.</a>
              </li>

              <li>Martinková, P., & Drabinová, A. (2018).
              ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.
              <i>The R Journal, 10</i>(2), 503-515.
              <a href = "https://doi.org/10.32614/RJ-2018-074", target = "_blank">doi:10.32614/RJ-2018-074</a>
              </li>

              <li>Martinková, P., Drabinová, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M. (2017).
              Checking equity: Why differential item functioning analysis should be a routine part
              of developing conceptual Assessments.
              <i>CBE-Life Sciences Education, 16</i>(2), rm2.
              <a href = "https://doi.org/10.1187/cbe.16-10-0307",
              target = "_blank">doi:10.1187/cbe.16-10-0307</a>
              </li>

              <li>Kolek, L., Šisler, V., Martinková, P., & Brom, C. (2021).
              Can video games change attitudes towards history? Results from a laboratory experiment measuring short- and long-term effects.
              <i>Journal of Computer Assisted Learning, 37</i>(5), 1348-1369.
              <a href = "https://doi.org/10.1111/jcal.12575", target = "_blank">doi:10.1111/jcal.12575</a>
              </li>

            </ul>'),

      # acknowledgements --------------------------------------------------------

      h4("Acknowledgements"),
      p(
        "ShinyItemAnalysis Modules are developed by the",
        a(
          "Computational Psychometrics Group",
          href = "http://www.cs.cas.cz/comps/",
          target = "_blank"
        ),
        "supported by the Czech Science Foundation under Grant Number",
        a(
          "21-03658S",
          href = "http://www.cs.cas.cz/comps/projectTheorFoundComPs.html",
          target = "_blank",
          .noWS = "after"
        ),
        "."
      )
    )
  )
)
