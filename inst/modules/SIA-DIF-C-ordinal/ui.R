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
    includeCSS("www/custom.css"),
    useShinyjs(),
  ),
  navbarPage(
    windowTitle = "DIF-C ordinal Module",
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
        tags$i("DIF-C with ordinal items")
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

      h3("Differential Item Functioning in Change (DIF-C) with ordinal items"),
      p(
        "This ", code("ShinyItemAnalysis"), " module provides interactive display of
        Differential Item Functioning in Change (DIF-C) analysis in ordinal items.
        We use Micro and Macro measurements of attitudes towards the expulsion
        of Sudeten Germans after WWII, as more closesly described in ",
        a(
          "Kolek, Šisler, Martinková, and Brom (2021)",
          href = "https://doi.org/10.1111/jcal.12575",
          target = "_blank",
          .noWS = "after"
        ),
        ". DIF-C analysis was first described for binary items in ",
        a(
          "Martinková, Hladká, and Potužníková (2020)",
          href = "http://doi.org/10.1016/j.learninstruc.2019.101286",
          target = "_blank",
          .noWS = "after"
        ),
        ", demonstrating that this more detailed item-level analysis is able
        to detect between-group differences in pre-post gains even in case when
        no difference is observable in gains in total scores. DIF analysis is
        implemented with generalized logistic regression models in the ",
        code("difNLR"), " package ",
        a(
          "(Hladká & Martinková, 2020)",
          href = "http://doi.org/10.32614/RJ-2020-014",
          target = "_blank",
          .noWS = "after"
        ),
        ". The module is part of the ", code("ShinyItemAnalysis"), " package ",
        a(
          "(Martinková & Drabinová, 2018).",
          href = "https://doi.org/10.32614/RJ-2018-074",
          target = "_blank",
          .noWS = "after"
        ),
      ),


      # item set selection ------------------------------------------------------

      fluidRow(column(
        3,
        selectInput("item_set", "Chose the set of items", c(
          "Micro", "Macro"
        ))
      )),

      # tabs --------------------------------------------------------------------

      tabsetPanel(
        selected = "DIF-C for Pre-Post",


        # scores summary ----------------------------------------------------------

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
            ". This section examines the between-group differences in total scores only.
        Further sections are devoted to DIF and DIF-C analysis."
          ),
          h4("The two groups on Pretest"),
          p("We first examine the two groups (experimental and control) on Pretest.
            No between-group differences were expected on Pretest."),
          ts_summary_ui("pre"),
          # ts_summary_ui("post"),
          # ts_summary_ui("del"),

          h4("Group differences between testing sessions"),
          p(
            "We now examine the change in attitudes towards expulsion in the two
             groups, and the between-group differences in this change. We expected
             the experimental group being more affected by the game, both in
             short-term (Pretest - Posttest) and in the long term (Pretest - Delayed
             Posttest, while the change was expected to remain long-term (no
             difference was expected between Posttest - Delayed Posttes. Note that
             in their study,  ",
            a(
              "Kolek et al. (2021)",
              href = "https://doi.org/10.1111/jcal.12575",
              target = "_blank",
              .noWS = "after"
            ),
            " complement the t tests displayed below also by more complex mixed-effect
            regression models taking into account respondent characteristics."
          ),
          time_diff_ui("pre_post"),
          time_diff_ui("pre_del"),
          time_diff_ui("del_post"),
          h4("Selected R code"),
          pre(includeText("sc/summary.R"), class = "mb-4 language-r")
        ),


        # DIF -----------------------------------------------------------

        tabPanel(
          "DIF in Pretest",
          tagList(
            p(
              "In Pretest, Kolek et al. (2021) assumed the items will function similarly
      for the experimental and the control group. As expected, no DIF was
      confirmed in Pretest.",
              class = "mt-3"
            ),
            dif_ui("pretest", lab = "Pretest")
          )
        ),
        tabPanel(
          "DIF-C for Pre-Post",
          tagList(
            p(
              "In Posttest, Kolek et al. (2021) assumed some but not necessarily all
      the items will function differentially for respondents in the experimental
      and the control group with the same pretest score. The DIF-C analysis revealed
      that Item 4 in Macro, and Item 10 in Micro measurement functioned
      differenially.",
              class = "mt-3"
            ), dif_ui("posttest", lab = "Posttest"),
            h4("Selected R code"),
            pre(includeText("sc/pre_post.R"), class = "mb-4 language-r")
          )
        ),
        tabPanel(
          "DIF-C for Pre-Del",
          tagList(
            p(
              "In Delayed Posttest, Kolek et al. (2021) assumed some but not
              necessarily all the items will function differentially for
              respondents in the experimental and the control group with the
              same pretest score. The DIF-C analysis revealed that items 6 and
              10 in Micro measurement functioned differenially.",
              class = "mt-3"
            ),
            dif_ui("delayed_posttest", lab = "Delayed posttest")
          )
        )
      ),


      # references --------------------------------------------------------
      h4("References"),
      HTML('<ul class = "biblio">
              <li>Kolek, L., Šisler, V., Martinková, P., & Brom, C. (2021).
              Can video games change attitudes towards history? Results from a laboratory experiment measuring short- and long-term effects.
              <i>Journal of Computer Assisted Learning, 37</i>(5), 1348-1369.
              <a href = "https://doi.org/10.1111/jcal.12575", target = "_blank">doi:10.1111/jcal.12575</a>
              </li>

              <li>Martinková, P., Drabinová, A., & Potužníková, E. (2020).
              Is academic tracking related to gains in learning competence?
              Using propensity score matching and differential item change functioning analysis for better understanding
              of tracking implications.
              <i>Learning and Instruction 66</i>(April).
              <a href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank">doi:10.1016/j.learninstruc.2019.101286</a>
              </li>

              <li>Hladká, A., & Martinková, P. (2020).
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
      ),
    )
  )
)
