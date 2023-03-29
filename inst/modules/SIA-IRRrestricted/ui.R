ui <- tagList(
  tags$head(
    tags$link(
      rel = "shortcut icon",
      href = "favicon.ico"
    ),
    tags$link(
      rel = "stylesheet",
      href = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.1.0/styles/stackoverflow-light.min.css"
    ),
    tags$script(
      src = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.1.0/highlight.min.js"
    ),
    tags$script("hljs.configure({cssSelector: 'pre', languages: ['r']}); hljs.highlightAll();"),
    # body padding on different @media widths
    tags$link(
      rel = "stylesheet",
      href = "custom.css"
    ),
    tags$link(
      rel = "stylesheet",
      href = "headroom.css"
    ),
    tags$script(src = "headroom.js"),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/headroom/0.12.0/headroom.min.js",
      integrity = "sha512-9UsrKTYzS9smDm2E58MLs0ACtOki+UC4bBq4iK5wi7lJycwqcaiHxr1bdEsIVoK0l5STEzLUdYyDdFQ5OEjczw==",
      crossorigin = "anonymous", referrerpolicy = "no-referrer"
    ),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/headroom/0.12.0/jQuery.headroom.js",
      integrity = "sha512-pmwEYLNG99yaAFqU2lDdLO/34xv4lQSHo+STfaRqxY57FeIBKvQv72A1F3xMYNphxxUwO+jnnYiEDdqpT4dKfQ==",
      crossorigin = "anonymous", referrerpolicy = "no-referrer"
    )
  ),
  fluidPage(
    withTags(
      tagList(
        title("Range-restricted Reliability Module"),
        nav(
          class = "navbar navbar-default navbar-fixed-top",
          role = "navigation",
          div(
            class = "container-fluid",
            div(
              class = "navbar-header",
              span(
                class = "navbar-brand",
                div(
                  class = "row",
                  div(
                    class = "col-sm-2 align-self-center",
                    img(
                      src = "sia_logo_trans.svg",
                      height = "55px"
                    )
                  ),
                  div(
                    class = "col-sm-10 align-self-center",
                    style = "line-height: 18pt;",
                    b(
                      style = "font-size: x-large;",
                      "ShinyItemAnalysis Modules"
                    ),
                    br(),
                    i("Range-restricted Reliability")
                  )
                )
              )
            ),
            ul(
              class = "nav navbar-nav",
              "data-tabsetid" = "8647",
              li(
                class = "nav-item",
                a(
                  href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/",
                  target = "_blank",
                  class = "nav-link",
                  i(
                    class = "fa fa-home",
                    role = "presentation",
                    "aria-label" = "home icon"
                  ),
                  "Go to the main ShinyItemAnalysis app",
                  class = "nav-link"
                )
              )
            )
          )
        ),
        footer(
          class = "row fixed-bottom bg-light p-3",
          div(
            class = "col ml-1 align-self-center",
            style = "-ms-flex: 0 0 0px; flex: 0 0 0px;",
            img(src = "sia_logo.svg", height = "43px")
          ),
          div(
            class = "col pl-0 text-muted align-self-center",
            span("Powered by", a("ShinyItemAnalysis: Test and Item Analysis via Shiny",
              href = "https://shiny.cs.cas.cz/ShinyItemAnalysis/"
            ), paste0(
              "(version ",
              as.character(packageVersion("ShinyItemAnalysis")), ")"
            ),
            class = "d-block"
            ),
            span(
              "©", format(Sys.Date(), "%Y"), "ShinyItemAnalysis"
            ),
            style = "line-height: 16pt;"
          )
        )
      )
    ),
    theme = bs_theme(bootswatch = "cerulean"),

    # description -------------------------------------------------------------

    p(
      "This module illustrates the issue of range-restricted reliability and
      the difficulties with maximum likelihood estimation, described in more
      detail in the context of inter-rater reliability in grant proposal review
      in",
      a(
        "Erosheva, Martinkova, & Lee (2021)",
        href = "http://doi.org/10.1111/rssa.12681",
        target = "_blank", .noWS = "after"
      ), ". We use their AIBS grant proposal peer-review dataset for presentation."
    ),
    p(
      "Below, you may select the ratio and type of range restriction given by the",
      strong("proportion of rated proposals", .noWS = "after"), ".",
      "In contexts other than grant review, the subject/object of interest may be different:
      a student in educational assessment, a job application
      in hiring, a patient in a medical study, etc. Further, you may select the",
      strong("direction"), "of restriction (top or bottom). The left plot illustrates
      the variability in ratings for the whole dataset outshading the data which
      would be lost due to range-restriction. The right plot provides the estimates
      of the calculated inter-rater reliability estimates, defined by intraclass
      corelation in the simplest model including the ratee effect only. The estimates
      are accompanied by a bootstrapped 95% confidence interval based on 25 bootstrap samples.",
      class = "mb-5"
    ),


    # UI ----------------------------------------------------------------------

    fluidRow(
      column(
        4, sliderInput(
          inputId = "reliability_restricted_proportion",
          label = "Proportion",
          min = 0,
          max = 100,
          step = 1,
          value = 100,
          animate = animationOptions(2000),
          post = "%", width = "100%"
        )
      ),
      column(
        2, selectInput(
          inputId = "reliability_restricted_direction",
          label = "Direction",
          choices = c("top", "bottom"),
          selected = "top"
        )
      ),
      column(
        2, actionButton(
          inputId = "reliability_restricted_clear",
          label = "Clear everything",
          icon = icon("eraser")
        ),
        class = "align-self-center pb-4"
      )
    ),
    fluidRow(
      column(6, plotlyOutput("reliability_restricted_caterpillarplot")),
      column(6, plotlyOutput("reliability_restricted_iccplot")),
      style = "padding-bottom: 20px;"
    ),


    # plots -------------------------------------------------------------------

    fluidRow(
      class = "mb-4", # margin after the row
      column(
        6,
        downloadButton("DB_reliability_restricted_caterpillarplot",
          label = "Download figure"
        )
      ),
      column(
        6,
        downloadButton("DB_reliability_restricted_iccplot",
          label = "Download figure"
        ),
        downloadButton("DB_reliability_restricted_iccdata",
          label = "Download data"
        )
      )
    ),


    # interpretation ----------------------------------------------------------

    h4("Interpretation"),
    fluidRow(column(
      12,
      textOutput("icc_text"),
    ), class = "mb-4"),


    # sample R code -----------------------------------------------------------

    h4("Selected R code"),
    tags$pre(includeText("sc/restr_range.R"), class = "language-r mb-4")
  ),

  # references --------------------------------------------------------
  h4("References"),
  HTML('<ul class = "biblio">
              <li>Erosheva, E., Martinkova, P., & Lee, C. (2021).
               When zero may not be zero: A cautionary note on the use of inter-rater
               reliability in evaluating grant peer review. Journal of the Royal Statistical Society — Series A,
               184(3), 904-919.
              <a href = "https://doi.org/10.1111/rssa.12681", target = "_blank">doi:10.1111/rssa.12681</a>
              </li>

              <li>Martinkova, P., & Drabinova, A. (2018).
              ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.
              The R Journal, 10(2), 503-515.
              <a href = "https://doi.org/10.32614/RJ-2018-074", target = "_blank">doi:10.32614/RJ-2018-074</a>
              </li>
            </ul>'),

  # acknowledgements -------------------------------------------------------------
  h4("Acknowledgements"),
  p(
    "ShinyItemAnalysis Modules are developed by the ",
    a(
      "Computational Psychometrics Group",
      href = "http://www.cs.cas.cz/comps/",
      target = "_blank", .noWS = "after"
    ), " supported by the Czech Science Foundation under Grant Number ",
    a(
      "21-03658S",
      href = "http://www.cs.cas.cz/comps/projectTheorFoundComPs.html",
      target = "_blank", .noWS = "after"
    ), " awarded to Patricia Martinkova. Elena Erosheva and Carole Lee's
    contributions to the development of this module and
    the data collection were further supported by the National Science
    Foundation under Grant Number 1759825.", br(), "Disclaimer:
    Any opinions, findings, and conclusions or recommendations expressed in this
    material are those of the author(s) and do not necessarily reflect the views
    of the National Science Foundation. "
  )
)
