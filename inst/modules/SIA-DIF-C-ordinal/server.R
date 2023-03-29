function(input, output, session) {
  # kills the local server as the window closes
  session$onSessionEnded(function(x) {
    stopApp()
  })

  # data --------------------------------------------------------------------

  d <- readRDS("data.rds")

  # static group var for all cases
  group <- d[["group"]]

  # total scores are dependent on item-set chosen
  ts_pretest <- reactive({
    # this is going to be DIF matching criterion for all analyses
    d[["total_scores"]][[input$item_set]][["pre"]]
  })

  ts_posttest <- reactive({
    d[["total_scores"]][[input$item_set]][["post"]]
  })

  ts_delayed_posttest <- reactive({
    d[["total_scores"]][[input$item_set]][["del"]]
  })



  ## item sets -------------------------------------------------------------

  items_pretest <- reactive({
    d[["items"]][[input$item_set]][["pre"]]
  })

  items_posttest <- reactive({
    d[["items"]][[input$item_set]][["post"]]
  })

  items_delayed_posttest <- reactive({
    d[["items"]][[input$item_set]][["del"]]
  })


  # summary -----------------------------------------------------------------

  ts_summary("pre", group, ts_pretest)
  ts_summary("post", group, ts_posttest)
  ts_summary("del", group, ts_delayed_posttest)


  # differences in time points -----------------------------------------------

  time_diff("pre_post", ts_pretest, ts_posttest, group)
  time_diff("pre_del", ts_pretest, ts_delayed_posttest, group)
  time_diff("del_post", ts_delayed_posttest, ts_posttest, group)










  # DIF -------------------------------------------------------------------


  # note that the id determines observed score selection and purification-related UI
  dif_server("pretest", items_pretest, group, ts_pretest)
  dif_server("posttest", items_posttest, group, ts_pretest)
  dif_server("delayed_posttest", items_delayed_posttest, group, ts_pretest)
}
