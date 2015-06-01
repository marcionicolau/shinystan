output$ui_diagnostics_sample <- renderUI({
  fluidRow(
    column(7,
           fluidRow(
             column(6, 
                    plotOutput("lp_trace_out", height = "200px"),
                    plotOutput("accept_stat_trace_out", height = "200px")
             ),
             column(6, 
                    plotOutput("lp_hist_out", height = "200px"),
                    plotOutput("accept_stat_hist_out", height = "200px")
             )
           )
    ),
    column(5, plotOutput("accept_stat_vs_lp_out"))
  )
})

output$ui_diagnostics_td_divergent <- renderUI({
  div(
  fluidRow(
    column(4,
           plotOutput("ndivergent_trace_out", height = "150px"),
           plotOutput("treedepth_trace_out", height = "150px")
    ),
    column(4, 
           plotOutput("ndivergent_vs_lp_out", height = "150px"),
           plotOutput("treedepth_vs_lp_out", height = "150px")
           ),
    column(4, 
           plotOutput("ndivergent_vs_accept_stat_out", height = "150px"),
           plotOutput("treedepth_vs_accept_stat_out", height = "150px")
           )
  ),
  splitLayout( 
         plotOutput("treedepth_ndivergent_hist_out", height = "134px"),
         plotOutput("treedepth_ndivergent0_hist_out", height = "133px"),
         plotOutput("treedepth_ndivergent1_hist_out", height = "133px")
  )
)
})

