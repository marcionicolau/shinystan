output$ui_diagnostics_sample <- renderUI({
  fluidRow(
    column(7,
           fluidRow(
             column(6, 
                    plotOutput("lp_trace_out", height = "150px"),
                    plotOutput("accept_stat_trace_out", height = "150px")
             ),
             column(6, 
                    plotOutput("lp_hist_out", height = "150px"),
                    plotOutput("accept_stat_hist_out", height = "150px")
             )
           )
    ),
    column(5, plotOutput("accept_stat_vs_lp_out", height = "300px"))
  )
})

output$ui_diagnostics_ndivergent <- renderUI({
  fluidRow(
    column(7,
           plotOutput("ndivergent_trace_out", height = "150px"),
           plotOutput("ndivergent_vs_lp_out", height = "150px")
           
    ),
    column(5, 
           plotOutput("ndivergent_vs_accept_stat_out", height = "300px")
    )
  )
})
output$ui_diagnostics_treedepth <- renderUI({
  div(
    fluidRow(
      column(7,
             plotOutput("treedepth_trace_out", height = "150px"),
             plotOutput("treedepth_vs_lp_out", height = "150px")
      ),
      column(5, plotOutput("treedepth_vs_accept_stat_out", height = "300px"))
    ),
    br(),br(),
    splitLayout( 
      plotOutput("treedepth_ndivergent_hist_out", height = "125px"),
      plotOutput("treedepth_ndivergent0_hist_out", height = "125px"),
      plotOutput("treedepth_ndivergent1_hist_out", height = "125px")
    )
  )
})


