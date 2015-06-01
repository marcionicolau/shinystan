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
    column(5, plotOutput("accept_stat_corr_lp_out"))
  )
})

output$ui_diagnostics_td_divergent <- renderUI({
  div(
    splitLayout(
      # plotOutput("sampler_plot_divergent_out", height = "200px"),
      plotOutput("ndivergent_trace_out", height = "200px"),
      plotOutput("treedepth_trace_out", height = "200px")
    ),
    splitLayout(
      plotOutput("treedepth_ndivergent_hist_out", height = "150px"),
      plotOutput("treedepth_ndivergent0_hist_out", height = "150px"),
      plotOutput("treedepth_ndivergent1_hist_out", height = "150px")
    )
  )
})

.treedepth_ndivergent_hist <- function(df_td, df_nd, chain = 0, divergent = c("All", 0, 1)) {
  plot_title <- theme(plot.title = element_text(size = 11, hjust = 0))
  plot_labs <- labs(x = "Treedepth", y = "") 
  plot_theme <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + plot_title + lgnd_right + transparent) 
  
  mdf_td <- reshape2::melt(df_td, id.vars = "iterations")
  mdf_nd <- reshape2::melt(df_nd, id.vars = "iterations")
  mdf <- cbind(mdf_td, div = mdf_nd$value)
  
  
  plot_data <- if (divergent == "All") mdf else subset(mdf, div == divergent)
  if (nrow(plot_data) == 0) return(NULL)
  graph <- ggplot(plot_data, aes(x = factor(value)), na.rm = TRUE) + 
    stat_bin(aes(y=..count../sum(..count..)), fill = "gray35", 
             color = "black", width=1) + 
    plot_labs + 
    plot_theme
  
  graph <- graph + 
    if (divergent == 0) ggtitle("n_divergent = 0") 
    else if (divergent == 1) ggtitle("n_divergent = 1") 
    else ggtitle("All")
  
  if (chain == 0) return(graph)
  
  chain_data <- subset(plot_data, variable == paste0("chain:",chain))
  graph + stat_bin(data = chain_data, aes(y=..count../sum(..count..)), fill = "skyblue", alpha = 0.5, width = 1)
}

