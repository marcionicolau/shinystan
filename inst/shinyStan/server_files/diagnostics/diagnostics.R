lp_hist <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."))
  chain <- input$diagnostic_chain
  lp <- samps_post_warmup[,,"lp__"]
  df <- as.data.frame(cbind(iterations = 1:nrow(lp), lp))
  .lp_hist(df, chain)
})
lp_trace <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."))
  
  lp <- samps_post_warmup[,,"lp__"]
  df <- as.data.frame(cbind(iterations = (warmup_val+1):(warmup_val+nrow(lp)), lp))
  chain <- input$diagnostic_chain
  .lp_trace(df, chain)
})
accept_stat_hist <- reactive({
  df <- accept_stat_pw()
  chain <- input$diagnostic_chain
  .accept_stat_hist(df, chain)
})
accept_stat_trace <- reactive({
  df <- accept_stat_pw()
  chain <- input$diagnostic_chain
  .accept_stat_trace(df, chain)
}) 
accept_stat_vs_lp <- reactive({
  metrop <- accept_stat_pw()[,-1] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- input$diagnostic_chain
  .sampler_param_vs_lp(lp, sp = metrop, sp_lab = "Mean Metropolis Acceptance", chain = chain)
})
ndivergent_trace <- reactive({
  df <- ndivergent_pw()
  chain <- input$diagnostic_chain
  .ndivergent_trace(df, chain)
})
treedepth_trace <- reactive({
  df <- treedepth_pw()
  chain <- input$diagnostic_chain
  .treedepth_trace(df, chain)
})
treedepth_ndivergent_hist <- reactive({
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  chain <- input$diagnostic_chain
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = "All")
})
treedepth_ndivergent0_hist <- reactive({
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  chain <- input$diagnostic_chain
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 0)
})
treedepth_ndivergent1_hist <- reactive({
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  chain <- input$diagnostic_chain
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 1)
})
treedepth_vs_lp <- reactive({
  treedepth <- treedepth_pw()[,-1] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- input$diagnostic_chain
  .sampler_param_vs_lp(lp, sp = treedepth, sp_lab = "Tree Depth", 
                       chain = chain, violin = TRUE)
})
ndivergent_vs_lp <- reactive({
  ndivergent <- ndivergent_pw()[,-1] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- input$diagnostic_chain
  .sampler_param_vs_lp(lp, sp = ndivergent, sp_lab = "N Divergent", 
                       chain = chain, violin = TRUE)
})
treedepth_vs_accept_stat <- reactive({
  df_td <- treedepth_pw()[,-1] # drop iterations column
  df_as <- accept_stat_pw()[,-1] 
  chain <- input$diagnostic_chain
  .treedepth_vs_accept_stat(df_td, df_as, chain)
})
ndivergent_vs_accept_stat <- reactive({
  df_nd <- ndivergent_pw()[,-1] # drop iterations column
  df_as <- accept_stat_pw()[,-1] 
  chain <- input$diagnostic_chain
  .ndivergent_vs_accept_stat(df_nd, df_as, chain)
})