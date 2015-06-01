.sampler_param_pw <- function(sp, which = "accept_stat__", warmup_val) {
  sp_pw <- lapply(1:length(sp), function(i) {
    out <- sp[[i]][, which]
    if (warmup_val == 0) out else out[-(1:warmup_val)]
  })
  sp_mat <- do.call("cbind", sp_pw)
  colnames(sp_mat) <- paste0("chain:", 1:ncol(sp_mat))
  sp_mat <- cbind(iterations = (warmup_val+1):(warmup_val + nrow(sp_mat)), sp_mat)
  as.data.frame(sp_mat)
}

accept_stat_pw <- function() {
  .sampler_param_pw(sampler_params, which = "accept_stat__", 
                    warmup_val = warmup_val)
}
treedepth_pw <- function() {
  .sampler_param_pw(sampler_params, which = "treedepth__", 
                    warmup_val = warmup_val)
}
ndivergent_pw <- function() {
  .sampler_param_pw(sampler_params, which = "n_divergent__", 
                    warmup_val = warmup_val)
}
stepsize_pw <- function() {
  .sampler_param_pw(sampler_params, which = "stepsize__", 
                    warmup_val = warmup_val)
}

.which_diagnostic <- function(x) {
  if (x == "sample") return("accept_stat__")
  if (x == "treedepth") return("treedepth__")
  if (x == "divergent") return("n_divergent__")
  else return("stepsize__")
}

accept_stat_hist <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."))
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + no_yaxs + transparent)
  df <- accept_stat_pw()
  chain <- input$diagnostic_chain
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  base <- ggplot(mdf, aes(x = value)) + 
    geom_histogram(binwidth = diff(range(mdf$value))/30) + 
    labs(x = "Mean Metropolis Acceptance", y = "") + 
    thm
  
  if (chain == 0) {
    graph <- base + 
      geom_vline(xintercept = mean(mdf$value), color = "gray") + 
      geom_vline(xintercept = median(mdf$value), color = "gray", lty = 2)
    return(graph)
  }
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  base + thm + geom_histogram(data = chain_data,
                              binwidth = diff(range(chain_data$value))/30,
                              fill = "skyblue", alpha = 0.5) + 
  geom_vline(xintercept = mean(chain_data$value), color = "gray") + 
  geom_vline(xintercept = median(chain_data$value), color = "gray", lty = 2)
})

accept_stat_trace <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."),
           need(input$diagnostic_interval, message = "Loading..."))
  chain <- input$diagnostic_chain
  q <- 0.5 * input$diagnostic_interval
  probs <- 0.5 + c(-q, q)
  
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "Mean Metropolis Acceptance")
  
  df <- accept_stat_pw()
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  if (chain == 0) {
    lbub <- quantile(mdf$value, probs)
    graph <- ggplot(mdf, aes(x = iterations, y = value, group = variable)) +
      annotate("rect", 
               xmin = -Inf, 
               xmax = Inf,
               ymin = lbub[1], 
               ymax = lbub[2],
               fill = "skyblue", alpha = 0.5) +
      geom_path(aes(color = variable), size = 0.25) 
    
    return(graph + xy_labs + thm)
  }
  
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  lbub <- quantile(chain_data$value, probs)
  graph <- ggplot(chain_data, aes(x = iterations, y = value)) +
    annotate("rect", 
             xmin = -Inf, 
             xmax = Inf,
             ymin = lbub[1], 
             ymax = lbub[2],
             fill = "skyblue", alpha = 0.5) +
    geom_path(color = "black")
  
  graph + xy_labs + thm
}) 

accept_stat_corr_lp <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."))
  chain <- input$diagnostic_chain
  
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Mean Metropolis Acceptance", y = "Log Posterior")
  
  metrop <- accept_stat_pw()[,-1] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  df <- data.frame(metrop = do.call("c", metrop), lp = c(lp))
  
  base <- ggplot(df, aes(x = metrop, y = lp)) + xy_labs + thm 
  
  if (chain == 0) return(base + geom_point(alpha = 0.5))
  chain_data <- data.frame(metrop = metrop[, chain], lp = lp[, chain])
  base + 
    geom_point(alpha = 0.5) + 
    geom_point(data = chain_data, aes(x = metrop, y = lp), color = "skyblue", alpha = 0.5)
})

lp_hist <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."))
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + no_yaxs + transparent)
  chain <- input$diagnostic_chain
  lp <- samps_post_warmup[,,"lp__"]
  df <- as.data.frame(cbind(iterations = 1:nrow(lp), lp))
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  base <- ggplot(mdf, aes(x = value)) + 
    geom_histogram(binwidth = diff(range(mdf$value))/30) + 
    labs(x = "Log Posterior", y = "") + 
    thm
  
  if (chain == 0) {
    graph <- base + 
      geom_vline(xintercept = mean(mdf$value), color = "gray") + 
      geom_vline(xintercept = median(mdf$value), color = "gray", lty = 2)
    return(graph)
  }
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  base + thm + geom_histogram(data = chain_data,
                              binwidth = diff(range(chain_data$value))/30,
                              fill = "skyblue", alpha = 0.5) +
    geom_vline(xintercept = mean(chain_data$value), color = "gray") + 
    geom_vline(xintercept = median(chain_data$value), color = "gray", lty = 2)
})

lp_trace <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."),
           need(input$diagnostic_interval, message = "Loading..."))
  chain <- input$diagnostic_chain
  q <- 0.5 * input$diagnostic_interval
  probs <- 0.5 + c(-q, q)
  
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "Log Posterior")
  
  lp <- samps_post_warmup[,,"lp__"]
  df <- as.data.frame(cbind(iterations = (warmup_val+1):(warmup_val+nrow(lp)), lp))
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  if (chain == 0) {
    lbub <- quantile(mdf$value, probs)
    graph <- ggplot(mdf, aes(x = iterations, y = value, group = variable)) +
      annotate("rect", 
               xmin = -Inf, 
               xmax = Inf,
               ymin = lbub[1], 
               ymax = lbub[2],
               fill = "skyblue", alpha = 0.5) +
      geom_path(aes(color = variable), size = 0.25) 
    
    return(graph + xy_labs + thm)
  }
  
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  lbub <- quantile(chain_data$value, probs)
  graph <- ggplot(chain_data, aes(x = iterations, y = value)) +
    annotate("rect", 
             xmin = -Inf, 
             xmax = Inf,
             ymin = lbub[1], 
             ymax = lbub[2],
             fill = "skyblue", alpha = 0.5) +
    geom_path(color = "black")
  
  graph + xy_labs + thm
}) 


treedepth_trace <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."),
           need(input$diagnostic_interval, message = "Loading..."))
  chain <- input$diagnostic_chain
  q <- 0.5 * input$diagnostic_interval
  probs <- 0.5 + c(-q, q)
  
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "Tree Depth")
  
  df <- treedepth_pw()
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  if (chain == 0) {
    lbub <- quantile(mdf$value, probs)
    graph <- ggplot(mdf, aes(x = iterations, y = value, group = variable)) +
      annotate("rect", 
               xmin = -Inf, 
               xmax = Inf,
               ymin = lbub[1], 
               ymax = lbub[2],
               fill = "skyblue", alpha = 0.5) +
      geom_path(aes(color = variable), size = 0.25) 
    
    return(graph + xy_labs + thm)
  }
  
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  lbub <- quantile(chain_data$value, probs)
  graph <- ggplot(chain_data, aes(x = iterations, y = value)) +
    annotate("rect", 
             xmin = -Inf, 
             xmax = Inf,
             ymin = lbub[1], 
             ymax = lbub[2],
             fill = "skyblue", alpha = 0.5) +
    geom_path(color = "black")
  
  graph + xy_labs + thm
})
ndivergent_trace <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."),
           need(input$diagnostic_interval, message = "Loading..."))
  chain <- input$diagnostic_chain

  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "Divergent")
  y_scale <- scale_y_continuous(breaks = c(0,1))
  
  df <- ndivergent_pw()
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  base <- ggplot(mdf, aes(x = iterations, xend = iterations,
                           y = 0, yend = value)) +
    xy_labs + 
    y_scale +
    thm
  
  if (chain == 0) return(base + geom_segment(aes(color = variable), size = 0.25))
  
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  base +
    geom_segment(size = 0.25) + 
    geom_segment(data = chain_data, 
                 aes(x = iterations, xend = iterations,
                    y = 0, yend = value), size = 0.5, 
                 color = "skyblue", alpha = 0.5)
}) 




treedepth_ndivergent_hist <- reactive({
  chain <- input$diagnostic_chain
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = "All")
})
treedepth_ndivergent0_hist <- reactive({
  chain <- input$diagnostic_chain
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 0)
})
treedepth_ndivergent1_hist <- reactive({
  chain <- input$diagnostic_chain
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 1)
})