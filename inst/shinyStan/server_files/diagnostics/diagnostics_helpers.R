# post-warmup sampler parameters ------------------------------------------
sp_nuts_check <- reactive({
  validate(
    need(sampler_params[[1]] != "Not Stan", message = "Only available for Stan models"),
    need(stan_algorithm == "NUTS", message = "Only available for algorithm = NUTS"),
    need(input$diagnostic_chain, message = "Loading...")
  )
})

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


accept_stat_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params, which = "accept_stat__", 
                    warmup_val = warmup_val)
})
treedepth_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params, which = "treedepth__", 
                    warmup_val = warmup_val)
})
ndivergent_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params, which = "n_divergent__", 
                    warmup_val = warmup_val)
})
stepsize_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params, which = "stepsize__", 
                    warmup_val = warmup_val)
})

.sampler_param_vs_lp <- function(lp, sp, sp_lab, chain = 0, violin = FALSE) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(y = "Log Posterior", x = if (missing(sp_lab)) NULL else sp_lab)
  df <- data.frame(sp = do.call("c", sp), lp = c(lp))
  
  if (violin) df$sp <- as.factor(df$sp)
  
  base <- ggplot(df, aes(sp,lp)) + xy_labs + thm 
  
  if (chain == 0) {
    graph <- base + if (violin) geom_violin(fill = "black") else geom_point(alpha = 0.5)
    return(graph)
  }
  chain_data <- data.frame(sp = sp[, chain], lp = lp[, chain])
  if (violin) {
    chain_data$sp <- as.factor(chain_data$sp)
    graph <- base + 
      geom_violin(fill = "black") + 
      geom_violin(data = chain_data, aes(sp, lp), color = "skyblue", fill = "skyblue", alpha = 0.5)
    return(graph)
  }
  
  base + 
    geom_point(alpha = 0.5) + 
    geom_point(data = chain_data, aes(sp,lp), color = "skyblue", alpha = 0.5)
}

.lp_hist <- function(df, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + no_yaxs + transparent)
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
}

.lp_trace <- function(df, chain = 0) {

  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "Log Posterior")
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  if (chain == 0) {
    xbar <- mean(mdf$value, na.rm = TRUE)
    sigma <- sd(mdf$value, na.rm = TRUE)
    graph <- ggplot(mdf, aes(x = iterations, y = value, group = variable)) +
      annotate("rect", 
               xmin = -Inf, 
               xmax = Inf,
               ymin = xbar - sigma, 
               ymax = xbar + sigma,
               fill = "skyblue", alpha = 0.5) +
      geom_path(aes(color = variable), size = 0.25) 
    
    return(graph + xy_labs + thm)
  }
  
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  xbar <- mean(chain_data$value, na.rm = TRUE)
  sigma <- sd(chain_data$value, na.rm = TRUE)
  graph <- ggplot(chain_data, aes(x = iterations, y = value)) +
    annotate("rect", 
             xmin = -Inf, 
             xmax = Inf,
             ymin = xbar - sigma, 
             ymax = xbar + sigma,
             fill = "skyblue", alpha = 0.5) +
    geom_path(color = "black")
  
  graph + xy_labs + thm
}

.accept_stat_hist <- function(df, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + no_yaxs + transparent)
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  base <- ggplot(mdf, aes(x = value)) + 
    geom_histogram(binwidth = diff(range(mdf$value))/30) + 
    labs(x = "Mean Metropolis Acceptance", y = "") + 
    xlim(0,1) +
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
}

.accept_stat_trace <- function(df, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "Mean Metropolis Acceptance")
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  if (chain == 0) {
    xbar <- mean(mdf$value, na.rm = TRUE)
    sigma <- sd(mdf$value, na.rm = TRUE)
    graph <- ggplot(mdf, aes(x = iterations, y = value, group = variable)) +
      annotate("rect", 
               xmin = -Inf, 
               xmax = Inf,
               ymin = xbar - sigma,
               ymax = xbar + sigma,
               fill = "skyblue", alpha = 0.5) +
      geom_path(aes(color = variable), size = 0.25) 
    
    return(graph + xy_labs + thm)
  }
  
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  xbar <- mean(chain_data$value, na.rm = TRUE)
  sigma <- sd(chain_data$value, na.rm = TRUE)
  graph <- ggplot(chain_data, aes(x = iterations, y = value)) +
    annotate("rect", 
             xmin = -Inf, 
             xmax = Inf,
             ymin = xbar - sigma,
             ymax = xbar + sigma,
             fill = "skyblue", alpha = 0.5) +
    geom_path(color = "black")
  
  graph + xy_labs + thm
}

.accept_stat_corr_lp <- function(metrop, lp, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Mean Metropolis Acceptance", y = "Log Posterior")
  df <- data.frame(metrop = do.call("c", metrop), lp = c(lp))
  
  base <- ggplot(df, aes(metrop,lp)) + xy_labs + thm 
  
  if (chain == 0) return(base + geom_point(alpha = 0.5))
  chain_data <- data.frame(metrop = metrop[, chain], lp = lp[, chain])
  base + 
    geom_point(alpha = 0.5) + 
    geom_point(data = chain_data, aes(metrop,lp), color = "skyblue", alpha = 0.5)
}


.treedepth_ndivergent_hist <- function(df_td, df_nd, chain = 0, divergent = c("All", 0, 1)) {
  plot_title <- theme(plot.title = element_text(size = 11, hjust = 0))
  plot_theme <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + plot_title + lgnd_right + transparent) 
  x_lab <- if (divergent == "All") "Treedepth (All)" else paste0("Treedepth (N Divergent = ", divergent,")")
  plot_labs <- labs(x = x_lab, y = "") 
  
  mdf_td <- reshape2::melt(df_td, id.vars = "iterations")
  mdf_nd <- reshape2::melt(df_nd, id.vars = "iterations")
  mdf <- cbind(mdf_td, div = mdf_nd$value)
  
  
  plot_data <- if (divergent == "All") mdf else subset(mdf, div == divergent)
  if (nrow(plot_data) == 0) return(NULL)
  graph <- ggplot(plot_data, aes(x = factor(value)), na.rm = TRUE) + 
    stat_bin(aes(y=..count../sum(..count..)), width=1) + 
    plot_labs + 
    plot_theme

  if (chain == 0) return(graph)
  
  chain_data <- subset(plot_data, variable == paste0("chain:",chain))
  graph + stat_bin(data = chain_data, aes(y=..count../sum(..count..)), fill = "skyblue", alpha = 0.5, width = 1)
}

.ndivergent_trace <- function(df, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "N Divergent")
  y_scale <- scale_y_continuous(breaks = c(0,1))
  
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
                 color = "skyblue", alpha = 0.85)
}

.treedepth_trace <- function(df, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(x = "Iteration", y = "Tree Depth")
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  if (chain == 0) {
    xbar <- mean(mdf$value, na.rm = TRUE)
    sigma <- sd(mdf$value, na.rm = TRUE)

    graph <- ggplot(mdf, aes(x = iterations, y = value, group = variable)) +
      annotate("rect", 
               xmin = -Inf, 
               xmax = Inf,
               ymin = xbar - sigma, 
               ymax = xbar + sigma,
               fill = "skyblue", alpha = 0.5) +
      geom_path(aes(color = variable), size = 0.25) 
    
    return(graph + xy_labs + thm)
  }
  
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  xbar <- mean(chain_data$value, na.rm = TRUE)
  sigma <- sd(chain_data$value, na.rm = TRUE)
  graph <- ggplot(chain_data, aes(x = iterations, y = value)) +
    annotate("rect", 
             xmin = -Inf, 
             xmax = Inf,
             ymin = xbar - sigma, 
             ymax = xbar + sigma,
             fill = "skyblue", alpha = 0.5) +
    geom_path(color = "black")
  
  graph + xy_labs + thm
}

.treedepth_vs_accept_stat <- function(df_td, df_as, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(y = "Mean Metrop. Acceptance", x = "Treedepth")
  df <- data.frame(td = do.call("c", df_td), as = do.call("c", df_as))
  df$td <- as.factor(df$td)
  
  base <- ggplot(df, aes(td,as)) + xy_labs + thm 
  
  if (chain == 0) {
    graph <- base + geom_violin() 
    return(graph)
  }
  chain_data <- data.frame(td = as.factor(df_td[, chain]), as = df_as[, chain])
  base + 
    geom_violin(fill = "black")  + 
    geom_violin(data = chain_data, aes(td,as), color = "skyblue", fill = "skyblue", alpha = 0.5)
}

.ndivergent_vs_accept_stat <- function(df_nd, df_as, chain = 0) {
  thm <- theme_classic() %+replace% (no_lgnd + axis_color + axis_labs + fat_axis + transparent)
  xy_labs <- labs(y = "Mean Metrop. Acceptance", x = "N Divergent")
  df <- data.frame(nd = do.call("c", df_nd), as = do.call("c", df_as))
  df$nd <- as.factor(df$nd)
  
  base <- ggplot(df, aes(nd,as)) + xy_labs + thm 
  
  if (chain == 0) {
    graph <- base + geom_violin() 
    return(graph)
  }
  chain_data <- data.frame(nd = as.factor(df_nd[, chain]), as = df_as[, chain])
  base + 
    geom_violin(fill = "black")  + 
    geom_violin(data = chain_data, aes(nd,as), color = "skyblue", fill = "skyblue", alpha = 0.5)
}
