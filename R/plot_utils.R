plot_freq_univariate <- function(data, endogenous, exogenous){
  
  
  data_mean <- data %>% pull({{endogenous}}) %>% mean()
  
  summary_table <- select(data, {{endogenous}}, {{exogenous}}) %>% 
    group_by({{exogenous}}) %>% 
    summarise(n = n(), avg = mean({{endogenous}}))
  max_endo <- summary_table %>% pull(avg) %>% max()
  max_exog <- summary_table %>% pull(n) %>% max()
  coeff <- (max_endo/max_exog)
  colname <- paste('Average', data %>% select({{endogenous}}) %>% colnames())
  
  summary_table %>% 
    ggplot() +
    geom_col(aes(x = {{exogenous}}, y = n*coeff ), fill = "gray") +  
    geom_point(aes(x = {{exogenous}}, y= avg), color ="navyblue") + 
    geom_hline(yintercept = data_mean, linetype = 2L) +
    scale_y_continuous(name = colname, limits = c(0, max_endo*2),
                       sec.axis = sec_axis(~.*(1/coeff), name = "Frequecy"))}
