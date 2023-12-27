# predict -----------------------------------------------------------------
pred_t <- function(d, t) {
  
  # split data
  d_pred <- subset(d, year == t)
  ## assumption of costant TFE
  d_pred$year <- t - 5
  d_est <- subset(d, year < t)
  
  # define formulars
  f_wdde <- formula(Y_g ~ log_Y_o_L + L_3_g + L_1_o_L + L_3_o_L + log_Y_o_L_t_L_3_o_L + K_g + pop_tot_wdde_g + factor(iso3c) + factor(year))
  f_pwlf <- formula(Y_g ~ log_Y_o_L + wa_weight_g + wa_weight_o_wa_sum + log_Y_o_L_t_wa_weight_o_wa_sum +  K_g + pop_tot_wdde_g + factor(iso3c) + factor(year))
  f_base <- formula(Y_g ~ log_Y_o_L + K_g + pop_tot_wdde_g + factor(iso3c) + factor(year))
  
  # estimate
  m_wdde <- lm(f_wdde, d_est)
  m_pwlf <- lm(f_pwlf, d_est)
  m_base <- lm(f_base, d_est)
  
  # predict
  out <- data.frame(iso3c = d_pred$iso3c,
                    real = d_pred$Y_g,
                    real_lagged = subset(d_est, year == t - 5)$Y_g,
                    p_wdde = predict.lm(m_wdde, d_pred),
                    p_pwlf = predict.lm(m_pwlf, d_pred),
                    p_base = predict.lm(m_base, d_pred))
  
  # return
  return(out)
  
  
}




# prediction validation --------------------------------------------------------------


validate <- function(pred_t_output, m){
  
  #extract values
  real <- pred_t_output$real
  pred <- pred_t_output[ , m]
  real_lagged <- pred_t_output$real_lagged

  #real direction
  realDirection <- ifelse(real - real_lagged >=0, 1, 0)
    
  #predicted direction
  predDirection <- ifelse(pred - real_lagged >=0, 1, 0)
    
  #mean directional accuracy
  MDA <- sum(realDirection==predDirection)/length(realDirection)
  
  #calculate RMSE
  RMSE <- sqrt((t(real - pred)%*%(real - pred))/length(real))
  
  # MAE
  MAE <- sum(abs(real - pred))/length(real)
  
  # # estimate rationality model
  # rat <- lm(real ~ pred)
  
  #results
  res <- c(RMSE = RMSE, MAE = MAE, MDA = MDA)
  
  # return
  return(res)
  
}




# add world region and income classes -------------------------------------

add_wr_ic <- function(p) {
  
  # load packages
  pacman::p_load(countrycode)
  
  # add world region
  p$region <- countrycode(p$iso3c, origin = 'iso3c', destination = 'continent')
  
  # add income classes
  ic <- readRDS('input/WB_income_classification.RDS')
  p <- merge(p, ic, by = 'iso3c')
  
  # return
  return(p)
  
}



# tmp ---------------------------------------------------------------------

# # tmp
# f_wdde <- formula(Y_g ~ log_Y_o_L + L_1_g + L_2_g + L_3_g + L_1_o_L + L_2_o_L + L_3_o_L + Y_o_L_t_L_1_o_L + Y_o_L_t_L_2_o_L + Y_o_L_t_L_3_o_L + log_Y_o_L_t_L_1_o_L + log_Y_o_L_t_L_2_o_L + log_Y_o_L_t_L_3_o_L +  K_g + pop_tot_wdde_g + factor(iso3c) + factor(year))
# f_pwlf <- formula(Y_g ~ log_Y_o_L + wa_weight_g + wa_weight_o_wa_sum + Y_o_L_t_wa_weight_o_wa_sum + log_Y_o_L_t_wa_weight_o_wa_sum +  K_g + pop_tot_wdde_g + factor(iso3c) + factor(year))
# 
# 
# 
# 
# e_wdde <- lm(f_wdde, d)
# summary(e_wdde)
# e_pwlf <- lm(f_pwlf, d)
# summary(e_pwlf)
# 
# stargazer(e_wdde, e_pwlf, keep = c(1:12))
# 
# 
# 
# e_wdde <- lm(f_wdde, d)
# summary(e_wdde)
# e_pwlf <- lm(f_pwlf, d)
# summary(e_pwlf)
# 
# stargazer(e_wdde, e_pwlf, keep = c(1:12))