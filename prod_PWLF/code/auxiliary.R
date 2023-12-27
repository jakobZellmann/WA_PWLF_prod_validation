# predict -----------------------------------------------------------------
pred <- function(d) {
  
  # subset 2020
  d <- d[d$year == 2015,]
  
  # split sample in test and train data
  i <- sample(1:nrow(d), as.integer(nrow(d)/10)) 
  d_train <- d[setdiff(1:nrow(d), i),]
  d_test <- d[setdiff(1:nrow(d), i),]
  
  # define formulas
  f_wdde <- formula(Y_g ~ log_Y_o_L + L_3_g + L_1_o_L + L_3_o_L + log_Y_o_L_t_L_3_o_L + K_g + pop_tot_wdde_g)
  f_pwlf <- formula(Y_g ~ log_Y_o_L + PWLF_g + PWLF_o_LF + log_Y_o_L_t_PWLF_o_LF  +  K_g + pop_tot_wdde_g)
  f_base <- formula(Y_g ~ log_Y_o_L + K_g + pop_tot_wdde_g)
  
  # estimate
  m_wdde <- lm(f_wdde, d_train)
  m_pwlf <- lm(f_pwlf, d_train)
  m_base <- lm(f_base, d_train)
  
  # predict
  out <- data.frame(iso3c = d_test$iso3c,
                    real = d_test$Y_g,
                    p_wdde = predict.lm(m_wdde, d_test),
                    p_pwlf = predict.lm(m_pwlf, d_test),
                    p_base = predict.lm(m_base, d_test))
  
  # return
  return(out)
  
  
}



# prediction validation --------------------------------------------------------------


validate <- function(pred_t_output, m){
  
  #extract values
  real <- pred_t_output$real
  pred <- pred_t_output[ , m]
  
  #calculate RMSE
  RMSE <- sqrt((t(real - pred)%*%(real - pred))/length(real))
  
  # MAE
  MAE <- sum(abs(real - pred))/length(real)
  
  #results
  res <- c(RMSE = RMSE, MAE = MAE)
  
  # return
  return(res)
  
}