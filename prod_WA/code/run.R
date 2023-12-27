
# read data ---------------------------------------------------------------

d <- read.csv(file = 'output/d.csv')


# source auxiliary function -----------------------------------------------

source('code/auxiliary.R')



# predict and validate ----------------------------------------------------

# predict
p <- c()
for (t in seq(1995, 2015, 5)) {
  
  # predict
  p <- rbind(p, pred_t(d, t))
  
}

# add world regions and income classes
p <- add_wr_ic(p)

# validate
res_val <- array(dim = c(10,3,3),
                 dimnames = list(c('total', unique(p$in_cla), unique(p$region)),
                                 c('p_wdde', 'p_pwlf', 'p_base'),
                                 c('RMSE', 'MAE', 'MDA')))

for (m in c('p_wdde', 'p_pwlf', 'p_base')) {
  for (rn in rownames(res_val)) {
    # subset for region / income class
    p_sub <- p[p$region == rn | p$in_cla == rn, ]
    
    # validate
    res_val[rn, m, ] <- validate(p_sub, m)
    
  }
  
  # validate
  res_val['total', m, ] <- validate(p, m)
  
}



# so far: overall pretty similar but pwlf is better in terms of MAE and for high income countries
