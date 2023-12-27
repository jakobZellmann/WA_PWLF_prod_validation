
# read data ---------------------------------------------------------------

d <- read.csv(file = 'output/d.csv')


# source auxiliary function -----------------------------------------------

source('code/auxiliary.R')



# predict and validate ----------------------------------------------------

# predict and validate x 1000
tmp <- c()
out <- c()

# iterate over models 
for (m in c('p_wdde', 'p_pwlf', 'p_base')){
  
  # rep CV
  for (i in 1:1000) {
    
    # predict
    p <- pred(d)
    # validate
    v <- validate(p, m)
    # store
    tmp <- rbind(tmp, v)
    
  }
  
  # estimated expected error
  out <- rbind(out, colMeans(tmp))
  
} 

# names
rownames(out) <- c('p_wdde', 'p_pwlf', 'p_base')
  

# latex output ------------------------------------------------------------

stargazer::stargazer(out)

