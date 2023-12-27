# source functions --------------------------------------------------------
install.packages("pacman")
pacman::p_load(readxl, wcde, pwt10, tidyverse, countrycode, stargazer)


# read data ---------------------------------------------------------------

# Penn World Tabel: https://www.rug.nl/ggdc/productivity/pwt/?lang=en
data("pwt10.01")

# Wittgenstein Center population and human capital projections
wdde_raw <- get_wcde(indicator = 'pop', scenario = 2, pop_age = 'total', pop_sex = 'total', pop_edu = "four")

# PWLF
pwlf_raw <- read_csv('input/pwlf.csv')



# process data ------------------------------------------------------------


# PWT

## select relevant variables
pwt <- data.frame(iso3c = pwt10.01$isocode, year = pwt10.01$year, Y = pwt10.01$rgdpo, K =  pwt10.01$cn)

## exclude VEN as recent instability leads to distortions in growth path => extrapolation is more adequate
pwt <- subset(pwt, iso3c != 'VEN')

## assume that 2020 behaves like 2019 (ignore COVID distortions)
tmp <- subset(pwt, year == 2019)
tmp$year <- 2020
pwt <- rbind(pwt, tmp)



# PWLF
pwlf <- pwlf_raw %>%
  mutate(iso3c = countrycode(as.numeric(substr(region, 4, 6)), 'iso3n', 'iso3c')) %>%
  rename(year = Time,
         pop_tot_pwlf = pop_Sum) %>%
  select(!'region')



#WDDE

# kill scenario
wdde_raw$scenario <- NULL

# total population
pop_tot <- wdde_raw %>% 
  
  ## add iso3c country code and working age
  mutate(iso3c = countrycode(country_code, origin = 'iso3n', destination = 'iso3c')) %>%
  
  ## remove observations with unknown country code (e.g. world)
  drop_na() %>%
  
  ## aggregate
  group_by(iso3c, year) %>%
  summarise(pop_tot_wdde = sum(pop))



# some manicure
wdde <- wdde_raw %>% 
  
  # omit observation under 15
  filter(education != "Under 15") %>%
  
  
  ## add iso3c country code and working age
  mutate(iso3c = countrycode(country_code, origin = 'iso3n', destination = 'iso3c')) %>%
         # working_age = fct_collapse(wdde_raw$age,
         #                            "15--34" = c("15--19", "20--24", "25--29", "30--34"),
         #                            "35--64" = c("35--39", "40--44", "50--54", "55--59", "60--64"))) %>%
  
  ## remove observations with unknown country code (e.g. world)
  drop_na() %>%
  
  ## only include working age population and countries that are included in pwt (and exclude inconsistency in the data)
  # filter(working_age %in% c("15--34", "35--64") & education != 'Under 15') %>%
  
  ## aggregate working age groups
  # group_by(scenario, iso3c, year, education, working_age) %>%
  # summarise(pop = sum(pop)) %>%
  
  ## change data structur s.t. keys correspond to i,t and pop variables to L_jk
  pivot_wider( 
    names_from = c(education),
    values_from = pop
  ) %>%
  
  ## rename
  # rename(L_01 = `No Education_15--34`,
  #        L_02 = `No Education_35--64`,
  #        L_11 = `Primary_15--34`,
  #        L_12 = `Primary_35--64`,
  #        L_21 = `Secondary_15--34`,
  #        L_22 = `Secondary_35--64`,
  #        L_31 = `Post Secondary_15--34`,
  #        L_32 = `Post Secondary_35--64`,) %>%
    
  rename(L_0 = `No Education`,
         L_1 = Primary,
         L_2 = Secondary,
         L_3 = `Post Secondary`) %>%
  
  ## add human capital variables (shift by +1 to allow logarithm below)
  # mutate(L_0_ = L_01 + L_02,
  #        L_1_ = L_11 + L_12,
  #        L_2_ = L_21 + L_22,
  #        L_3_ = L_31 + L_32,
  #        L = L_0_ + L_1_ + L_2_ + L_3_) %>%
  
# add human capital variables (shift by +1 to allow logarithm below)
    mutate(L = L_0 + L_1 + L_2 + L_3) %>%
  
  # select relevant variabels
  select(!c(name, country_code)) %>%
  
  ## add total population
  left_join(., pop_tot, by = c('iso3c', 'year'))


# PWT + WDDE + PWLF
d <- left_join(pwt, pwlf, by = c('year', 'iso3c')) %>%
  left_join(., wdde, by = c('year', 'iso3c')) %>%
  # omit missing values
  drop_na()



## add d_{t+1} to d
tmp <- d
tmp$year <- tmp$year - 5
names(tmp)[3:ncol(d)] <- paste(names(tmp)[3:ncol(d)], '_tp1', sep = '')
d <- left_join(d, tmp, by = c('iso3c', 'year'))

## calculate log difference ~ 1 + growth rate =: g where the year indicates the beginning of the respective period
for (i in names(d)[3:as.integer((length(names(d))-2)/2 + 2)]){
  
  d[paste(i, '_g', sep = '')] <- log(d[ , paste(i, '_tp1', sep = '')] + 1) - log(d[ , i] + 1)
  
}


# define variables for estimation
d <- d %>%
  
  ## add new variables
  mutate(Y_o_L = Y/pop_tot_wdde,
         log_Y_o_L = log(Y_o_L + 1),
         L_1_o_L = L_1/L,
         L_2_o_L = L_2/L,
         L_3_o_L = L_3/L,
         Y_o_L_t_L_1_o_L = Y_o_L * L_1_o_L,
         Y_o_L_t_L_2_o_L = Y_o_L * L_2_o_L,
         Y_o_L_t_L_3_o_L = Y_o_L * L_3_o_L,
         log_Y_o_L_t_L_1_o_L = log(Y_o_L) * (L_1_o_L),
         log_Y_o_L_t_L_2_o_L = log(Y_o_L) * (L_2_o_L),
         log_Y_o_L_t_L_3_o_L = log(Y_o_L) * (L_3_o_L),
         wa_weight_o_wa_sum = wa_weight/wa_sum,
         Y_o_L_t_wa_weight_o_wa_sum = Y_o_L * wa_weight_o_wa_sum,
         log_Y_o_L_t_wa_weight_o_wa_sum = log(Y_o_L) * wa_weight_o_wa_sum) %>%
  
  
  ## select relevant variables
  select(c(iso3c, year, Y, L, pop_tot_wdde, pop_tot_wdde_g, Y_g, K_g, L_g, L_1_g, L_2_g, L_3_g, Y_o_L, log_Y_o_L, L_1_o_L, L_2_o_L, L_3_o_L, Y_o_L_t_L_1_o_L, Y_o_L_t_L_2_o_L, Y_o_L_t_L_3_o_L, log_Y_o_L_t_L_1_o_L, log_Y_o_L_t_L_2_o_L, log_Y_o_L_t_L_3_o_L, wa_weight_g, wa_weight_o_wa_sum, Y_o_L_t_wa_weight_o_wa_sum, log_Y_o_L_t_wa_weight_o_wa_sum))





# write data --------------------------------------------------------------

write.csv(d, 'output/d.csv', row.names = F)

