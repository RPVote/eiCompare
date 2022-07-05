library(eiCompare)

library(eiCompare)
library(eiPack)
library(tidyverse)

source("/mnt/sda7/Git/georgia_demography_destiny/code/my_ei_md_bayes.R", echo=TRUE)

ga_data16 = read_csv("/mnt/sda7/Git/georgia_demography_destiny/data/ga_data_16.csv")
ga_data20 = read_csv("/mnt/sda7/Git/georgia_demography_destiny/data/ga_data_20.csv")

his_data = ga_data16 %>% mutate(NonHispanic = totvot - Hispanic-Unknown,
                                Oth = totvot - Dem - Rep,
                                Hispanic = Hispanic/totvot,
                                NonHispanic = NonHispanic/totvot,
                                Unknown = 1 - Hispanic - NonHispanic,
                                Dem = Dem/totvot,
                                Rep = Rep/totvot,
                                Oth = 1 - Dem - Rep) %>% select(Dem, Rep, Oth, totvot, Hispanic, Unknown, NonHispanic)


cand_cols = c("Dem", "Rep", "Oth")
race_cols = c("NonHispanic", "Hispanic", "Unknown")
totals_col = "totvot"

form = as.formula(cbind(Dem, Rep, Oth) ~ cbind(Hispanic, NonHispanic, Unknown))

his_std =
  stdize_votes_all(his_data,
                   totals_from = "cand",
                   race_cols = race_cols,
                   cand_cols = cand_cols) %>% 
  left_join(his_data)

tune.list = eiPack::tuneMD(
  formula = form,
  data = his_data,
  total = "totvot",
  covariate = NULL, 
  ntunes = 10, 
  totaldraws = 10000)

{
  covariate = NULL
  total = "totvot"
  data=his_std 
  lambda1 = 4
  lambda2 = 2
  covariate.prior.list = NULL
  tune.list = tune.list 
  start.list = NULL 
  sample = 50000 
  totaldraws = 10000
  thin = 10 
  burnin = 20000 
  ntunes=10000
  verbose = 0 
  ret.beta = 'd' 
  ret.mcmc = FALSE 
  usrfun = NULL 
}

cand_cols = c("Dem", "Rep", "Oth")
race_cols = c("NonHispanic", "Hispanic", "Unknown")
totals_col = "totvot"

his_output = ei_rxc(
  data=his_std,
  cand_cols = c("Dem", "Rep", "Oth"),
  race_cols= c("NonHispanic", "Hispanic", "Unknown"),
  totals_col = "totvot",
  name = paste0("Bayes Multinomial Dirichlet: ",yr),
  ntunes = ntunes,
  totaldraws = totaldraws,
  samples = sample,
  thin = thin,
  burnin = burnin,
  ci_size = 0.90,
  seed = 1203,
  eiCompare_class = TRUE,
  ret_mcmc = FALSE,
  verbose = FALSE,
  diagnostic = FALSE,
  n_chains = n_chains,
  plot_path = "",
  par_compute = TRUE)

saveRDS(his_output, file = "/mnt/sda7/Git/georgia_demography_destiny/viz/his_output_2016.RDS")


his_output = readRDS(file = "/mnt/sda7/Git/georgia_demography_destiny/viz/his_output_2016.RDS")
