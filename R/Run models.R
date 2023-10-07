library(Rceattle)
library(dplyr)

mydata <- read_data(file = "Data/WGSAM3sppEdited.xlsx") # 2 spp and 2 edited work
check <- rearrange_dat(mydata)

mydata$fsh_biom %>%
  group_by(Fleet_name) %>%
  summarise(maxcatch = max(Catch))


# --------------------------------
# Fit model
# --------------------------------
ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            verbose = 1)



ss_run_m <- Rceattle::fit_mod(data_list = mydata,
                            inits = ss_run$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            M1Fun = build_M1(M1_model = c(1,1,1)),
                            phase = "default",
                            verbose = 1)

ss_run_m <- Rceattle::fit_mod(data_list = mydata,
                            inits = ss_run_m$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            M1Fun = build_M1(M1_model = c(1,1,1)),
                            phase = NULL,
                            verbose = 1)
plot_selectivity(ss_run)
plot_comp(ss_run)
plot_biomass(ss_run)

# Reweight
ss_run_reweight <- ss_run_m
for(i in 1:3){
  mydata$fleet_control$Comp_weights <- sapply( ss_run_reweight$data_list$fleet_control$Est_weights_mcallister, function(x) min(c(x,1)))
  ss_run_reweight <- Rceattle::fit_mod(data_list = mydata,
                              inits = ss_run_m$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              M1Fun = build_M1(M1_model = c(0,0,1)),
                              phase = NULL,
                              verbose = 1)
}


# ss_run$quantities$jnll_comp
plot_biomass(ss_run_reweight)
plot_ssb(ss_run_reweight)
plot_recruitment(ss_run_reweight)
plot_index(ss_run_reweight)
plot_catch(ss_run_reweight)
plot_selectivity(ss_run_reweight)
plot_comp(ss_run_reweight)

# Save
library(writexl)
yrs <- mydata$styr:mydata$endyr
nyrs <- length(yrs)
data_list <- list(Recruitment = cbind(data.frame(Year = yrs), t(ss_run$quantities$R[,1:nyrs])),
                  Total_biomass = cbind(data.frame(Year = yrs), t(ss_run$quantities$biomass[,1:nyrs])),
                  SSB = cbind(data.frame(Year = yrs), t(ss_run$quantities$biomassSSB[,1:nyrs])),
                  Ftot = cbind(data.frame(Year = yrs), t(ss_run$quantities$F_spp[,1:nyrs]))
)

write_xlsx(data_list, path = "CEATTLE_initial_run.xlsx")
                  