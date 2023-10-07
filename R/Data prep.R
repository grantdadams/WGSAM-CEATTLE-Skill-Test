# Set up packages
# install.packages("pacman")
pacman::p_load(remotes, tidyr, dplyr, data.table)

# remotes::install_github("NOAA-EDAB/ms-keyrun", ref = "sarah_wgsamsim")
library(mskeyrun)
# remotes::install_github("grantdadams/Rceattle")
library(Rceattle)


# Load Rceattle data-set
data("BS2017SS")

WGSAM <- BS2017SS
# write_data(WGSAM, file = "WGSAM.xlsx")

# --------------------------------
# Remove Long_rough_dab and Polar_cod because they have no fishery
# --------------------------------
# Select species
species <- c("Capelin", "Haddock", "North_atl_cod")

# Cod, capelin, haddock
simFocalSpecies <- simFocalSpecies %>% 
  as.data.frame() %>% 
  filter(Name %in% species)

simCatchIndex <- simCatchIndex %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simSurveyIndex <- simSurveyIndex %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simSurveyAgecomp <- simSurveyAgecomp %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simSurveyLencomp <- simSurveyLencomp %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simSurveyAgeLencomp <- simSurveyAgeLencomp %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simSurveyWtatAge <- simSurveyWtatAge %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simFisheryAgecomp <- simFisheryAgecomp %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simFisheryLencomp <- simFisheryLencomp %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simFisheryWtatAge<- simFisheryWtatAge %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simBiolPar <- simBiolPar %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simSurveyDietcomp <- simSurveyDietcomp %>%
  as.data.frame() %>% 
  filter(Name %in% species)

simPerCapCons <- simPerCapCons %>%
  as.data.frame() %>% 
  filter(Name %in% species)


# --------------------------------
# Set up control sheet ----
# --------------------------------
WGSAM$nspp <- length(simFocalSpecies$Name)
WGSAM$spnames <- sort(simFocalSpecies$Name)
WGSAM$styr <- min(simCatchIndex$year)
WGSAM$endyr <- max(simCatchIndex$year)
WGSAM$projyr <- WGSAM$endyr + 20
WGSAM$nsex <- rep(1, WGSAM$nspp) # Assuming single-sex models
WGSAM$spawn_month <- rep(0, WGSAM$nspp) # Assuming spawn at new-year
WGSAM$R_sexr <- rep(0.5, WGSAM$nspp)
WGSAM$sex_ratio_sigma <- rep(0.5, WGSAM$nspp)

# - Ages (take min and max across survey and fishery)
fsh_ages <- simFisheryWtatAge %>% group_by(Name) %>% summarise(fshmaxage = max(age), fshminage = min(age))
srv_ages <- simSurveyWtatAge %>% group_by(Name) %>% summarise(srvmaxage = max(age), srvminage = min(age))
ages <- merge(fsh_ages, srv_ages, all = TRUE) %>%
  group_by(Name) %>%
  mutate(minage = min(c(srvminage, fshmaxage), na.rm = TRUE),
         maxage = max(c(srvmaxage, fshmaxage), na.rm = TRUE))

WGSAM$nages <- ages$maxage
WGSAM$minage <- ages$minage

# - Lengths (take n-unique across survey and fishery)
fsh_lengths<- simFisheryLencomp %>% group_by(Name) %>% 
  summarise(nfshlengths = length(unique(lenbin)),
            maxfshlength = max(lenbin),
            minfshlength = min(lenbin))

srv_lengths <- simSurveyLencomp %>% group_by(Name) %>% 
  summarise(nsrvlengths = length(unique(lenbin)),
            maxsrvlength = max(lenbin),
            minsrvlength = min(lenbin))

srv_agelengths <- simSurveyAgeLencomp %>% group_by(Name) %>% 
  summarise(nsrvlengths = length(unique(lenbin)),
            maxsrvagelength = max(lenbin),
            minsrvagelength = min(lenbin))

lengths <- merge(srv_lengths, fsh_lengths, all = TRUE) %>%
  group_by(Name) %>%
  mutate(nlengths = max(c(maxsrvlength, maxfshlength), na.rm = TRUE))

WGSAM$nlengths <- lengths$nlengths - min(lengths[,2:ncol(lengths)])+1
WGSAM$sigma_rec_prior <- rep(1, WGSAM$nspp)
WGSAM$other_food <- rep(1e6, WGSAM$nspp)
WGSAM$estDynamics <- rep(0, WGSAM$nspp) # Estimate everything
WGSAM$proj_F <- rep(0, WGSAM$nspp) # Not used
WGSAM$est_M1 <- rep(0, WGSAM$nspp)
WGSAM$est_sex_ratio <- rep(0, WGSAM$nspp) # Not used


spp_inds <- data.frame(Name = sort(simFocalSpecies$Name), Species = 1:length(simFocalSpecies$Name), maxage = WGSAM$nages)

# --------------------------------
# Fleet control ----
# --------------------------------
surveys <- simSurveyIndex %>% 
  as.data.frame() %>% 
  distinct(Name, survey)
surveys$Fleet_type <- 2
surveys <- surveys %>%
  rename(Fleet_name = survey) # Do spring and fall surveys have same catchability?

fishingfleets <- simCatchIndex %>% 
  as.data.frame() %>% 
  distinct(Name, fishery)
fishingfleets$Fleet_type <- 1
fishingfleets <- fishingfleets %>%
  rename(Fleet_name = fishery)

# - Age-first selected 
frst_fish_age <- simFisheryWtatAge %>% 
  as.data.frame() %>% 
  group_by(Name, fishery) %>% 
  summarise(Age_first_selected = min(age)) %>%
  rename(Fleet_name = fishery)
frst_srv_age <- simSurveyWtatAge %>% 
  as.data.frame() %>% 
  group_by(Name, survey) %>% 
  summarise(Age_first_selected = min(age)) %>%
  rename(Fleet_name = survey)
first_selected_age <- rbind(frst_srv_age, frst_fish_age)

# - build
fleet_control <- rbind(fishingfleets, surveys)
fleet_control <- merge(fleet_control, spp_inds, all = TRUE)
# fleet_control <- merge(fleet_control, first_selected_age, all = TRUE)

fleet_control <- fleet_control %>%
  as.data.frame() %>% 
  mutate(Fleet_code = 1:n(),
         Selectivity_index = 1:n(), # Selectivity the same for spring and fall surveys
         Selectivity = 2, # All non-parametric
         Nselages = 8,
         Time_varying_sel = 12.5,
         Sel_sd_prior = 20,
         Age_first_selected = 1,
         Accumatation_age_lower = 1,
         Accumatation_age_upper = maxage,
         Weight1_Numbers2 = 1,
         Weight_index = 1:n(), # Unique weight for season/fleet
         Age_transition_index = 1:n(),
         Q_index = 1:n(), # Q same for spring and fall surveys
         Estimate_q = 3, # Free parameter (analytical)
         Q_prior = 1,
         Q_sd_prior = 0,
         Time_varying_q = 0,
         Time_varying_q_sd_prior = NA,
         Estimate_survey_sd = 0,
         Survey_sd_prior = NA,
         Estimate_catch_sd = 0,
         Catch_sd_prior = NA,
         Comp_weights = 1,
         proj_F_prop = 1 # One fishing fleet per species
  )

# Set q/srv bits to NA for fishery
fleet_control[which(fleet_control$Fleet_type == 1),
              c("Q_index", "Estimate_q", "Q_prior", "Q_sd_prior", "Time_varying_q", "Time_varying_q_sd_prior", "Estimate_survey_sd","Survey_sd_prior")] <- NA

# # Set selectivity index for surveys to spp + 11 (survey has same selectivity across seasons)
# fleet_control[which(fleet_control$Fleet_type == 2), "Selectivity_index"] <- fleet_control[which(fleet_control$Fleet_type == 2), "Selectivity_index"] + WGSAM$nspp

# Set fishery bits to NA for survey
fleet_control[which(fleet_control$Fleet_type == 2),
              c("Estimate_catch_sd", "Catch_sd_prior", "proj_F_prop")] <- NA

fleet_control <- fleet_control[,c("Name", colnames(WGSAM$fleet_control))]


# --------------------------------
# Survey biomass ----
# --------------------------------
biomass <- simSurveyIndex %>% 
  as.data.frame() %>% 
  filter(variable == "biomass")

biomasscv <- simSurveyIndex %>%
  as.data.frame() %>% 
  filter(variable == "cv")

srv_biom <- data.frame(
  Name = biomass$Name,
  Fleet_name = biomass$survey,
  Year = biomass$year,
  Month = ifelse(substr(biomass$survey, 5, 8) == "fall", 10, 5), # Double check spring and fall survey months?
  Selectivity_block = 1,
  Q_block = 1,
  Observation = biomass$value, # Is this metric tons?
  Log_sd = biomasscv$value # May want to do analytical SD
)

# Need species and fleetcode
species_fleet_crosswalk <- fleet_control %>%
  select(Name, Fleet_name, Fleet_code, Species)

species_fleet_crosswalk_fishery <- species_fleet_crosswalk %>%
  mutate(Fleet_name = ifelse(Fleet_name  == "allfleet", "census", Fleet_name))

srv_biom <- merge(srv_biom, species_fleet_crosswalk, all.x = TRUE)
srv_biom <- srv_biom[,c("Name", colnames(WGSAM$srv_biom))]


# --------------------------------
# Fishery catch ----
# --------------------------------
catch <- simCatchIndex %>%
  as.data.frame() %>% 
  filter(variable == "catch")

catchcv <- simCatchIndex %>%
  as.data.frame() %>% 
  filter(variable == "cv")

fsh_biom <- data.frame(
  Name = catch$Name,
  Fleet_name = catch$fishery,
  Year = catch$year,
  Month = 0, # Double check spring and fall survey months?
  Selectivity_block = 1,
  Catch = catch$value, # Is this metric tons?
  Log_sd = 0.05 # May want to do analytical SD (default is 0.01, loosening it up)
)

fsh_biom <- merge(fsh_biom, species_fleet_crosswalk, all.x = TRUE)
fsh_biom <- fsh_biom[,c("Name", colnames(WGSAM$fsh_biom))]


# --------------------------------
# Set up comp data ----
# --------------------------------
# - Fishery age
simFisheryAgecomp <- pivot_wider(simFisheryAgecomp, names_from = "age") %>% 
  as.data.frame()
comp_data_fsh_age <- data.frame(
  Name = simFisheryAgecomp$Name,
  Fleet_name = simFisheryAgecomp$fishery,
  Sex = 0,
  Age0_Length1 = 0,
  Year = simFisheryAgecomp$year,
  Month = 0, 
  Sample_size = 200) # Set as default (using number of samples was a lot...)

comp_temp <- simFisheryAgecomp[,8:ncol(simFisheryAgecomp)]
comp_temp <- comp_temp[,order(as.numeric(colnames(comp_temp)))]
comp_temp[is.na(comp_temp)] <- 0
# colnames(comp_temp) <- paste0("Comp_", 1:ncol(comp_temp))

comp_data_fsh_age <- cbind(comp_data_fsh_age, comp_temp)
comp_data_fsh_age <- merge(comp_data_fsh_age, species_fleet_crosswalk_fishery, all.x = TRUE)

# - survey age
simSurveyAgecomp <- pivot_wider(simSurveyAgecomp, names_from = "age") %>% 
  as.data.frame()
comp_data_srv_age <- data.frame(
  Name = simSurveyAgecomp$Name,
  Fleet_name = simSurveyAgecomp$survey,
  Sex = 0,
  Age0_Length1 = 0,
  Year = simSurveyAgecomp$year,
  Month = ifelse(substr(simSurveyAgecomp$survey, 5, 8) == "fall", 10, 5), 
  Sample_size = 200) # Set as default (using number of samples was a lot...)
# rowSums(simSurveyAgecomp[,8:ncol(simSurveyAgecomp)], na.rm = TRUE))

comp_temp <- simSurveyAgecomp[,8:ncol(simSurveyAgecomp)]
comp_temp <- comp_temp[,order(as.numeric(colnames(comp_temp)))]
comp_temp[is.na(comp_temp)] <- 0
# colnames(comp_temp) <- paste0("Comp_", 1:ncol(comp_temp))

comp_data_srv_age <- cbind(comp_data_srv_age, comp_temp)
comp_data_srv_age <- merge(comp_data_srv_age, species_fleet_crosswalk_fishery, all.x = TRUE)


# 
# # - Fishery length
# simFisheryLencomp <- pivot_wider(simFisheryLencomp, names_from = "lenbin") %>% as.data.frame()
# comp_data_fsh_len <- data.frame(
#   Name = simFisheryLencomp$Name,
#   Fleet_name = simFisheryLencomp$fishery,
#   Sex = 0,
#   Age0_Length1 = 1,
#   Year = simFisheryLencomp$year,
#   Month = 0, 
#   Sample_size = rowSums(simFisheryLencomp[,8:ncol(simFisheryLencomp)], na.rm = TRUE))
# comp_data_fsh_len <- merge(comp_data_fsh_len, species_fleet_crosswalk, all.x = TRUE)
# 
# comp_temp <- simFisheryLencomp[,8:ncol(simFisheryLencomp)]
# comp_temp <- comp_temp[,order(as.numeric(colnames(comp_temp)))]
# comp_temp[is.na(comp_temp)] <- 0
# # colnames(comp_temp) <- paste0("Comp_", 1:ncol(comp_temp))
# 
# comp_data_fsh_len <- cbind(comp_data_fsh_len, comp_temp)
# 
# 
# # - Survey length
# simSurveyLencomp <- pivot_wider(simSurveyLencomp, names_from = "lenbin") %>% 
#   as.data.frame()
# 
# comp_data_srv_len <- data.frame(
#   Name = simSurveyLencomp$Name,
#   Fleet_name = simSurveyLencomp$survey,
#   Sex = 0,
#   Age0_Length1 = 1,
#   Year = simSurveyLencomp$year,
#   Month = ifelse(substr(simSurveyLencomp$survey, 5, 8) == "fall", 10, 5), 
#   Sample_size = rowSums(simSurveyLencomp[,8:ncol(simSurveyLencomp)], na.rm = TRUE))
# comp_data_srv_len <- merge(comp_data_srv_len, species_fleet_crosswalk, all.x = TRUE)
# 
# comp_temp <- simSurveyLencomp[,8:ncol(simSurveyLencomp)]
# comp_temp <- comp_temp[,order(as.numeric(colnames(comp_temp)))]
# comp_temp[is.na(comp_temp)] <- 0
# # colnames(comp_temp) <- paste0("Comp_", 1:ncol(comp_temp))
# 
# comp_data_srv_len <- cbind(comp_data_srv_len, comp_temp)

# - Combine
age_comp <- rbind(comp_data_fsh_age, comp_data_srv_age)
age_comp <- age_comp[,c("Name","Fleet_name","Fleet_code", "Species", "Sex", "Age0_Length1", "Year", "Month", "Sample_size", 1:max(WGSAM$nages))]
colnames(age_comp)[10:ncol(age_comp)] <- paste0("Comp_", 1:length(10:ncol(age_comp)))
# 
# len_comp <- bind_rows(comp_data_srv_len, comp_data_fsh_len)
# len_comp[is.na(len_comp)] <- 0 # Min length-bin is 3, max length-bin is 199
# colnames(len_comp)[10:ncol(len_comp)] <- paste0("Comp_", 1:length(10:ncol(len_comp)))

comp_data <- age_comp # bind_rows(age_comp, len_comp)
# comp_data <- comp_data[,c("Name", colnames(WGSAM$comp_data), paste0("Comp_",26:197))]

# Remove comp data with 0s
comp_data <- comp_data[which(rowSums(comp_data[,10:ncol(comp_data)]) != 0),]

comp_data <- comp_data %>%
  select(Name, Fleet_name, Fleet_code, Species, Sex, Age0_Length1, Year, Month, Sample_size, paste0("Comp_",1:max(WGSAM$nages)))


# --------------------------------
# Empirical selectivity ----
# --------------------------------
emp_sel <- WGSAM$emp_sel
emp_sel <- emp_sel[-c(1:20),] 


# --------------------------------
# Age transition matrix ----
# --------------------------------
#FIXME adjust for agecl to age converesion?
agelengthdat <- simSurveyAgeLencomp %>%
  as.data.frame() %>% 
  mutate(lenbin = paste0("Length",lenbin - min(lenbin) + 1)) %>% 
  pivot_wider(names_from = "lenbin") %>% 
  as.data.frame()

agelengthdat <- agelengthdat %>%
  as.data.frame() %>% 
  select(ModSim, year, Code,  Name, survey, agecl, variable, units, paste0("Length",sort(unique(simSurveyAgeLencomp$lenbin))-min(simSurveyAgeLencomp$lenbin) + 1))

agelen_names <- agelengthdat %>%
  as.data.frame() %>% 
  distinct(Name, survey) %>%
  mutate(Age_transition_index = 1:n())

age_trans_list <- list()
for(i in 1:nrow(agelen_names)){
  
  # Annual
  agelencomp_temp <- agelengthdat %>%
    as.data.frame() %>% 
    filter(Name == agelen_names$Name[i] & survey == agelen_names$survey[i])
  agecl_df <- data.frame(agecl = sort(unique(agelencomp_temp$agecl)))
  
  # Convert annual to average across time-span
  yrs <- unique(agelencomp_temp$year)
  agelencomp_temp_list <- list()
  for(yr in 1:length(yrs)){
    agelencomp_temp_list[[yr]] <- agelencomp_temp %>%
      filter(year == yrs[yr])
    agelencomp_temp_list[[yr]] <- merge(agelencomp_temp_list[[yr]], agecl_df, by = "agecl", all = TRUE) # fill in ages, if missing that year
    agelencomp_temp_list[[yr]] <- agelencomp_temp_list[[yr]][,9:ncol(agelencomp_temp_list[[yr]])]
    agelencomp_temp_list[[yr]][is.na(agelencomp_temp_list[[yr]])] <- 0
  }
  
  # Fill in bits
  age_trans_temp <- data.frame(
    Name = agelen_names$Name[i],
    Age_transition_name = agelen_names$survey[i],
    Age_transition_index = i,
    Species = spp_inds$Species[which(spp_inds$Name == agelen_names$Name[i])],
    Sex = 0,
    Age = 1:spp_inds$maxage[which(spp_inds$Name == agelen_names$Name[i])])
  
  # Get comp
  comp_temp <- Reduce('+', agelencomp_temp_list)
  comp_temp <- comp_temp[,order(as.numeric(colnames(comp_temp)))]
  comp_temp[is.na(comp_temp)] <- 0
  
  comp_temp <- comp_temp/rowSums(comp_temp) # Set up transition
  colnames(comp_temp) <- paste0("Length",1:ncol(comp_temp))
  
  # Not all ages may be present
  comp_temp$Age = agecl_df$agecl
  
  age_trans_temp <- merge(age_trans_temp, comp_temp, by = "Age", all.x = TRUE)
  age_trans_temp[is.na(age_trans_temp)] <- 0
  age_trans_temp <- age_trans_temp[,c("Name", colnames(BS2017SS$age_trans_matrix)[1:5], paste0("Length",1:max(WGSAM$nlengths)))]
  age_trans_list[[i]] <- age_trans_temp
}

age_trans_matrix <-rbindlist(age_trans_list)

# --------------------------------
# Age error ----
# --------------------------------
age_error_list <- list()
for(i in 1:WGSAM$nspp){
  ages <- WGSAM$minage[i]:WGSAM$nages[i]
  
  # Fill in bits
  age_error_tmp <- data.frame(
    Species = i,
    True_age = ages
  )
  
  comp_temp <- diag(length(ages))
  colnames(comp_temp) = paste0("Obs_age",ages)
  
  age_error_tmp <- cbind(age_error_tmp, comp_temp)
  
  age_error_list[[i]] <- age_error_tmp
}

age_error <- Reduce('bind_rows', age_error_list)


# --------------------------------
# WT ----
# --------------------------------
wt <- WGSAM$wt
species_wt_crosswalk <- fleet_control %>%
  select(Name, Fleet_name, Weight_index, Species) %>%
  rename(Wt_name = Fleet_name, Wt_index = Weight_index)

# - Survey
simSurveyWtatAge$value <- simSurveyWtatAge$value * 0.001 # Convert g to kg
simSurveyWtatAge$units <- "kg"
simSurveyWtatAge <- simSurveyWtatAge %>%
  as.data.frame() %>% 
  mutate(age = paste0("Age",age)) %>%
  pivot_wider(names_from = "age") %>% as.data.frame()

srv_wt <- data.frame(
  Name = simSurveyWtatAge$Name,
  Wt_name = simSurveyWtatAge$survey,
  Sex = 0,
  Year = simSurveyWtatAge$year
)

# Add wt_index and species
wt_temp <- simSurveyWtatAge[,paste0("Age",1:max(WGSAM$nages))]
srv_wt <- cbind(srv_wt, wt_temp)
srv_wt <- merge(srv_wt, species_wt_crosswalk, all.x = TRUE)


# - fishery
simFisheryWtatAge$value <- simFisheryWtatAge$value * 0.001 # Convert g to kg
simFisheryWtatAge$units <- "kg"
simFisheryWtatAge <- simFisheryWtatAge %>%
  as.data.frame() %>% 
  mutate(age = paste0("Age",age)) %>%
  pivot_wider(names_from = "age") %>% as.data.frame()

fsh_wt <- data.frame(
  Name = simFisheryWtatAge$Name,
  Wt_name = simFisheryWtatAge$fishery,
  Sex = 0,
  Year = simFisheryWtatAge$year
)

# Add wt_index and species
wt_temp <- simFisheryWtatAge[,paste0("Age",1:max(WGSAM$nages))]
fsh_wt <- cbind(fsh_wt, wt_temp)
fsh_wt <- merge(fsh_wt, species_wt_crosswalk, all.x = TRUE)


# - Combine fishery and survey
wt <- rbind(fsh_wt, srv_wt)
wt[is.na(wt)] <- 0 # Set wt to 0 for age surveys
wt <- wt[,c("Name", "Wt_name", "Wt_index", "Species","Sex", "Year", paste0("Age",1:max(WGSAM$nages)))]



# --------------------------------
# pmaturity  ----
# --------------------------------
# -- assume mature at half age
pmature <- data.frame(matrix(0, WGSAM$nspp, max(WGSAM$nages)+1))
pmature[,1] <- 1:nrow(pmature)
colnames(pmature) <- c("Species", paste0("Age",1:max(WGSAM$nages)))
for(i in 1:nrow(pmature)){
  pmature[i, (floor(WGSAM$nages[i]/2):WGSAM$nages[i])+1] <- 1
}


# --------------------------------
# sex_ratio - assume 0.5 ----
# --------------------------------
sex_ratio <- data.frame(matrix(0.5, WGSAM$nspp, max(WGSAM$nages)+1))
sex_ratio[,1] <- 1:nrow(sex_ratio)
colnames(sex_ratio) <- c("Species", paste0("Age",1:max(WGSAM$nages)))


# --------------------------------
# m1 - assume 0.2 ----
# --------------------------------
M1_base <- data.frame(matrix(0.2, WGSAM$nspp, max(WGSAM$nages)+2))
M1_base[,1] <- 1:nrow(M1_base)
M1_base[,2] <- 0
colnames(M1_base) <- c("Species", "Sex", paste0("Age",1:max(WGSAM$nages)))


# --------------------------------
# aLW ----
# --------------------------------
simBiolPar <- simBiolPar %>%
  as.data.frame() %>% 
  arrange(Name)
aLW <- data.frame(Species = 1: WGSAM$nspp, a = simBiolPar$WLa, b = simBiolPar$WLb)


# --------------------------------
# bioenergetics ----
# - Easier to do in excel (copy column 1 11 times)
# --------------------------------
bioenergetics <- c("Ceq", "Cindex", "Pvalue", "fday", "CA", "CB", "Qc", "Tco", "Tcm", "Tcl", "CK1", "CK4")
for(i in 1:length(bioenergetics)){
  WGSAM[[bioenergetics[i]]] <- rep(WGSAM[[bioenergetics[i]]][1], WGSAM$nspp)
}

# --------------------------------
# Env data ----
# --------------------------------
temp_series <- simSurveyBottemp %>%
  as.data.frame() %>% 
  filter(survey == "BTS_fall_allbox_effic1")
env_data <- data.frame(Year = WGSAM$styr:WGSAM$endyr, BTemp = temp_series$value)


# Update alk index
agelen_names <- agelen_names %>%
  rename(Fleet_name = survey)
fleet_control$Age_transition_index <- NULL
fleet_control <- merge(fleet_control, agelen_names, all = TRUE)
fleet_control <- fleet_control[,c("Name", colnames(BS2017SS$fleet_control))]

# --------------------------------
# Update name and add in ----
# --------------------------------
WGSAM$fleet_control <- fleet_control %>% arrange(Fleet_code)
WGSAM$srv_biom <- srv_biom
WGSAM$fsh_biom <- fsh_biom
WGSAM$comp_data <- comp_data
WGSAM$age_trans_matrix <- age_trans_matrix
WGSAM$wt <- wt

# - Update names
rename_objects <- c("fleet_control", "srv_biom", "fsh_biom", "comp_data")
for(i in 1:length(rename_objects)){
  temp_obj <- WGSAM[[rename_objects[i]]]
  temp_obj <- temp_obj %>%
    mutate(Fleet_name = paste0(Name, "_",Fleet_name)) %>%
    select(-Name)
  
  WGSAM[[rename_objects[i]]] <- temp_obj
}

WGSAM$age_trans_matrix <- WGSAM$age_trans_matrix %>%
  mutate(Age_transition_name = paste0(Name, "_",Age_transition_name)) %>%
  select(-Name)

WGSAM$wt <- WGSAM$wt %>%
  mutate(Wt_name = paste0(Name, "_",Wt_name)) %>%
  select(-Name)

# No need to adjust name on these
WGSAM$emp_sel <- emp_sel
WGSAM$age_error <- age_error 
WGSAM$pmature <- pmature
WGSAM$sex_ratio <- sex_ratio
WGSAM$M1_base <- M1_base
WGSAM$aLW <- aLW
WGSAM$env_data <- env_data
WGSAM$Pyrs <- WGSAM$Pyrs[-c(1:nrow(WGSAM$Pyrs)),]
WGSAM$UobsWtAge <- BS2017SS$UobsWtAge[-c(1:nrow(BS2017SS$UobsWtAge)),]


# Weight indices
WGSAM$pop_wt_index <- rep(1, WGSAM$nspp) #TODO
WGSAM$ssb_wt_index <- rep(1, WGSAM$nspp) #TODO
WGSAM$pop_age_transition_index <- rep(1, WGSAM$nspp) #TODO

# --------------------------------
# Save ----
# --------------------------------
setwd("~/GitHub/WGSAM-CEATTLE-Skill-Test")
write_data(WGSAM, file = "Data/WGSAM_unedited.xlsx")

# Edits using excel
# - Selectivity
# - Q


# TODO
# update ALK on control. Update wt_index for pop and ssb
# add length comp data
# convert agecl to age
# add diet data
# add ration data
# maturity from simbiopar
#FIXME: weight-at-age for fishery data missing for ages 1-4

# Questions:
# - What are spring and fall survey months?
# - Fit length comp data?
# - Are selectivity and catchability the same for spring and fall surveys?
# - Are all species are sex combined?
# - When is spawning occurring during the year for each species? Assuming at new year now.
# - Sample size for age comp? (use number of obs... which is huge!)
# - How do age-classes convert to ages?
# - What weight index to use for SSB and biomass?
# - What form of selectivity?

##  Diet data
# Gap width parameters are weight ratio (can use that to fix lognormal suitability parameters)
# found in simbiolpar (gaperatio)
