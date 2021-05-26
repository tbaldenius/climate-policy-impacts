#### Fuel efficency estimation ####

dat_TANK$ID <- dat_TANK$IDHH # Rename household ID to match with dat_HH dataset
dat_MOP <- merge(dat_HH,dat_TANK,by="ID")

# Recode variables for interpretability of regression model and to correctly classify them as categorial variables

dat_MOP$SEGMENT_ = ifelse(dat_MOP$SEGMENT == 1, 'mini',
                          ifelse(dat_MOP$SEGMENT == 2, 'small',
                                 ifelse(dat_MOP$SEGMENT == 3, 'compact',
                                        ifelse(dat_MOP$SEGMENT == 4, 'middle.class',
                                               ifelse(dat_MOP$SEGMENT == 5, 'upper.middle.class',
                                                      ifelse(dat_MOP$SEGMENT == 6, 'upper.class',
                                                             ifelse(dat_MOP$SEGMENT == 7, 'all.terrain',
                                                                    ifelse(dat_MOP$SEGMENT == 8, 'sport',
                                                                           ifelse(dat_MOP$SEGMENT == 9, 'minivan',
                                                                                  ifelse(dat_MOP$SEGMENT == 10, 'large.van',
                                                                                         ifelse(dat_MOP$SEGMENT == 11, 'utility',
                                                                                                ifelse(dat_MOP$SEGMENT == 13, 'suv', NA))))))))))))

dat_MOP$ANTRIEB_= ifelse(dat_MOP$ANTRIEB == 1, 'diesel',
                         ifelse(dat_MOP$ANTRIEB == 2,'petrol',
                                ifelse(dat_MOP$ANTRIEB == 3, 'nat.Gas',
                                       ifelse(dat_MOP$ANTRIEB == 4, 'hybrid',
                                              ifelse(dat_MOP$ANTRIEB == 5, 'electric',NA)))))

mod4 <- lm(VERB ~ SEGMENT_ + KMJAHR + BAUJAHR + ANTRIEB_+ EINKO, data = dat_MOP) # Estimate regression model

if(test_income) dat_MiD_HH <- filter(dat_MiD_HH, hheink_imp!=9000) # True: Exclude housholds exceeding the maximum reported income (9000 Euro/month)

dat_MiD <- right_join(dat_MiD_Auto, dat_MiD_HH, by = c("H_ID","H_ANZAUTO"))

source("create_variables.R") # Recode a collection of variables for later use

if(test_anzauto) dat_MiD <- filter(dat_MiD, H_ANZAUTO != 0) # True: Exclude households that do not own a car

if(remove_na_cars) { # True: Exclude cars of which some of the relevant characteristics are missing, else, keep as missing values and impute later on
  if(test_morecars) { # True: Include households that own more than three cars and impute average efficiency for those whose characteristics were not reported (a maximum of three cars' characterisitcs are reported in the survey)
    dat_MiD <- filter(dat_MiD, A_BAUJ != 9999 & # Serves to exclude any missing values and values marked implausible
                        A_JAHRESFL <= 250000 &
                        H_ANZAUTO <= 98 &
                        !is.na(SEGMENT_) &
                        !is.na(ANTRIEB_) |
                        H_ANZAUTO == 0) 
  } else {
    dat_MiD <- filter(dat_MiD, A_BAUJ != 9999 &
                        A_JAHRESFL <= 250000 &
                        H_ANZAUTO <= 3 &
                        !is.na(SEGMENT_) &
                        !is.na(ANTRIEB_) |
                        H_ANZAUTO == 0)
  }
} else{
  if(test_morecars) {
    dat_MiD <- filter(dat_MiD, H_ANZAUTO <= 98)
  } else {
    dat_MiD <- filter(dat_MiD, H_ANZAUTO <= 3)
  }
  dat_MiD$A_BAUJ <- ifelse(dat_MiD$A_BAUJ != 9999, dat_MiD$A_BAUJ, NA) # Missing values of construction year and yearly distance are imputed here
  dat_MiD$A_BAUJ <- ifelse(!is.na(dat_MiD$A_BAUJ), dat_MiD$A_BAUJ, mean(dat_MiD$A_BAUJ, na.rm = TRUE))
  dat_MiD$A_JAHRESFL <- ifelse(dat_MiD$A_JAHRESFL <= 250000, dat_MiD$A_JAHRESFL, NA)
  dat_MiD$A_JAHRESFL <- ifelse(!is.na(dat_MiD$A_JAHRESFL), dat_MiD$A_JAHRESFL, mean(dat_MiD$A_JAHRESFL, na.rm = TRUE))
}

# Predict fuel efficiency of MiD observations based on MOP-based model

coeffs_mod4 <- summary(mod4)$coefficients

if(remove_na_cars) { # True: missing values were excluded, compute fuel efficiency estimates
  dat_MiD$fuel_eff = (coeffs_mod4[1,1] 
                      + dat_MiD$seg_compact*coeffs_mod4[2,1] 
                      + dat_MiD$seg_large.van*coeffs_mod4[3,1]
                      + dat_MiD$seg_middle.class*coeffs_mod4[4,1] 
                      + dat_MiD$seg_mini*coeffs_mod4[5,1]
                      + dat_MiD$seg_minivan*coeffs_mod4[6,1] 
                      + dat_MiD$seg_small*coeffs_mod4[7,1]
                      + dat_MiD$seg_sport*coeffs_mod4[8,1] 
                      + dat_MiD$seg_suv*coeffs_mod4[9,1]
                      + dat_MiD$seg_upper.class*coeffs_mod4[10,1]
                      + dat_MiD$seg_upper.middle.class*coeffs_mod4[11,1]
                      + dat_MiD$seg_utility*coeffs_mod4[12,1]
                      + dat_MiD$A_JAHRESFL*coeffs_mod4[13,1]
                      + dat_MiD$A_BAUJ*coeffs_mod4[14,1]
                      + dat_MiD$fuel_Hybrid*coeffs_mod4[15,1]
                      + dat_MiD$fuel_Nat.Gas*coeffs_mod4[16,1] 
                      + dat_MiD$fuel_Petrol*coeffs_mod4[17,1]
                      + dat_MiD$inc_gr * coeffs_mod4 [18,1])
} else { # Determine characterstic-specific average values for cars of which we do not know the vehicle class or drive type
  avg_seg <-  mean(
    dat_MiD$seg_compact * coeffs_mod4[2, 1]
    + dat_MiD$seg_large.van * coeffs_mod4[3, 1]
    + dat_MiD$seg_middle.class * coeffs_mod4[4, 1]
    + dat_MiD$seg_mini * coeffs_mod4[5, 1]
    + dat_MiD$seg_minivan * coeffs_mod4[6, 1]
    + dat_MiD$seg_small * coeffs_mod4[7, 1]
    + dat_MiD$seg_sport * coeffs_mod4[8, 1]
    + dat_MiD$seg_suv * coeffs_mod4[9, 1]
    + dat_MiD$seg_upper.class * coeffs_mod4[10, 1]
    + dat_MiD$seg_upper.middle.class * coeffs_mod4[11, 1]
    + dat_MiD$seg_utility * coeffs_mod4[12, 1],
    na.rm = TRUE
  )
  avg_ant <- mean(
    dat_MiD$fuel_Hybrid*coeffs_mod4[15,1]
    + dat_MiD$fuel_Nat.Gas*coeffs_mod4[16,1] 
    + dat_MiD$fuel_Petrol*coeffs_mod4[17,1],
    na.rm = TRUE
  )
  dat_MiD$fuel_eff <-  rep(NA, nrow(dat_MiD))
  dat_MiD$fuel_eff[is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)] = (coeffs_mod4[1,1] +
                                                                           avg_seg+
                                                                           dat_MiD$A_JAHRESFL[is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[13,1]+
                                                                           dat_MiD$A_BAUJ[is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[14,1]+
                                                                           avg_ant+
                                                                           dat_MiD$inc_gr[is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)] * coeffs_mod4 [18,1])
  dat_MiD$fuel_eff[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)] = (coeffs_mod4[1,1] +
                                                                            dat_MiD$seg_compact[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[2,1] +
                                                                            dat_MiD$seg_large.van[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[3,1] +
                                                                            dat_MiD$seg_middle.class[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[4,1] + 
                                                                            dat_MiD$seg_mini[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[5,1] +
                                                                            dat_MiD$seg_minivan[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[6,1] + 
                                                                            dat_MiD$seg_small[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[7,1] +
                                                                            dat_MiD$seg_sport[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[8,1] +
                                                                            dat_MiD$seg_suv[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[9,1] +
                                                                            dat_MiD$seg_upper.class[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[10,1] +
                                                                            dat_MiD$seg_upper.middle.class[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[11,1] +
                                                                            dat_MiD$seg_utility[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[12,1] +
                                                                            dat_MiD$A_JAHRESFL[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[13,1] +
                                                                            dat_MiD$A_BAUJ[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[14,1] +
                                                                            avg_ant +
                                                                            dat_MiD$inc_gr[!is.na(dat_MiD$SEGMENT_) & is.na(dat_MiD$ANTRIEB_)] * coeffs_mod4 [18,1])
  dat_MiD$fuel_eff[is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)] = (coeffs_mod4[1,1] +
                                                                            avg_seg +
                                                                            dat_MiD$A_JAHRESFL[is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[13,1] +
                                                                            dat_MiD$A_BAUJ[is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[14,1] +
                                                                            dat_MiD$fuel_Hybrid[is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[15,1] +
                                                                            dat_MiD$fuel_Nat.Gas[is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[16,1]+
                                                                            dat_MiD$fuel_Petrol[is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[17,1] +
                                                                            dat_MiD$inc_gr[is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)] * coeffs_mod4 [18,1])
  dat_MiD$fuel_eff[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)] = (coeffs_mod4[1,1] +
                                                                             dat_MiD$seg_compact[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[2,1] + 
                                                                             dat_MiD$seg_large.van[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[3,1] +
                                                                             dat_MiD$seg_middle.class[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[4,1] + 
                                                                             dat_MiD$seg_mini[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[5,1] +
                                                                             dat_MiD$seg_minivan[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[6,1] + 
                                                                             dat_MiD$seg_small[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[7,1] +
                                                                             dat_MiD$seg_sport[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[8,1]  +
                                                                             dat_MiD$seg_suv[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[9,1] +
                                                                             dat_MiD$seg_upper.class[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[10,1] +
                                                                             dat_MiD$seg_upper.middle.class[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[11,1] +
                                                                             dat_MiD$seg_utility[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[12,1]+
                                                                             dat_MiD$A_JAHRESFL[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[13,1]+
                                                                             dat_MiD$A_BAUJ[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[14,1]+
                                                                             dat_MiD$fuel_Hybrid[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[15,1]+
                                                                             dat_MiD$fuel_Nat.Gas[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[16,1]+ 
                                                                             dat_MiD$fuel_Petrol[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)]*coeffs_mod4[17,1]+
                                                                             dat_MiD$inc_gr[!is.na(dat_MiD$SEGMENT_) & !is.na(dat_MiD$ANTRIEB_)] * coeffs_mod4 [18,1])
}

# Deal with missing and implausible values

dat_MiD$fuel_eff[dat_MiD$fuel_eff <= 0] <- NA # Negative values set missing
dat_MiD$fuel_eff[dat_MiD$fuel_Electric==1] = ifelse(keep_electric_cars, 0, NA) # Set fuel efficiency of electric vehicles to 0 (litres/100km)
dat_MiD$fuel_eff[dat_MiD$H_ANZAUTO == 0] = NA # Set fuel efficency of households without a car to missing.
dat_MiD$A_JAHRESFL[dat_MiD$H_ANZAUTO == 0] = NA
dat_MiD$A_BAUJ[dat_MiD$H_ANZAUTO == 0] = NA
dat_MiD = filter(dat_MiD, !is.na(fuel_eff) | H_ANZAUTO == 0) # Remove any missing values from households that own at least one car

dat_MiD$h_inco <- dat_MiD$hheink_imp
dat_MiD$h_inco_eq <- switch(use_eq_scale,
                         dat_MiD$hheink_imp,
                         dat_MiD$aq_eink,
                         dat_MiD$hheink_imp/sqrt(dat_MiD$H_GR.x))

if(test_morecars) { # True: Impute average efficiency for vehicles whose characteristics were not reported (a maximum of three cars' characterisitcs per household are reported in the survey)
  add_cars <- dat_MiD %>%
    filter(H_ANZAUTO > 3) %>%
    group_by(H_ID) %>%
    summarise(fuel_eff = mean(fuel_eff),
              H_ANZAUTO = unique(H_ANZAUTO),
              A_JAHRESFL = morecars_fl,
              fuel_Petrol = 1, # Set drive type to petrol, because this makes up the largest fraction of vehicles in the survey (and expectedly in the population)
              fuel_Nat.Gas = 0,
              fuel_Diesel = 0,
              fuel_Electric = 0,
              fuel_Hybrid = 0,
              H_GEW = unique(H_GEW),
              A_GEW = unique(A_GEW),
              h_inco_eq = unique(h_inco_eq),
              h_inco = unique(h_inco))
  dupl <-  unlist(map2(1:nrow(add_cars), add_cars$H_ANZAUTO-3, rep)) # Get indices of households that have 4+ cars as many times as they have cars exceeding 3
  add_cars <-  add_cars[dupl,] # Replicate observations to match number of cars exceeding 3
  dat_MiD <- bind_rows(dat_MiD, add_cars) # Add to existing dataset
}


#### Carbon price and efficiency standard ####

# Using numbers from UBA (https://www.umweltbundesamt.de/sites/default/files/medien/1968/publikationen/co2_emission_factors_for_fossil_fuels_correction.pdf)
# For the carbon content of petrol and diesel;
#   Petrol: We use the emission factor for 'super' (page 32);
#   Diesel:taking the mean of emission factor (third column page 34 of publication) diesel summer and winter
# Using https://www.bdbe.de/daten/umrechnung-und-formeln for the density of diesel and petrol- the second number

# Calculate Euro per liter costs

tax_liter_petrol_55 <- 55*3.186*0.00075 # price (Euro/t co2) * emission factor (co2/fuel) * fuel density (t/l)
tax_liter_diesel_55 <- 55*3.1665*0.00084
tax_liter_hybrid_55 <- tax_liter_petrol_55 # Most hybrids are petrol-based, their reported fuel consumption corresponds to their fuel-driven part

# Annual cost carbon price

dat_MiD$acost_car_cp55 <- (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_diesel_55*dat_MiD$fuel_Diesel + # yearly distance travelled (100km) * fuel efficency (l fuel/100km) * Euro per liter cost (Euro/l fuel)
  (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_petrol_55*dat_MiD$fuel_Petrol +
  (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_hybrid_55*dat_MiD$fuel_Hybrid

dat_MiD <- filter(dat_MiD, fuel_Nat.Gas == 0 | H_ANZAUTO == 0) # Exclude natural-gas-driven cars, they make up a tiny fraction of the dataset and emissions vary widely depending on what type of gas
dat_MiD$acost_car_cp55[dat_MiD$H_ANZAUTO == 0] = 0 # Set payment of households without car to 0


# Calculate revenue equivalent tax on inefficiency (efficency standard)

dat_MiD$weight <- dat_MiD$H_HOCH
total_tax_cp55 <- sum(dat_MiD$acost_car_cp55*dat_MiD$weight)
ineff_tax_cp55 <- total_tax_cp55 / sum(dat_MiD$fuel_eff*dat_MiD$weight, na.rm = TRUE)
dat_MiD$acost_ineff_cp55 <- dat_MiD$fuel_eff * ineff_tax_cp55 
dat_MiD$acost_ineff_cp55[dat_MiD$H_ANZAUTO == 0] <- 0 # Needs to be set to zero, otherwise missing and excluded from analysis

# Bring to household level

g_HH <- dat_MiD %>% group_by(H_ID)

# Carbon price

df_cp55 <- summarise(g_HH,
                     h_total_cost_cp55 = sum(acost_car_cp55),
                     h_inco = unique(h_inco),
                     h_inco_eq = unique(h_inco_eq),
                     #comm = unique(commuter),
                     w = unique(H_HOCH))

df_cp55 <- mutate(df_cp55,
                  quintile =  cut(h_inco_eq, MetricsWeighted::weighted_quantile(df_cp55$h_inco_eq, df_cp55$w, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE),
                  h_inc.share = h_total_cost_cp55/(h_inco*12)*100, # (yearly household cost / yearly household income) * 100
                  w = w,
                  legend = "CP 55") 


# Efficency standard

df_ineff_cp55 <- summarise(g_HH,
                           h_total_cost_ineff_cp55 = sum(acost_ineff_cp55),
                           h_inco = unique(h_inco),
                           h_inco_eq = unique(h_inco_eq),
                           #comm = unique(commuter),
                           w = unique(H_HOCH))

df_ineff_cp55 <- mutate(df_ineff_cp55,
                        quintile =  cut(h_inco_eq, MetricsWeighted::weighted_quantile(df_ineff_cp55$h_inco_eq, df_ineff_cp55$w, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE),
                        h_inc.share = (h_total_cost_ineff_cp55/(h_inco*12))*100, # (yearly household cost / yearly household income) * 100
                        legend = "Revenue equivalent inefficiency tax")


#### Revenue recycling policies #####

# Lump sum recycling

recycle_value_cp55 <- total_tax_cp55/sum(df_cp55$w)

df_cp55_r <- summarise(g_HH,
                       h_total_cost_cp55 = sum(acost_car_cp55),
                       h_inco = unique(h_inco),
                       h_inco_eq = unique(h_inco_eq),
                       #comm = unique(commuter),
                       w = unique(H_HOCH))

df_cp55_r <- mutate(df_cp55_r,
                    h_total_cost_cp55_recycled = h_total_cost_cp55 - recycle_value_cp55, # Net tax after revenue recycling
                    quintile =  cut(h_inco_eq, MetricsWeighted::weighted_quantile(df_cp55_r$h_inco_eq, df_cp55_r$w, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE),
                    h_inc.share = (h_total_cost_cp55_recycled/(h_inco*12))*100,
                    legend = "CP 55 + Lump sum recycling")

if(abs(weighted.mean(df_cp55_r$h_total_cost_cp55_recycled, df_cp55$w)) > 1e-5) stop("Error: No revenue neutral outcome")


#### Feebate #####

# Calculate g CO2 emitted per km

tCO2_per_l_petrol <- 3.186*0.00075 # See 'Carbon price and eficiency standard'
tCO2_per_l_diesel <- 3.1665*0.00084
dat_MiD$fuel_eff_lkm <- dat_MiD$fuel_eff/100
dat_MiD$gramsco2km <- ((dat_MiD$fuel_eff_lkm *tCO2_per_l_diesel*dat_MiD$fuel_Diesel) + # l fuel/km * emission factor (t co2 /t fuel) * fuel density (t/l) * 1000000 (g co2/t co2)
                                   (dat_MiD$fuel_eff_lkm*tCO2_per_l_petrol*dat_MiD$fuel_Petrol) +
                                   (dat_MiD$fuel_eff_lkm*tCO2_per_l_petrol*dat_MiD$fuel_Hybrid))*1000000



# Feebate monthly payment: payment is linear in 'gramsco2km' and designed such that it is revenue neutral and total payment of those who make (positive) tax payments equates the total tax revenue of the carbon price

pivot <- weighted.mean(dat_MiD$gramsco2km, dat_MiD$H_HOCH, na.rm = TRUE) # Set point of zero payment
m <- sum(df_cp55$h_total_cost_cp55*df_cp55$w, na.rm = TRUE) / sum((dat_MiD$gramsco2km-pivot)*((dat_MiD$gramsco2km-pivot)>0)*dat_MiD$H_HOCH, na.rm = TRUE) # Determine slope
dat_MiD$feebate_margin <- m*(dat_MiD$gramsco2km-pivot) # Determine individual feebate payments
dat_MiD$feebate_margin[dat_MiD$H_ANZAUTO == 0] = 0

# Bring to household level

df_bonus_malus_marginal <- dat_MiD %>%
  group_by(H_ID) %>%
  summarise(h_inco = unique(h_inco),
            h_inco_eq = unique(h_inco_eq),
            #comm = unique(commuter),
            h_total_cost_feebate = sum(feebate_margin),
            w = unique(H_HOCH))

df_bonus_malus_marginal <-  df_bonus_malus_marginal %>%
  mutate(quintile =  cut(h_inco_eq, MetricsWeighted::weighted_quantile(df_bonus_malus_marginal$h_inco_eq, df_bonus_malus_marginal$w, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE),
         legend = "Feebate",
         h_inc.share = (h_total_cost_feebate/(h_inco*12))*100)

if(abs(weighted.mean(df_bonus_malus_marginal$h_total_cost_feebate, df_bonus_malus_marginal$w)) > 1e-5) stop("Error: No revenue neutral outcome")

##### Driving ban

dat_MiD_A4 <- left_join(dat_MiD_HH, dat_MiD_Wege, by = "H_ID") ## HH x Wege

# 1) Dropping outliers + means of transport other than car + reason to travel other than going to work
# 2) create relevant variables
# 3) aggregate to household level
# 4) create quintile and income share variables

df_A4 <- filter(dat_MiD_A4, anzerw18 <= 4 & # 1)
                       min_altern_opnv <= 120 &
                       wegmin <= 120 &
                       (dat_MiD_A4$hvm ==4|dat_MiD_A4$hvm ==3) &
                       dat_MiD_A4$zweck ==1) %>%
  mutate(extra_travel_time = min_altern_opnv - wegmin, # 2)
         travel_work_car = wegmin,
         altern_public_trans = min_altern_opnv,
         travel_work_car_month = travel_work_car * 2 * 20, # reasoning *2: roundtrip, *20: approx. 20 work days per month
         altern_public_trans_month = altern_public_trans * 2 * 20,
         h_inco_minutes = hheink_imp/(43200*2), #Household income in minutes - there are 43,200 minutes in a month of 30 days (multiplied by 2 as time is valuated by half)
         car_ban_cost = h_inco_minutes*extra_travel_time * 2 * 20, #cost of the car ban multiplied by 2 for two trips per day and by 20 as there are roughly 20 workdays per month
         extra_travel_time_month = extra_travel_time * 2 * 20)  %>%
  group_by(H_ID) %>%
  summarise(additional_time = sum(extra_travel_time), # 3)
            additional_time_month = sum(extra_travel_time_month),
            h_cost_timeloss = sum(car_ban_cost),
            h_inco = unique(hheink_imp),
            h_inco_eq = unique(aq_eink),
            state = unique(BLAND.x),
            regiostar17 = unique(RegioStaR17.x),
            commute_car_month = sum(travel_work_car_month),
            altern_public_trans_month = sum(altern_public_trans_month),
            adults_hh = unique(anzerw18),
            ad_employed = sum(HP_BERUF == 1),
            quali_opnv = unique(quali_opnv.x),
            w = unique(H_HOCH))

df_A4 <- df_A4 %>%
  mutate(quintile = cut(h_inco_eq, MetricsWeighted::weighted_quantile(df_A4$h_inco_eq, df_A4$w, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE), # 4)
         state = factor(state),
         state_name = recode(state, "1"="Schleswig-Holstein","2"="Hamburg","3"="Lower Saxony","4"="Bremen","5"="North Rhine-Westphalia",
                                       "6"="Hesse","7"="Rhineland-Palatinate","8"="Baden-Württemberg","9"="Bavaria","10"="Saarland",
                                       "11"="Berlin","12"="Brandenburg","13"="Mecklenburg-Western Pomerania","14"="Saxony","15"="Saxony-Anhalt",
                                       "16"="Thuringia"),
         state_abb = recode(state, "1"="SH","2"="HH","3"="NI","4"="HB","5"="NW",
                                                 "6"="HE","7"="RP","8"="BW","9"="BY","10"="SL",
                                                 "11"="BE","12"="BB","13"="MV","14"="SN","15"="ST","16"="TH"),
         h_inc.share = h_cost_timeloss/h_inco*100)


###analysis of all metropolitan areas in Germany, creating one df for each regional type within the area

#Metropolis
df_A4_Metropolis <- select(filter(df_A4, regiostar17 == 111),
                           c(altern_public_trans_month, quali_opnv, w, H_ID,state,quintile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_Metropolis$legend = "Metropolis"
#Large city
df_A4_large_city <- select(filter(df_A4, regiostar17 == 112),
                           c(altern_public_trans_month, quali_opnv, w, H_ID,state,quintile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_large_city$legend = "Large city"
#Medium-sized town
df_A4_medium_sized_town <- select(filter(df_A4, regiostar17 == 113),
                                  c(altern_public_trans_month, quali_opnv, w, H_ID,state,quintile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_medium_sized_town$legend = "Medium-sized town"
#Urban region
df_A4_urban_region <- select(filter(df_A4, regiostar17 == 114),
                             c(altern_public_trans_month, quali_opnv, w, H_ID,state,quintile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_urban_region$legend = "Urban region"
#Rural region
df_A4_rural_region <- select(filter(df_A4, regiostar17 == 115),
                             c(altern_public_trans_month, quali_opnv, w, H_ID,state,quintile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_rural_region$legend = "Rural region"

#stacking the datasets
df_A4_stack1 <- bind_rows(df_A4_Metropolis,df_A4_large_city)
df_A4_stack2 <- bind_rows(df_A4_stack1,df_A4_medium_sized_town)
df_A4_stack3 <- bind_rows(df_A4_stack2,df_A4_urban_region)
df_A4_stack <- bind_rows(df_A4_stack3,df_A4_rural_region)

#order labels to get the outputs in the right order
df_A4_stack$legend <- ordered(df_A4_stack$legend, levels=c("Metropolis", "Large city",  "Medium-sized town","Urban region","Rural region"))

