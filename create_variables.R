dat_MiD$inc_gr = ifelse(dat_MiD$hheink_imp < 500, 1,
                        ifelse(dat_MiD$hheink_imp >= 500 & dat_MiD$hheink_imp < 1000, 2,
                               ifelse(dat_MiD$hheink_imp >= 1000 & dat_MiD$hheink_imp < 1500, 3,
                                      ifelse(dat_MiD$hheink_imp >= 1500 & dat_MiD$hheink_imp < 2000, 4,
                                             ifelse(dat_MiD$hheink_imp >= 2000 & dat_MiD$hheink_imp < 2500, 5,
                                                    ifelse(dat_MiD$hheink_imp >= 2500 & dat_MiD$hheink_imp < 3000, 6,
                                                           ifelse(dat_MiD$hheink_imp >= 3000 & dat_MiD$hheink_imp < 3500, 7,
                                                                  ifelse(dat_MiD$hheink_imp >= 3500 & dat_MiD$hheink_imp < 4000, 8,
                                                                         ifelse(dat_MiD$hheink_imp >= 4000 & dat_MiD$hheink_imp < 5000, 9,
                                                                                ifelse(dat_MiD$hheink_imp >= 5000, 10, NA))))))))))


dat_MiD$SEGMENT_ = ifelse(dat_MiD$seg_kba == 1, 'Mini',
                          ifelse(dat_MiD$seg_kba == 2, 'Small',
                                 ifelse(dat_MiD$seg_kba == 3, 'compact',
                                        ifelse(dat_MiD$seg_kba == 4, 'Middle.class',
                                               ifelse(dat_MiD$seg_kba == 5, 'upper.middle.class',
                                                      ifelse(dat_MiD$seg_kba == 6, 'upper.class',
                                                             ifelse(dat_MiD$seg_kba == 7, 'suv',
                                                                    ifelse(dat_MiD$seg_kba == 8, 'all.terrain',
                                                                           ifelse(dat_MiD$seg_kba == 9, 'Sport',
                                                                                  ifelse(dat_MiD$seg_kba == 10, 'minivan',
                                                                                         ifelse(dat_MiD$seg_kba == 11, 'large.van',
                                                                                                ifelse(dat_MiD$seg_kba == 12, 'utility', NA))))))))))))

#Creating dummies for fuel type in MiD - diesel and petrol in the list are swapped in the order from MOP dataset - it is just how they are recorded in the individual datasets
dat_MiD$ANTRIEB_=  NA
dat_MiD$ANTRIEB_= ifelse(dat_MiD$A_ANTRIEB == 1, 'Petrol',
                         ifelse(dat_MiD$A_ANTRIEB == 2,'Diesel',
                                ifelse(dat_MiD$A_ANTRIEB == 3, 'Nat.Gas',
                                       ifelse(dat_MiD$A_ANTRIEB == 4, 'Hybrid',
                                              ifelse(dat_MiD$A_ANTRIEB == 5, 'Electric',NA)))))

# create dummy variables one by one
# mini
dat_MiD$seg_mini <- NA
dat_MiD$seg_mini = ifelse(dat_MiD$seg_kba == 1, 1,
                          ifelse(dat_MiD$seg_kba == 2, 0,
                                 ifelse(dat_MiD$seg_kba == 3, 0,
                                        ifelse(dat_MiD$seg_kba == 4, 0,
                                               ifelse(dat_MiD$seg_kba == 5, 0,
                                                      ifelse(dat_MiD$seg_kba == 6, 0,
                                                             ifelse(dat_MiD$seg_kba == 7, 0,
                                                                    ifelse(dat_MiD$seg_kba == 8, 0,
                                                                           ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                  ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#small
dat_MiD$seg_small <- NA
dat_MiD$seg_small = ifelse(dat_MiD$seg_kba == 1, 0,
                           ifelse(dat_MiD$seg_kba == 2, 1,
                                  ifelse(dat_MiD$seg_kba == 3, 0,
                                         ifelse(dat_MiD$seg_kba == 4, 0,
                                                ifelse(dat_MiD$seg_kba == 5, 0,
                                                       ifelse(dat_MiD$seg_kba == 6, 0,
                                                              ifelse(dat_MiD$seg_kba == 7, 0,
                                                                     ifelse(dat_MiD$seg_kba == 8, 0,
                                                                            ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                   ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                          ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                 ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#compact
dat_MiD$seg_compact <- NA
dat_MiD$seg_compact = ifelse(dat_MiD$seg_kba == 1, 0,
                             ifelse(dat_MiD$seg_kba == 2, 0,
                                    ifelse(dat_MiD$seg_kba == 3, 1,
                                           ifelse(dat_MiD$seg_kba == 4, 0,
                                                  ifelse(dat_MiD$seg_kba == 5, 0,
                                                         ifelse(dat_MiD$seg_kba == 6, 0,
                                                                ifelse(dat_MiD$seg_kba == 7, 0,
                                                                       ifelse(dat_MiD$seg_kba == 8, 0,
                                                                              ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                     ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                            ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                   ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#Middle class
dat_MiD$seg_middle.class <- NA
dat_MiD$seg_middle.class = ifelse(dat_MiD$seg_kba == 1, 0,
                                  ifelse(dat_MiD$seg_kba == 2, 0,
                                         ifelse(dat_MiD$seg_kba == 3, 0,
                                                ifelse(dat_MiD$seg_kba == 4, 1,
                                                       ifelse(dat_MiD$seg_kba == 5, 0,
                                                              ifelse(dat_MiD$seg_kba == 6, 0,
                                                                     ifelse(dat_MiD$seg_kba == 7, 0,
                                                                            ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                   ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                          ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                 ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                        ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#upper middle class
dat_MiD$seg_upper.middle.class <- NA
dat_MiD$seg_upper.middle.class = ifelse(dat_MiD$seg_kba == 1, 0,
                                        ifelse(dat_MiD$seg_kba == 2, 0,
                                               ifelse(dat_MiD$seg_kba == 3, 0,
                                                      ifelse(dat_MiD$seg_kba == 4, 0,
                                                             ifelse(dat_MiD$seg_kba == 5, 1,
                                                                    ifelse(dat_MiD$seg_kba == 6, 0,
                                                                           ifelse(dat_MiD$seg_kba == 7, 0,
                                                                                  ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                       ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                              ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#upper class
dat_MiD$seg_upper.class <- NA
dat_MiD$seg_upper.class = ifelse(dat_MiD$seg_kba == 1, 0,
                                 ifelse(dat_MiD$seg_kba == 2, 0,
                                        ifelse(dat_MiD$seg_kba == 3, 0,
                                               ifelse(dat_MiD$seg_kba == 4, 0,
                                                      ifelse(dat_MiD$seg_kba == 5, 0,
                                                             ifelse(dat_MiD$seg_kba == 6, 1,
                                                                    ifelse(dat_MiD$seg_kba == 7, 0,
                                                                           ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                  ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                       ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#sport
dat_MiD$seg_suv <- NA
dat_MiD$seg_suv = ifelse(dat_MiD$seg_kba == 1, 0,
                         ifelse(dat_MiD$seg_kba == 2, 0,
                                ifelse(dat_MiD$seg_kba == 3, 0,
                                       ifelse(dat_MiD$seg_kba == 4, 0,
                                              ifelse(dat_MiD$seg_kba == 5, 0,
                                                     ifelse(dat_MiD$seg_kba == 6, 0,
                                                            ifelse(dat_MiD$seg_kba == 7, 1,
                                                                   ifelse(dat_MiD$seg_kba == 8, 0,
                                                                          ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                 ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                        ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                               ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#all terrain
dat_MiD$seg_all.terrain <- NA
dat_MiD$seg_all.terrain = ifelse(dat_MiD$seg_kba == 1, 0,
                                 ifelse(dat_MiD$seg_kba == 2, 0,
                                        ifelse(dat_MiD$seg_kba == 3, 0,
                                               ifelse(dat_MiD$seg_kba == 4, 0,
                                                      ifelse(dat_MiD$seg_kba == 5, 0,
                                                             ifelse(dat_MiD$seg_kba == 6, 0,
                                                                    ifelse(dat_MiD$seg_kba == 7, 0,
                                                                           ifelse(dat_MiD$seg_kba == 8, 1,
                                                                                  ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                       ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#sport
dat_MiD$seg_sport <- NA
dat_MiD$seg_sport = ifelse(dat_MiD$seg_kba == 1, 0,
                           ifelse(dat_MiD$seg_kba == 2, 0,
                                  ifelse(dat_MiD$seg_kba == 3, 0,
                                         ifelse(dat_MiD$seg_kba == 4, 0,
                                                ifelse(dat_MiD$seg_kba == 5, 0,
                                                       ifelse(dat_MiD$seg_kba == 6, 0,
                                                              ifelse(dat_MiD$seg_kba == 7, 0,
                                                                     ifelse(dat_MiD$seg_kba == 8, 0,
                                                                            ifelse(dat_MiD$seg_kba == 9, 1,
                                                                                   ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                          ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                 ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#minivan
dat_MiD$seg_minivan <- NA
dat_MiD$seg_minivan = ifelse(dat_MiD$seg_kba == 1, 0,
                             ifelse(dat_MiD$seg_kba == 2, 0,
                                    ifelse(dat_MiD$seg_kba == 3, 0,
                                           ifelse(dat_MiD$seg_kba == 4, 0,
                                                  ifelse(dat_MiD$seg_kba == 5, 0,
                                                         ifelse(dat_MiD$seg_kba == 6, 0,
                                                                ifelse(dat_MiD$seg_kba == 7, 0,
                                                                       ifelse(dat_MiD$seg_kba == 8, 0,
                                                                              ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                     ifelse(dat_MiD$seg_kba == 10, 1,
                                                                                            ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                   ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#large van
dat_MiD$seg_large.van <- NA
dat_MiD$seg_large.van = ifelse(dat_MiD$seg_kba == 1, 0,
                               ifelse(dat_MiD$seg_kba == 2, 0,
                                      ifelse(dat_MiD$seg_kba == 3, 0,
                                             ifelse(dat_MiD$seg_kba == 4, 0,
                                                    ifelse(dat_MiD$seg_kba == 5, 0,
                                                           ifelse(dat_MiD$seg_kba == 6, 0,
                                                                  ifelse(dat_MiD$seg_kba == 7, 0,
                                                                         ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                       ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                              ifelse(dat_MiD$seg_kba == 11, 1,
                                                                                                     ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))

#utilities
dat_MiD$seg_utility <- NA
dat_MiD$seg_utility = ifelse(dat_MiD$seg_kba == 1, 0,
                             ifelse(dat_MiD$seg_kba == 2, 0,
                                    ifelse(dat_MiD$seg_kba == 3, 0,
                                           ifelse(dat_MiD$seg_kba == 4, 0,
                                                  ifelse(dat_MiD$seg_kba == 5, 0,
                                                         ifelse(dat_MiD$seg_kba == 6, 0,
                                                                ifelse(dat_MiD$seg_kba == 7, 0,
                                                                       ifelse(dat_MiD$seg_kba == 8, 0,
                                                                              ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                     ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                            ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                   ifelse(dat_MiD$seg_kba == 12, 1, NA))))))))))))

# create dummy variables one by one for fuel types
dat_MiD$fuel_Petrol <- NA
dat_MiD$fuel_Petrol = ifelse(dat_MiD$A_ANTRIEB == 1, 1,
                             ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                    ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                           ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                  ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Diesel <- NA
dat_MiD$fuel_Diesel= ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                            ifelse(dat_MiD$A_ANTRIEB == 2, 1,
                                   ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                          ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                 ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Nat.Gas <- NA
dat_MiD$fuel_Nat.Gas = ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                              ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                     ifelse(dat_MiD$A_ANTRIEB == 3, 1,
                                            ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                   ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Hybrid <- NA
dat_MiD$fuel_Hybrid = ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                             ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                    ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                           ifelse(dat_MiD$A_ANTRIEB == 4, 1,
                                                  ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Electric <- NA
dat_MiD$fuel_Electric = ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                               ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                      ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                             ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                    ifelse(dat_MiD$A_ANTRIEB == 5, 1, NA)))))

