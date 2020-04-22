library(tidyverse)
library(knitr)
library(janitor)

# I made an R script that loads in all the data because it is very large. 
# Download the data from the dataverse and place the csv files into a folder called "data"
# https://doi.org/10.7910/DVN/8FWVFK

poison_multiple <- read_csv("data/drugpoisonings_multiplecausesofdeath_stateyear_19992017pop.csv", 
                            col_types = cols(State = col_character(),
                                             Year = col_double(),
                                             Deaths = col_double(),
                                             Population = col_double(),
                                             `Crude Rate` = col_double(),
                                             `Age Adjusted Rate` = col_double())) %>% 
  clean_names()
write_rds(poison_multiple, "data/poison_multiple.rds")




# they don't use this data

# poison_underlying <- read_csv("data/drugpoisonings_underlyingcauseofdeath_stateyear_19992017pop.csv", 
#                               col_types = cols(State = col_character(),
#                                                Year = col_double(),
#                                                Deaths = col_double(),
#                                                Population = col_double(),
#                                                `Crude Rate` = col_character(),
#                                                `Age Adjusted Rate` = col_character())) %>% 
#   clean_names()




fent2017nflis <- read_csv("data/fent2017nflis.csv",
                          col_types = cols(.default = col_double(),
                                           State = col_character())) %>% 
  clean_names()
write_rds(fent2017nflis, "data/fent2017nflis.rds")


# they don't use this data

# FOIA01_05 <- read_delim("data/FOIA_ItemLevelData_2001_2005.txt", 
#                         delim = ",", 
#                         col_types = cols(.default = col_logical(),
#                                          State = col_character(),
#                                          NFLISID = col_double(),
#                                          AnalysisID = col_double(),
#                                          SubmitDate = col_character(),
#                                          Color = col_character(),
#                                          Form = col_character(),
#                                          Quantity = col_double(),
#                                          Units = col_character(),
#                                          Subst1 = col_character(),
#                                          Subst2 = col_character(),
#                                          Subst3 = col_character(),
#                                          Subst4 = col_character(),
#                                          Subst5 = col_character(),
#                                          Subst6 = col_character(),
#                                          Subst7 = col_character(),
#                                          Subst8 = col_character(),
#                                          `PACKAGING/MARKINGS` = col_character(),
#                                          `How Acquired` = col_character(),
#                                          Purity = col_character(),
#                                          Origin = col_character(),
#                                          Manufacturer = col_character())) %>% 
#   clean_names()



# they don't use this data
# FOIA06_10 <- read_delim("data/FOIA_ItemLevelData_20062010.txt",
#                         delim = ",", 
#                         col_names = c("state","nflis_id","analysis_id",
#                                       "submit_date","how_acquired","color",
#                                       "form","quantity","units","subst1",
#                                       "subst2","subst3","subst4","subst5",
#                                       "subst6","subst7","subst8","purity",
#                                       "origin","manufacturer","packaging"), 
#                         col_types = cols(.default = col_logical(),
#                                          state = col_character(),
#                                          nflis_id = col_double(),
#                                          analysis_id = col_double(),
#                                          submit_date = col_character(),
#                                          color = col_character(),
#                                          form = col_character(),
#                                          quantity = col_double(),
#                                          units = col_character(),
#                                          subst1 = col_character(),
#                                          subst2 = col_character(),
#                                          subst3 = col_character(),
#                                          subst4 = col_character(),
#                                          subst5 = col_character(),
#                                          subst6 = col_character(),
#                                          subst7 = col_character(),
#                                          subst8 = col_character(),
#                                          packaging = col_character(),
#                                          how_acquired = col_character(),
#                                          purity = col_character(),
#                                          origin = col_character(),
#                                          manufacturer = col_character())) %>% 
#   clean_names()


FOIA11_16 <- read_delim("data/FOIA_ItemLevelData_20112016.txt",
                        delim = ",",
                        col_names = c("state","nflis_id","analysis_id",
                                      "submit_date","how_acquired","color",
                                      "form","quantity","units","subst1",
                                      "subst2","subst3","subst4","subst5",
                                      "subst6","subst7","subst8","purity",
                                      "origin","manufacturer","packaging"), 
                        col_types = cols(.default = col_logical(),
                                         state = col_character(),
                                         nflis_id = col_double(),
                                         analysis_id = col_double(),
                                         submit_date = col_character(),
                                         color = col_character(),
                                         form = col_character(),
                                         quantity = col_double(),
                                         units = col_character(),
                                         subst1 = col_character(),
                                         subst2 = col_character(),
                                         subst3 = col_character(),
                                         subst4 = col_character(),
                                         subst5 = col_character(),
                                         subst6 = col_character(),
                                         subst7 = col_character(),
                                         subst8 = col_character(),
                                         packaging = col_character(),
                                         how_acquired = col_character(),
                                         purity = col_character(),
                                         origin = col_character(),
                                         manufacturer = col_character())) %>% 
  clean_names()
write_rds(FOIA11_16, "data/FOIA11_16.rds")


FOIA11_16_State <- read_delim("data/FOIA_StateLevelData_20112016.txt",
                              delim = ",", 
                              col_types = cols(State = col_character(),
                                               CaseReceived_Year = col_double(),
                                               CaseReceived_Month = col_double(),
                                               CountOfReportedResult = col_double(),
                                               Subst1 = col_character(),
                                               Subst2 = col_character(),
                                               Subst3 = col_character(),
                                               Subst4 = col_character(),
                                               Subst5 = col_character(),
                                               Subst6 = col_character(),
                                               Subst7 = col_character(),
                                               Subst8 = col_character(),
                                               SumOfReportedQuantity = col_double(),
                                               Units = col_character(),
                                               `How Acquired` = col_character(),
                                               Color = col_character(),
                                               Form = col_character(),
                                               AveragePurity = col_double())) %>% 
  clean_names()
write_rds(FOIA11_16_State, "data/FOIA11_16_State.rds")


heroin <- read_csv("data/heroin2017nflis.csv", 
                   col_types = cols(State = col_character(),
                                    Heroin = col_double(),
                                    Year = col_double())) %>% 
  clean_names()
write_rds(heroin, "data/heroin.rds")


latlon <- read_csv("data/latlon.csv", 
                   col_types = cols(State = col_character(),
                                    Latitude = col_double(),
                                    Longitude = col_double())) %>%
  clean_names()
write_rds(latlon, "data/latlon.rds")



overdose <- read_csv("data/rx_od_state_0616.csv", 
                     col_types = cols(State = col_character(),
                                      Year = col_double(),
                                      Rate = col_double(),
                                      Age.Adjusted.Rate = col_double())) %>%
  clean_names()
write_rds(overdose, "data/overdose.rds")