# Instructions -----------------------------------------------------------------
# The functions below are used to clean the data in the analysis document.

# Packages ---------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(magrittr)
library(rio)
library(lubridate)

# NEED TO CLEAN PERSPECTIVE NARRATIVES FIRST TO MAKE SURE THEY RESPOND

# Import data & set-up ---------------------------------------------------------
# wide_data <- import("data/analog2_wide_pt to remove.csv") 

# Variable cleaning 1 ----------------------------------------------------------

wide_factor_clean <- function(wide_df){
  wide_df %>% 
    ## Removing people who did not respond to the perspective taking prompt correctly
    filter(pt_remove == "0") %>% 
    ## Getting variables in correct format
    mutate(recorded_date = mdy_hm(recorded_date),
           age = as.numeric(age),
           gender = as.factor(gender),
           ethnicity = as.factor(ethnicity),
           threat = as.numeric(threat),
           cand_pref = ifelse(cand_pref == 1, "-0.5",
                              ifelse(cand_pref == 3, "0.5", NA)),
           ingroup_ident = as.numeric(ingroup_ident)) %>% 
    ## Dropping unnecessary variables
    select(-finished, -qualtrics_response_id, -pt_remove, -analog_t_o1,
           -analog_t_o2, -analog_nt_o1, -analog_nt_o2, -gender_text, -ethncity_text)
  
}

# wide_data_clean <- wide_factor_clean(wide_data)

# Remove repeat Sona IDs -------------------------------------------------------
## First check whether there are repeat Sona IDs
# id_df <- wide_data_clean %>% 
#   select(sub_id, sona_id)
# 
# id_df$sona_id[duplicated(id_df$sona_id)]
## All repeat IDs were removed in the perspective taking step; if this code is 
## run on the without removing incorrect pt responses, there are 5 repeat IDs

# Get in long-format -----------------------------------------------------------
## Organized data so that the variables that don't need pivoted are at the beginning

wide_to_long <- function(data){
  data_org <- data %>% 
    select(sub_id, duration_seconds, recorded_date, age, gender, ethnicity,
           cand_pref, ingroup_ident, threat, everything())
  
  data_org %>%
    pivot_longer(c(bfi2xsh_self_oo_1:bfi2xsh_self_oo_19,
                   bfi2xsh_nt1_o1_1:bfi2xsh_nt1_o1_19,
                   bfi2xsh_nt1_o2_1:bfi2xsh_nt1_o2_19,
                   bfi2xsh_nt2_o1_1:bfi2xsh_nt2_o1_19,
                   bfi2xsh_nt2_o2_1:bfi2xsh_nt2_o2_19,
                   bfi2xsh_t1_o1_1:bfi2xsh_t1_o1_19,
                   bfi2xsh_t1_o2_1:bfi2xsh_t1_o2_19,
                   bfi2xsh_t2_o1_1:bfi2xsh_t2_o1_19,
                   bfi2xsh_t2_o2_1:bfi2xsh_t2_o2_19),
                 names_sep = "_",
                 names_to = c("drop1", "bfi_target", "target_order", "bfi_number")) %>%
    filter(!is.na(value)) %>% 
    ## Had to drop target order to get long-format with number correspondence between
    ## self and targets to work; will add in through a different variable or manually
    select(-drop1, -target_order) %>% 
    pivot_wider(names_from = bfi_target, values_from = value) %>%
    pivot_longer(nt1:t2) %>% 
    rename(target_bfi_value = value,
           target_number_in_condition = name) %>%
    filter(!is.na(target_bfi_value)) %>% 
    pivot_longer(c(mancheck_t1_o1, mancheck_t2_o1, mancheck_t1_o2, mancheck_t2_o2,
                   mancheck_nt1_o1, mancheck_nt2_o1, mancheck_nt1_o2, mancheck_nt2_o2),
                 names_sep = "_",
                 names_to = c("drop2", "mancheck_target", "condition_order")) %>% 
    rename(mancheck_score = value) %>% 
    filter(!is.na(mancheck_score)) %>% 
    pivot_longer(c(political_ident_t_o1, political_ident_t_o2,
                   political_ident_nt_o1, political_ident_nt_o2),
                 names_sep = "_",
                 names_to = c("drop3", "drop4", "political_ident_targ", 
                              "political_ident_order")) %>% 
    ## dropping order from identification because we do not need it anymore
    select(-drop2, -drop3, -drop4, -political_ident_targ, -political_ident_order) %>% 
    rename(political_ident = value) %>% 
    filter(!is.na(political_ident)) %>% 
    pivot_longer(covid1_t1_o1:covid3_nt1_o1,
                 names_sep = "_",
                 names_to = c("covid_q_num", "target_num_covid_q", "order_covid_q")) %>% 
    select(-target_num_covid_q, -order_covid_q) %>% 
    filter(!is.na(value)) %>% 
    pivot_wider(names_from = covid_q_num, values_from = value) %>% 
    mutate(mancheck_target = as.factor(mancheck_target),
           condition_order = as.factor(condition_order),
           target_bfi_number = as.factor(bfi_number),
           bfi_number = as.factor(bfi_number),
           cand_pref = as.numeric(cand_pref),
           political_ident = as.factor(political_ident),
           target_number_in_condition = as.factor(target_number_in_condition)) %>% 
    ## Getting one manipulation check var with correct value associations & filtering to correct
    mutate(mancheck = ifelse(mancheck_target == "t1" & 
                               mancheck_score == 1 | 
                               mancheck_target == "t2" & 
                               mancheck_score == 1, 
                             "correct",
                             ifelse(mancheck_target == "t1" & 
                                      mancheck_score == 2 | 
                                      mancheck_target == "t2" & 
                                      mancheck_score == 2, 
                                    "incorrect",
                                    ifelse(mancheck_target == "nt1" & 
                                             mancheck_score == 2 | 
                                             mancheck_target == "nt2" & 
                                             mancheck_score == 2, 
                                           "correct",
                                           ifelse(mancheck_target == "nt1" & 
                                                    mancheck_score == 1 | 
                                                    mancheck_target == "nt2" & 
                                                    mancheck_score == 1, 
                                                  "incorrect", NA)))),
           target_number_collapsed = as.factor(ifelse(target_number_in_condition == "nt1" | 
                                                        target_number_in_condition == "t1", 
                                                      "target_1", 
                                                      ifelse(target_number_in_condition == "nt2" | 
                                                               target_number_in_condition == "t2", 
                                                             "target_2", NA))),
           target_group = as.factor(ifelse(target_number_in_condition == "t1" | 
                                             target_number_in_condition == "t2", 
                                           "Trump Supporter Target",
                                           ifelse(target_number_in_condition == "nt1" | 
                                                    target_number_in_condition == "nt2", 
                                                  "Not Trump Supporter Target", NA))),
           cand_pref = as.factor(recode(cand_pref,
                               `-0.5` = "Trump Supporter",
                               `0.5` = "Not Trump Supporter")),
           gender = as.factor(recode(gender,
                                     `1` = "Female",
                                     `2` = "Male",
                                     `3` = "Non-Binary",
                                     `4` = "Self-describe",
                                     `5` = "Prefer not to say")),
           ethnicity = as.factor(recode(ethnicity,
                                        `1` = "American Indian/Alaska Native",
                                        `8` = "Asian",
                                        `9` = "Black/African American",
                                        `10` = "Hispanic or Latino/a",
                                        `12` = "Middle Eastern or North African",
                                        `14` = "Native Hawaiian or Pacific Islander",
                                        `16` = "White",
                                        `17` = "Other",
                                        `18` = "Prefer not to answer")),
           covid1 = as.factor(recode(covid1,
                                     `1` = "Had or knew someone with Covid",
                                     `3` = "Did not have or know")),
           covid2 = as.numeric(recode(covid2,
                                      `1` = "1", # 0 to 2 people
                                      `4` = "2", # 3 to 6 people
                                      `5` = "3", # 7 to 10 people
                                      `6` = "4", # 11 to 15 people 
                                      `7` = "5", # 16 to 20 people
                                      `8` = "6")), # 21 or more people
           covid3 = as.factor(recode(covid3,
                                     `1` = "Health",
                                     `4` = "Economic",
                                     `5` = "Neither",
                                     `6` = "Both")),
           political_ident = as.factor(recode(political_ident,
                                              `1` = "conservative",
                                              `2` = "progressive/liberal",
                                              `3` = "moderate",
                                              `4` = "other"))) %>%  
    filter(mancheck == "correct") %>% 
    select(-mancheck_score, -mancheck_target) %>% 
    mutate(ingroup_ident_c = ingroup_ident - mean(ingroup_ident, na.rm = TRUE),
           threat_c = threat - mean(threat, na.rm = TRUE),
           self_c = self - mean(self, na.rm = TRUE))
}

# long_data <- wide_to_long(wide_data_clean)
