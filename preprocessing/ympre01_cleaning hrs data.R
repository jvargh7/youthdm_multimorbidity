

col_names = c("general",paste0("wave",sprintf("%02d",1:14)))

g2a_variables = readxl::read_excel("data/Adult DM Multimorbidity Variable List.xlsx",sheet="G2A HRS")
rand_variables = readxl::read_excel("data/Adult DM Multimorbidity Variable List.xlsx",sheet="RAND HRS")

general <- map_dfr(col_names[1],
                   function(c){
                     
                     g2a_vars = g2a_variables %>% 
                       dplyr::select(variable,one_of(c)) %>% 
                       rename_at(vars(one_of(c)),~"selected") %>% 
                       mutate(selected = str_to_lower(selected)) %>% 
                       dplyr::filter(!is.na(selected));
                     
                     rand_vars = rand_variables %>% 
                       dplyr::select(variable,one_of(c)) %>% 
                       rename_at(vars(one_of(c)),~"selected") %>% 
                       mutate(selected = str_to_lower(selected)) %>% 
                       dplyr::filter(!is.na(selected));
                     
                     g2a_df <- haven::read_dta(paste0(path_g2aging_data,"/HarmonizedHRSvC_STATA/H_HRS_c.dta"),
                                               col_select = na.omit(g2a_vars$selected)) %>% 
                       rename_with(~ g2a_vars$variable[which(g2a_vars$selected == .x)], 
                                   .cols = g2a_vars$selected) 
                     
                     rand_df <- haven::read_dta(paste0(path_g2aging_data,"/randhrs1992_2018v2_STATA/randhrs1992_2018v2.dta"),
                                                col_select = na.omit(rand_vars$selected)) %>% 
                       rename_with(~ rand_vars$variable[which(rand_vars$selected == .x)], 
                                   .cols = rand_vars$selected) 
                     
                     wave_df <- full_join(g2a_df,
                                          rand_df,
                                          by=c("study_id","hhid")) %>% 
                       mutate(wave = c)
                     
                     return(wave_df)
                     
                     
                   })



waves <- map_dfr(col_names[-1],
        function(c){
          
          g2a_vars = g2a_variables %>% 
            dplyr::select(variable,one_of(c)) %>% 
            rename_at(vars(one_of(c)),~"selected") %>% 
            mutate(selected = str_to_lower(selected)) %>% 
            dplyr::filter(!is.na(selected));
          
          rand_vars = rand_variables %>% 
            dplyr::select(variable,one_of(c)) %>% 
            rename_at(vars(one_of(c)),~"selected") %>% 
            mutate(selected = str_to_lower(selected)) %>% 
            dplyr::filter(!is.na(selected));
          
          g2a_df <- haven::read_dta(paste0(path_g2aging_data,"/HarmonizedHRSvC_STATA/H_HRS_c.dta"),
                                 col_select = na.omit(g2a_vars$selected)) %>% 
            rename_with(~ g2a_vars$variable[which(g2a_vars$selected == .x)], 
                        .cols = g2a_vars$selected) 
          
          rand_df <- haven::read_dta(paste0(path_g2aging_data,"/randhrs1992_2018v2_STATA/randhrs1992_2018v2.dta"),
                                 col_select = na.omit(rand_vars$selected)) %>% 
            rename_with(~ rand_vars$variable[which(rand_vars$selected == .x)], 
                        .cols = rand_vars$selected) 
          
          wave_df <- full_join(g2a_df,
                               rand_df,
                               by=c("study_id","hhid")) %>% 
            mutate(wave = c)
          
          return(wave_df)
          
          
        })  %>% 
  arrange(study_id,age_years) %>% 
  dplyr::select(study_id,hhid,pn,rand_study_id,wave,age_months,age_years,waveid, 
                sampleweight,householdweight,stratum, personweight,wave_spouseid, year_marriage,
                starts_with("report"),starts_with("medication"),starts_with("agediag"),starts_with("ever"),starts_with("treatment"),everything())

saveRDS(general,paste0(path_multimorbidity_folder,"/working/hrs general.RDS"))


waves %>% 
  dplyr::filter(waveid == 1) %>% 
saveRDS(.,paste0(path_multimorbidity_folder,"/working/hrs multimorbidity.RDS"))
