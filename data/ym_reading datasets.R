

search <- readRDS(paste0(path_ada_proposal,"/working/search/search_multimorbidity.RDS"))
today <- readRDS(paste0(path_ada_proposal,"/working/today/today_multimorbidity.RDS"))

saveRDS(search,paste0(path_multimorbidity_folder,"/working/search.RDS"))
saveRDS(today,paste0(path_multimorbidity_folder,"/working/today.RDS"))

search %>% 
  group_by(study_id) %>% 
  mutate(follow_up = 1:n()) %>% 
  ungroup() %>% 
  group_by(follow_up,type_diabetes) %>% 
  summarize_all(~sum(!is.na(.))) %>% 
  writexl::write_xlsx("data/search variables.xlsx")

today %>% 
  group_by(study_id) %>% 
  mutate(follow_up = 1:n()) %>% 
  ungroup() %>% 
  group_by(follow_up,treatment) %>% 
  summarize_all(~sum(!is.na(.))) %>% 
  writexl::write_xlsx("data/today variables.xlsx")
