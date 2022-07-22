plan <-
  oa.design(factor.names = factor_definitions) %>%
  as_tibble() %>%
  arrange(Blocks) %>%
  group_by(Blocks) %>%
  mutate(Scenario = row_number()) %>%
  ungroup() %>%
  relocate(Scenario, .after = Blocks)


plan <- plan %>% 
  mutate(comfort_pt=fct_rev(comfort_pt)) %>% 
  mutate(across(where(is.factor), as.numeric))


scenario1<-plan %>% slice(1)
scenario2<-plan %>% slice(2)


my_function <- function(scenario1,scenario2){
  s1_pt<-scenario1 %>% select(ivt_pt,wt_pt,n_xfer_pt,comfort_pt,fare_pt) %>%  unlist(use.names = FALSE)
  s2_pt<-scenario2 %>% select(ivt_pt,wt_pt,n_xfer_pt,comfort_pt,fare_pt) %>%  unlist(use.names = FALSE)
  ifelse(all(s1_pt > s2_pt), "all elements are greater", "not all elements are greater")
}