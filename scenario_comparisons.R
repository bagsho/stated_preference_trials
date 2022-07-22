library(tidyverse)
library(DoE.base)

level_pct<-c(1,1.5,2)
factor_definitions <- list(
  Blocks = 1:3,
  ivt_pt = 25*level_pct,
  wt_pt = 10*level_pct,
  n_xfer_pt = c(0, 1,2),
  comfort_pt = c("uncomfortable", "comfortable"),
  cost_car = 70*level_pct,
  fare_pt = c(6.50, 13.00, 20.00),
  ivt_car=30*level_pct,
  n_pax_car = c("1-2", "3-4")
)

plan_initial <-
  oa.design(factor.names = factor_definitions) %>%
  as_tibble() %>%
  arrange(Blocks) %>%
  group_by(Blocks) %>%
  mutate(Scenario = row_number()) %>%
  ungroup() %>%
  relocate(Scenario, .after = Blocks)


plan <- plan_initial %>% 
  mutate(comfort_pt=fct_rev(comfort_pt)) %>% 
  mutate(across(where(is.factor), as.numeric))

results <- tibble(
  Block = numeric(),
  Scenario1 = numeric(),
  Scenario2 = numeric(),
  Pt_situation = character(),
  Car_situation = character()
)


# define function
my_function <- function(scenario1,scenario2){
  
  # PT comparisons
  s1_pt<-scenario1 %>% select(ivt_pt,wt_pt,n_xfer_pt,comfort_pt,fare_pt) %>%  unlist(use.names = FALSE)
  s2_pt<-scenario2 %>% select(ivt_pt,wt_pt,n_xfer_pt,comfort_pt,fare_pt) %>%  unlist(use.names = FALSE)
  Pt_situation <- ifelse(
    all(s1_pt <= s2_pt), 
    "superior", 
    ifelse(all(s1_pt >= s2_pt), "inferior", "other"))
  
  # car comparisons
  s1_car<-scenario1 %>% select(cost_car,ivt_car,n_pax_car) %>%  unlist(use.names = FALSE)
  s2_car<-scenario2 %>% select(cost_car,ivt_car,n_pax_car) %>%  unlist(use.names = FALSE)
  Car_situation <- ifelse(
    all(s1_car <= s2_car), 
    "superior", 
    ifelse(all(s1_car >= s2_car), "inferior", "other"))
  
  # create resulting row
  result <- tibble(
    Block=scenario1$Blocks,
    Scenario1=scenario1$Scenario,
    Scenario2=scenario2$Scenario,
    Pt_situation,
    Car_situation
  )
  
  # return output
  result
}


# for loop
for (i in 1:36){
  scenario1<-plan %>% slice(i)
  for (j in 1:36){
    scenario2<-plan %>% slice(j)
    if (scenario1$Blocks==scenario2$Blocks &
        scenario1$Scenario!=scenario2$Scenario) {
      results <- results %>% bind_rows(my_function(scenario1,scenario2))
    }
    
  }
}




output <- results %>% 
  filter(
    (Pt_situation=="inferior"&Car_situation=="superior")|
    (Pt_situation=="superior"&Car_situation=="inferior")
  ) 

openxlsx::write.xlsx(output,"output.xlsx")
openxlsx::write.xlsx(plan_initial,"plan.xlsx")