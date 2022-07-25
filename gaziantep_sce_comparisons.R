gaziantep <- read_excel("gaziantep.xlsx", col_types = c("numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "text", "numeric", 
                                                "numeric", "text", "numeric"))

plan_gaziantep <- gaziantep %>% 
  mutate(comfort_pt=as.factor(comfort_pt),
         n_pax_car=as.factor(n_pax_car)) %>% 
  #mutate(comfort_pt=fct_rev(comfort_pt)) %>% 
  mutate(across(where(is.factor), as.numeric))


results_gaziantep <- tibble(
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
  scenario1<-plan_gaziantep %>% slice(i)
  for (j in 1:36){
    scenario2<-plan_gaziantep %>% slice(j)
    if (scenario1$Blocks==scenario2$Blocks &
        scenario1$Scenario!=scenario2$Scenario) {
      results_gaziantep <- results_gaziantep %>% bind_rows(my_function(scenario1,scenario2))
    }
    
  }
}

gaziantep_output <- results_gaziantep %>% 
  filter(
    (Pt_situation=="inferior"&Car_situation=="superior")|
      (Pt_situation=="superior"&Car_situation=="inferior")
  ) 

openxlsx::write.xlsx(gaziantep_output,"gaziantep_output.xlsx")

#rm(results_gaziantep)