simulation_seir_model <- function(
  start_date = NULL, # Start date of the simulation
  amount_of_days=30,
  days_with_new_r0=list(),
  initial_susceptible_individuals = 10374822, #https://en.wikipedia.org/wiki/Portuguese_people
  initial_exposed_individuals = 0,
  initial_infectious_individuals = 1,
  initial_recovered_individuals = 0,
  initial_death_individuals = 0,
  initial_r0 = 2.6,
  infection_rate = 1/5.2,
  resolved_rates = NULL
){
  if(is.null(resolved_rates)){
    resolved_rates = list(
      "mild" = list(
        "recovery" = list(
          "percentage" = 0.81*0.99,
          "rate" = 1/14
        ),
        "death" = list(
          "percentage" = 0.81*0.01,
          "rate" = 1/35
        )
      ),
      "severe" = list(
        "recovery" = list(
          "percentage" = 0.14*0.95,
          "rate" = 1/35
        ),
        "death" = list(
          "percentage" = 0.14*0.05,
          "rate" = 1/40
        )
      ),
      "critical" = list(
        "recovery" = list(
          "percentage" = 0.05*0.3,
          "rate" = 1/40
        ),
        "death" = list(
          "percentage" = 0.05*0.7,
          "rate" = 1/40
        )
      )
    )
  }
  
  average_resolved_rate = resolved_rates[["mild"]][["recovery"]][["percentage"]]*resolved_rates[["mild"]][["recovery"]][["rate"]] +
  resolved_rates[["mild"]][["death"]][["percentage"]]*resolved_rates[["mild"]][["death"]][["rate"]] +
  resolved_rates[["severe"]][["recovery"]][["percentage"]]*resolved_rates[["severe"]][["recovery"]][["rate"]] +
  resolved_rates[["severe"]][["death"]][["percentage"]]*resolved_rates[["severe"]][["death"]][["rate"]] +
  resolved_rates[["critical"]][["recovery"]][["percentage"]]*resolved_rates[["critical"]][["recovery"]][["rate"]] +
  resolved_rates[["critical"]][["death"]][["percentage"]]*resolved_rates[["critical"]][["death"]][["rate"]]
  
  susceptible_individuals = rep(0, amount_of_days)
  exposed_individuals = rep(0, amount_of_days)
  infectious_individuals = rep(0, amount_of_days)
  removed_individuals = rep(0, amount_of_days)
  recovered_individuals = rep(0, amount_of_days)
  recovered_individuals[1] = 0
  dead_individuals = rep(0, amount_of_days)
  dead_individuals[1] = 0
  all_population_control = rep(0, amount_of_days)
  
  
  susceptible_individuals[1] = initial_susceptible_individuals 
  infectious_individuals[1] = initial_infectious_individuals
  exposed_individuals[1] = initial_exposed_individuals
  dead_individuals[1] = initial_death_individuals
  recovered_individuals[1] = initial_recovered_individuals
  
  all_population_control[1] = 0
  
  removed_individuals[1] = initial_death_individuals + initial_recovered_individuals
  
  current_transmission_rate = initial_r0 * average_resolved_rate
  
  all_population = susceptible_individuals[1] + exposed_individuals[1] + infectious_individuals[1] + dead_individuals[1] + recovered_individuals[1]
  
  all_population_control[all_population]
  
  for(step in 1:(amount_of_days-1)){
    if(step %in% names(days_with_new_r0)){
      current_transmission_rate = days_with_new_r0[[as.character(step)]]*average_resolved_rate
    }
    
    
    new_exposed_individuals = infectious_individuals[step]*current_transmission_rate*(susceptible_individuals[step]/all_population)
    
    new_infectious_individuals = infection_rate*exposed_individuals[step]
    new_removed_individuals = average_resolved_rate*infectious_individuals[step]
    
    susceptible_individuals[step + 1] = susceptible_individuals[step] - new_exposed_individuals
    exposed_individuals[step + 1] = exposed_individuals[step] + new_exposed_individuals - new_infectious_individuals
    infectious_individuals[step + 1] = infectious_individuals[step] + new_infectious_individuals - new_removed_individuals#dead_individuals[step] - recovered_individuals[step]
    
    #if(step >=14){
    new_recovered = infectious_individuals[step]*resolved_rates[["mild"]][["recovery"]][["percentage"]]*resolved_rates[["mild"]][["recovery"]][["rate"]] +
      infectious_individuals[step]*resolved_rates[["severe"]][["recovery"]][["percentage"]]*resolved_rates[["severe"]][["recovery"]][["rate"]] +
      infectious_individuals[step]*resolved_rates[["critical"]][["recovery"]][["percentage"]]*resolved_rates[["critical"]][["recovery"]][["rate"]]
    new_deaths = infectious_individuals[step]*resolved_rates[["mild"]][["death"]][["percentage"]]*resolved_rates[["mild"]][["death"]][["rate"]] +
      infectious_individuals[step]*resolved_rates[["severe"]][["death"]][["percentage"]]*resolved_rates[["severe"]][["death"]][["rate"]] +
      infectious_individuals[step]*resolved_rates[["critical"]][["death"]][["percentage"]]*resolved_rates[["critical"]][["death"]][["rate"]]
    new_removed_individuals = average_resolved_rate*infectious_individuals[step]
    #}else{
    #  new_recovered = 0
    #  new_deaths = 0
    #  new_removed_individuals = 0
    #}
    
    recovered_individuals[step + 1] = new_recovered + recovered_individuals[step]
    dead_individuals[step + 1] =new_deaths + dead_individuals[step]
    
    removed_individuals[step + 1] = removed_individuals[step] + new_removed_individuals
    
    all_population_control[step + 1] = susceptible_individuals[step + 1] + exposed_individuals[step + 1] +  infectious_individuals[step + 1] + dead_individuals[step + 1] + recovered_individuals[step + 1]
  }
  
  is_valid_date = !is.na(lubridate::parse_date_time(start_date,"ymd"))
  if(!is.null(start_date) && is_valid_date){
    days = 0:(amount_of_days-1) + as.Date(start_date)
  }else{
    days = 1:amount_of_days
  }
  
  simulation_data = data.frame(
    days = days,
    susceptible_individuals = susceptible_individuals,
    exposed_individuals = exposed_individuals,
    infectious_individuals = infectious_individuals,
    recovered_individuals = recovered_individuals,
    dead_individuals = dead_individuals,
    all_population_control = all_population_control,
    removed_individuals = removed_individuals
  )
  
  return(simulation_data)
  
}

sim_data = simulation_seir_model(
  initial_susceptible_individuals = 10283822, #Portuguese Population, https://www.pordata.pt/Portugal/Popula%C3%A7%C3%A3o+residente++m%C3%A9dia+anual+total+e+por+grupo+et%C3%A1rio-10
  initial_infectious_individuals = initial_infectious_individuals,
  initial_exposed_individuals = 20*initial_infectious_individuals,
  initial_recovered_individuals = 0,
  initial_death_individuals = 0,
  start_date = "2020-02-28",
  amount_of_days = 30*8,
  initial_r0 = base_R_stats$R_ml,
  infection_rate = 1/sars_cov_2_si_mu,
  days_with_new_r0 = changes_over_time
)
