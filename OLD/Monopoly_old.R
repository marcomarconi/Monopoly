
{
  
  Citizen <- setRefClass("Citizen",
                         fields = list(
                           id = "integer",
                           money = "numeric",
                           food  = "numeric",
                           energy  = "numeric"
                         ),
                         methods = list(
                           get_id = function() {return(id)}  
                         )
  )
  
  CentralBank <- setRefClass("CentralBank",
                             fields = list(
                               money_supply = "numeric",
                               interest_rate  = "numeric",
                               future  = "numeric"
                             ),
                             methods = list(
                               get_money_supply = function() {return(money_supply)}  
                             )
  )
  
  AgricultureProducer <- setRefClass("AgricultureProducer",
                                     fields = list(
                                       money = "numeric",
                                       supply = "numeric",
                                       price = "numeric",
                                       cost_per_unit = "numeric",
                                       employees  = "numeric",
                                       future  = "numeric"
                                     ),
                                     methods = list(
                                       get_employees = function() {return(employees)}  
                                     )
  )
  
  EnergyProducer <- setRefClass("EnergyProducer",
                                fields = list(
                                  production = "numeric",
                                  employees  = "numeric",
                                  future  = "numeric"
                                ),
                                methods = list(
                                  get_employees = function() {return(employees)}  
                                )
  )
  
  sigmoid <- function(x, a=0, r=1) {
    return(1 / (1 + exp(r*(x-a))))
  }
  
  { 
    # Total time
    iterations <- 52*10
    # Citizen parameters
    num_citizens <- 100
    citizen_init_money <- 10000
    citizen_init_food <- 4
    citizen_init_energy <- 4
    citizen_food_consumption <- 1
    citizen_food_max_price_perc <- 0.5 * 100
    citizen_energy_consumption <- 0
    # Government parameters
    governement_init_money <- 10000 * num_citizens
    # Initial money
    init_money <- num_citizens * citizen_init_money + governement_init_money
    # Agricultural parameters
    agricoltural_init_money <- 100000
    agricoltural_supply <- num_citizens * citizen_food_consumption 
    agricoltural_cost_per_unit <- 0.01
    
    citizens <- list()
    for(i in 1:num_citizens) {
      food <- rpois(1, citizen_init_food)
      citizens[[i]] <-  Citizen$new(id=i, money=citizen_init_money, food=food, energy=citizen_init_energy)
    }
    central_bank <- CentralBank$new(money_supply = init_money, interest_rate = 0)  
    agriculture <- AgricultureProducer$new(money = agricoltural_init_money, supply = agricoltural_supply, price = 1, cost_per_unit = agricoltural_cost_per_unit)
    
    agriculture_price_history <- rep(NA, iterations)
    citizens_money_history <- rep(NA, iterations)
    for(i in 1:iterations) {
      agriculture_price_history[i] <- agriculture$price
      citizens_money_history[i] <- sum(unlist(lapply( citizens, function(x) x$money)))
      
      agricolture_supply <- agriculture$supply
      agriculture$money <- agriculture$money - agricolture_supply * agriculture$cost_per_unit
      food_demand <- rep(0, num_citizens)
      price_ok <- rep(0, num_citizens)
      for(j in 1:num_citizens) {
        food_demand[j] <- rpois(1, max(1, citizen_food_consumption * 4 - citizens[[j]]$food ))
        if(food_demand[j] > 0) 
          price_ok[j] <- rbinom(1, 1, sigmoid((agriculture$price * food_demand[j]  / citizens[[j]]$money) * 100, a=citizen_food_max_price_perc, r=0.1))
        else
          price_ok[j] <- 0
        if(price_ok[j]) {
          transaction <- food_demand[j] * agriculture$price
          citizens[[j]]$food <- citizens[[j]]$food + food_demand[j]
          citizens[[j]]$money <- citizens[[j]]$money - transaction
          agriculture$money <- agriculture$money + transaction
        }
        citizens[[j]]$food <- citizens[[j]]$food - citizen_food_consumption
      }
      agricolture_demand <- sum(food_demand)
      agricolture_buy <- sum(price_ok * food_demand)
      price_perc_increase <- agricolture_buy / agricolture_demand - 0.5
      agriculture$price <- agriculture$price + agriculture$price * price_perc_increase
      
    }
    
  }
  
  
  {
    # Total time
    iterations <- 52*2
    # 
    num_citizens <- 100
    social_classes <- matrix(NA, iterations, 4)
    social_classes[1,] <- c(0.98, 0.01, 0.01, 0)
    money_unemployed <- rep(NA, iterations)
    money_unemployed[1] <- 1000*num_citizens*social_classes[1,1]
    income_citizens <- rep(NA, iterations)
    income_citizens[1] <- 1
    money_workers <- rep(NA, iterations)
    money_workers[1] <- 1000*num_citizens*social_classes[1,2]
    money_employers <- rep(NA, iterations)
    money_employers[1] <- 1000*num_citizens*social_classes[1,3]
    workers_part <- rep(NA, iterations)
    workers_part[1] <- 0.1
    workers_wage <- rep(NA, iterations)
    workers_wage[1] <- 1000
    staple_money <- rep(NA, iterations)
    staple_money[1] <- 1000000
    staple_price <- rep(NA, iterations)
    staple_price[1] <- 5
    staple_bought <- rep(NA, iterations)
    staple_bought[1] <- 0
    consumer_price <- rep(NA, iterations)
    consumer_price[1] <- 5
    consumer_bought <- rep(NA, iterations)
    consumer_bought[1] <- 0
    government_money <- rep(NA, iterations)
    government_money[1] <- 1000000
    government_subsidy <- rep(NA, iterations)
    government_subsidy[1] <- 0
    tax_level <- rep(NA, iterations)
    tax_level[1] <- 0
    tax_collection <- rep(NA, iterations)
    tax_collection[1] <- 0
    total_money <- rep(NA, iterations)
    total_money[1] <-  money_citizens[1] + government_money[1]
    for(i in 2:iterations) {
      unemployed_p <- social_classes[i-1, 1]
      workers_p <- social_classes[i-1, 2]
      employers_p <- social_classes[i-1, 3]
      
      income_per_capita <- income_citizens[i-1] / num_citizens
      staple_bought[i] <- staple_price[i-1]  * num_citizens
      consumer_bought[i] <- consumer_price[i-1]  * num_citizens
      
      government_subsidy[i] <- (staple_bought[i] + consumer_bought[i]) * unemployed_p #  (0.001 + exp(-2 * money_unemployed[i-1] / staple_bought[i-1])) * government_money[i-1]
      workers_wage[i] <- workers_part[i-1] * money_employers[i-1]
      income_citizens[i] <- government_subsidy[i] + workers_wage[i] 
      
      money_unemployed[i] <- money_unemployed[i-1] + government_subsidy[i] - (staple_bought[i] + consumer_bought[i]) * unemployed_p
      money_workers[i] <- money_workers[i-1] +  workers_wage[i] - (staple_bought[i] + consumer_bought[i]) * workers_p
      money_employers[i] <- money_employers[i-1] + consumer_bought[i] * (unemployed_p + workers_p) - workers_wage[i] -  staple_bought[i]*employers_p - tax_collection[i-1]
      government_money[i] <- government_money[i-1] + staple_bought[i] - government_subsidy[i] + tax_collection[i-1]
      
      tax_level[i] <-  exp(-2* government_money[i-1]/total_money[i-1])
      tax_collection[i] <- money_employers[i]  * tax_level[i-1]
      workers_part[i] <- workers_part[i-1] + 0.001 * log(money_employers[i] / money_employers[i-1]) + 0.001 * (1 / workers_p) - 0.001 * workers_part[i-1]
      
      staple_price[i] <- staple_price[i-1] + 1 * log(income_citizens[i] / income_citizens[i-1]) 
      consumer_price[i] <- consumer_price[i-1] + ( 0.1 * (money_unemployed[i-1] + money_workers[i-1]) / total_money[i-1])   + (0.01 * (1 / employers_p)) - 0.001 * consumer_price[i-1]
      
      new_employers <- 0.001 * unemployed_p + 0.05 * (money_employers[i-1] / total_money[i-1])
      lost_employers <- 0.05 * employers_p
      new_workers <- 0.02 * (workers_wage[i] / workers_p) / (government_subsidy[i] / unemployed_p) 
      lost_workers <- 0.01 * workers_p 
      social_classes[i, 3] <- social_classes[i-1, 3] + new_employers - lost_employers
      social_classes[i, 2] <- social_classes[i-1, 2] + new_workers   - lost_workers
      social_classes[i, 1] <- social_classes[i-1, 1] - new_employers - new_workers + lost_employers + lost_workers
      total_money[i] <- money_unemployed[i] + money_workers[i] + money_employers[i] + government_money[i]
    }
    
  }
  
}
