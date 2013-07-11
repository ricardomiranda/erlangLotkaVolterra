%% @author Ricardo C. Miranda
%% @doc Project to exemplify actors model,
%% using a simple Lotka-Volterra (prey-predator) model
%% @copyright 2013 Ricardo C. Miranda, license GNU GPL 
%% @version 0.1

-module(predators).
-export([loop/0]).
-include("records.hrl").

death(#population{} = Predators_P, Delta_T) -> -1.0 * Predators_P#population.decrease_rate * Predators_P#population.population_size * Delta_T.

increase(#population{} = Prey_P,
         #population{} = Predators_P, 
         Delta_T) ->  
    Predators_P#population.increase_rate * Predators_P#population.population_size * Prey_P#population.population_size * Delta_T.

population(#population{} = Prey_Population,
           #population{} = Predators_Population, 
           Delta_T) -> 
    New_Predators_Population_Size = death(Predators_Population, Delta_T) 
                                  + increase(Prey_Population, Predators_Population, Delta_T) 
                                  + Predators_Population#population.population_size,
                                  
    Predators_Population#population{population_size = New_Predators_Population_Size}.

loop()->
    receive
        {From,
         #population{} = Prey_Population,
         #population{} = Predators_Population, 
         Delta_T} ->
            From ! population(Prey_Population, Predators_Population, Delta_T),
            loop()
    end.


