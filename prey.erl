%% @author Ricardo C. Miranda
%% @doc Project to exemplify actors model,
%% using a simple Lotka-Volterra (prey-predator) model
%% @copyright 2013 Ricardo C. Miranda, license GNU GPL 
%% @version 0.1

-module(prey).
-export([loop/0]).
-include("records.hrl").

birth(#population{} = Prey_P, Delta_T) -> Prey_P#population.increase_rate * Prey_P#population.population_size * Delta_T.

destroy(#population{} = Prey_P,
        #population{} = Predators_P, 
        Delta_T) ->  
    -1.0 * Prey_P#population.decrease_rate * Predators_P#population.population_size * Prey_P#population.population_size * Delta_T.

population(#population{} = Prey_Population,
           #population{} = Predators_Population, 
           Delta_T) -> 
    New_Prey_Population_Size = birth(Prey_Population, Delta_T)
                             + destroy(Prey_Population, Predators_Population, Delta_T) 
                             + Prey_Population#population.population_size,
    
    Prey_Population#population{population_size = New_Prey_Population_Size}.

loop()->
    receive
        {From,
         #population{} = Prey_Population,
         #population{} = Predators_Population, 
         Delta_T} ->
            From ! population(Prey_Population, Predators_Population, Delta_T),
            loop()
    end.
