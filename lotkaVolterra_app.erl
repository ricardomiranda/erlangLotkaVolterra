%% @author Ricardo C. Miranda
%% @doc Project to exemplify actors model,
%% using a simple Lotka-Volterra (prey-predator) model
%% @copyright 2013 Ricardo C. Miranda, license GNU GPL 
%% @version 0.1


-module(lotkaVolterra_app).
-include("records.hrl").
-export([calc_populations/0]).

get_Delta_T() -> 
    Input = io:get_line("What is the time step? > "),
    {Delta_T, _}                  = string:to_float(Input),
    Delta_T.

get_Nbr_steps() -> 
    Input = io:get_line("How many time steps to compute? > "),
    {Nbr_steps, _}                = string:to_float(Input),
    Nbr_steps.

get_Ini_Prey_Population() -> 
    Input = io:get_line("What is the incial prey population? > "),
    {Ini_Prey_Population, _}      = string:to_float(Input),
    Ini_Prey_Population.
    
get_Ini_Predators_Population() ->
    Input = io:get_line("What is the incial predators population? > "),
    {Ini_Predators_Population, _} = string:to_float(Input),
    Ini_Predators_Population.

get_Prey_Birth_Rate() ->
    Input = io:get_line("What is the prey birth rate? > "),
    {Prey_Birth_Rate, _}          = string:to_float(Input),
    Prey_Birth_Rate.

get_Prey_Destroy_Rate() ->
    Input = io:get_line("What is the prey destroy rate? > "),
    {Prey_Destroy_Rate, _}        = string:to_float(Input),
    Prey_Destroy_Rate.
    
get_Predators_Death_Rate() ->
    Input = io:get_line("What is the predators death rate > "),
    {Predators_Death_Rate, _}     = string:to_float(Input),
    Predators_Death_Rate.
    
get_Predators_Increase_Rate() ->
    Input = io:get_line("What is the predators increase rate? > "),
    {Predators_Increase_Rate, _}  = string:to_float(Input),
    Predators_Increase_Rate.

%-------------------------------------------------------------------------------
        
calc_populations() ->
    Delta_T       = get_Delta_T(),
    Nbr_steps = get_Nbr_steps(),
    
    Ini_Prey_Population      = get_Ini_Prey_Population(),
    Ini_Predators_Population = get_Ini_Predators_Population(),
        
    Prey_Birth_Rate          = get_Prey_Birth_Rate(),
    Prey_Destroy_Rate        = get_Prey_Destroy_Rate(),
    Predators_Death_Rate     = get_Predators_Death_Rate(),
    Predators_Increase_Rate  = get_Predators_Increase_Rate(),

    Prey_Population      = #population{population_size = Ini_Prey_Population,
                                       increase_rate   = Prey_Birth_Rate,
                                       decrease_rate   = Prey_Destroy_Rate},
    
    Predators_Population = #population{population_size = Ini_Predators_Population,
                                       increase_rate   = Predators_Increase_Rate,
                                       decrease_rate   = Predators_Death_Rate},
    
    PreyID      = spawn(prey,     loop,[]),
    PredatorsID = spawn(predators,loop,[]),

    loop(PreyID, PredatorsID, Prey_Population, Predators_Population, Delta_T, Nbr_steps).

%-------------------------------------------------------------------------------

loop(PreyID,
     PredatorsID,
     #population{} = Prey_Population,
     #population{} = Predators_Population, 
     Delta_T, Nbr_steps) when Nbr_steps > 0.0 ->       
        io:format("Steps to complete: ~w, ",         [Nbr_steps]),
        io:format("Prey population is: ~w, ",        [Prey_Population#population.population_size]),
        io:format("Predators population is: ~w. ~n", [Predators_Population#population.population_size]),

        PreyID !      {self(), Prey_Population, Predators_Population, Delta_T},

        receive
            New_Prey_Population ->      New_Prey_Population
        end,
        
        PredatorsID ! {self(), Prey_Population, Predators_Population, Delta_T},

        receive
            New_Predators_Population -> New_Predators_Population
        end,
        
        loop(PreyID,
             PredatorsID,
             New_Prey_Population,
             New_Predators_Population,
             Delta_T, (Nbr_steps - Delta_T));

loop(_, _, _, _, _, Nbr_steps) when Nbr_steps =< 0.0 -> io:format("Simulation terminated successfully.~n").

