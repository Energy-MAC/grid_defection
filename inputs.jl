##run
# cd("G:\\My Drive\\Solar & storage paper\\Code & analysis\\reliability\\grid_defection")
# include("inputs.jl")

## set up inputs
using Distributed

addprocs(3)

@everywhere using  DataFrames, Clp, FileIO, TextParse, CSVFiles, JuMP

# Set-working directory
@everywhere DIR = "G:\\My Drive\\Solar & storage paper\\Code & analysis\\reliability\\"
@everywhere OUT = "out\\defection\\"
@everywhere INPUT = "in\\sol_load\\tmy\\csv\\"

##### CREATE MODEL RUN ######
#Set Case
@everywhere BAT_COST = 400 # $/kWh
@everywhere PV_COST = 1200 # $/kW
@everywhere LOAD_SHED = 0.05

# Set constants
@everywhere BAT_EFF = 0.92
#annual rates
@everywhere int_rate = 0.04 # percentage interest rate
@everywhere bat_life = 10 # years
@everywhere sol_life = 25 # years
@everywhere BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
@everywhere PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life));

# identify geography IDs to loop through
@everywhere ID_G = load(DIR * INPUT * "\\optimization_list.csv") |> DataFrame

#include model
@everywhere include("optimization_model.jl")

# Create parallelization
time = @time pmap(1:(nrow(ID_G)*3)) do i 
    solar_opt(ID_G, i) 
    print(i)
    GC.gc()
end
