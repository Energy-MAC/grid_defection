##run
# cd("C:\\Users\\wgorman\\Desktop\\grid_defection\\")
# cd("C:\\Users\\Will\\Desktop\\grid_defection\\")
## set up inputs
addprocs()

@everywhere using JuMP, DataFrames, Gurobi, FileIO, TextParse, CSVFiles

# Set-working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\"
#DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\"
@everywhere DIR = "G:\\Team Drives\\grid_defect_data\\"
@everywhere OUT = "Analysis\\out"
@everywhere INPUT = "Analysis\\in"

##### CREATE MODEL RUN ######
#Set Case
@everywhere BAT_COST = 100 # $/kWh
@everywhere PV_COST = 500 # $/kW
@everywhere LOAD_SHED = 0
@everywhere case = "BASE_" # "LOW_" # "HIGH_"

# Set constants
@everywhere INV_COST = 150 # $/kW
@everywhere BAT_EFF = 0.92
#annual rates
@everywhere int_rate = 0.03 # percentage interest rate
@everywhere bat_life = 20 # years
@everywhere sol_life = 25 # years
@everywhere inv_life = 10 # years
@everywhere BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
@everywhere PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life));
@everywhere INV_RATE = int_rate / (1 - (1+int_rate)^(-inv_life));

# identify geography IDs to loop through
@everywhere ID_G = load(DIR * INPUT * "\\optimization_list.csv") |> DataFrame

#include model
@everywhere include("optimization_model.jl")

# Create parallelization
time = @time pmap(1:nrow(ID_G)) do i 
    solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, INV_COST, INV_RATE, DIR, INPUT, OUT, i) 
    gc()
end
