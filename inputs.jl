##run
# cd("C:\\Users\\wgorman\\Desktop\\grid_defection\\")
# cd("C:\\Users\\Will\\Desktop\\grid_defection\\")
## set up inputs
using Distributed

addprocs(3)

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
@everywhere PV_COST = 600 # $/kW
@everywhere LOAD_SHED = 0.05

# Set constants
@everywhere BAT_EFF = 0.92
#annual rates
@everywhere int_rate = 0.07 # percentage interest rate
@everywhere bat_life = 20 # years
@everywhere sol_life = 25 # years
@everywhere BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
@everywhere PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life));

# identify geography IDs to loop through
@everywhere ID_G = load(DIR * INPUT * "\\optimization_list.csv") |> DataFrame

#include model
@everywhere include("optimization_model.jl")

# for i = 1:(nrow(ID_G)*3)
#     solar_opt(ID_G, i)
# end

# Create parallelization
time = @time pmap(1:(nrow(ID_G)*3)) do i 
    solar_opt(ID_G, i) 
    print(i)
    GC.gc()
end
