##run
# cd("C:\\Users\\wgorman\\Desktop\\grid_defection\\")
# cd("C:\\Users\\Will\\Desktop\\grid_defection\\")
## set up inputs
using JuMP, DataFrames, Gurobi, FileIO, TextParse, CSVFiles

# Set-working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\"
#DIR = "G:\\Team Drives\\grid_defect_data\\"
OUT = "Analysis\\out"
INPUT = "Analysis\\in"

##### CREATE MODEL RUN ######
#Set Case
BAT_COST = 100 # $/kWh
PV_COST = 500 # $/kW
LOAD_SHED = 0.05
case = "BASE_" # "LOW_" # "HIGH_"

# Set constants
INV_COST = 150 # $/kW
BAT_EFF = 0.92
#annual rates
int_rate = 0.03 # percentage interest rate
bat_life = 20 # years
sol_life = 25 # years
inv_life = 10 # years
BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life));
INV_RATE = int_rate / (1 - (1+int_rate)^(-inv_life));

# identify geography IDs to loop through
ID_G = load(DIR * INPUT * "\\optimization_list.csv") |> DataFrame

#include model
include("optimization_model.jl")

#Run Model
for i in 1:3 #nrow(ID_G)
    
    solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, INV_COST, INV_RATE, DIR, INPUT, OUT, i) 
    
    # if (i == 1)
    #     global results = result
    # else
    #     append!(results, result)
    # end

    print(i)
end

#output
#save(DIR * OUT * "\\results_500pv_100stor.csv", results)