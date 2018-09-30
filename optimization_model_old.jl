#Start parallelization process
addprocs()

@everywhere using JuMP, DataFrames, Gurobi, FileIO, TextParse, CSVFiles

# Set-working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\"
#DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
DIR = "G:\\Team Drives\\grid_defect_data\\Analysis\\"
OUT = "out"
INPUT = "in"

##### CREATE MODEL RUN ######
# Set constants
LOAD_SHED = [0.25, 0]
BAT_COST = 500 # $/kWh
PV_COST = 3000 # $/kW
BAT_EFF = 0.92
#annual rates
int_rate = 0.06 # percentage interest rate
bat_life = 20 # years
sol_life = 25 # years
BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life))

# identify geography IDs to loop through
ID_G = load(DIR * INPUT * "\\optimization_list.csv") |> DataFrame

## wrtie optimization function
function solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, DIR, INPUT, i)
 
    # set-up data collection objects
    result = DataFrame([Float64,Float64,Float64,String,Float64,Float64,Float64,Float64,Float64], [:pv,:storage,:shed_frac,:id, :load, :ann_cost, :solar_tot,:rfp,:shed_tot], 1)
    outcome = DataFrame([Float64, Float64, Float64,Float64, Float64, Float64,Float64, Float64, String], [:sol,:load,:power_in,:power_out,:shed,:pv_rpf,:bat_chg,:shed_frac,:id], 78840)

    #pick reliability level
    rel_length = length(LOAD_SHED)
    if i % rel_length == 0
        index = rel_length
    else
        index = i % 2 
    end
    shed_amt = LOAD_SHED[index]
        
    #select geography location
    g = ceil(Int, i/3) 
    sol_id = (ID_G[g,3]*"_"*ID_G[g,4])

    #select load case
    if i % 3 == 0
        case = "BASE_"
    elseif i % 2 == 0
        case = "LOW_"
    else 
        case = "HIGH_"
    end

    # Load in solar and load data
    data = load(DIR * INPUT * "\\all_data\\" * case * sol_id * ".csv")
    data = DataFrame(data)
    sol = data[:,5]
    load_v = data[:,6]
    tot_load = sum(load_v)

    # Set-up optimization model
    m = Model(solver = GurobiSolver()) # specify the solver, pass along to model

    @variables m begin
        x >= 0. ## solar capacity
        y >= 0. ## storage capacity
        power_in[i=1:78840] >= 0. ## charging amount
        power_out[i=1:78840] >= 0. ## discharging amount
        bat_chg[i=0:78840] >= 0. ## battery state of charge
        shed[i=1:78840] >= 0. ## total load shed with battery
        pv_rpf[i=1:78840] >= 0.  ## total extra pv with battery
    end

    @constraints(m, begin
        charging[0], bat_chg[0] == y / 2 ## beginning state of charge
        charging2[i=1:78840], bat_chg[i] == bat_chg[i-1] + BAT_EFF*power_in[i] - power_out[i]/BAT_EFF ## shows battery cycling
        charging3[i=1:78840], bat_chg[i] <= y ## can't have a higher charge than capacity of battery
        charging4[i=1:78840], bat_chg[i] >= 0 ## can't have a lower charge than 0
        powering[i=1:78840], power_in[i] <= y / 4 # rate allowed to charge battery (4 hr battery)
        depowering[i=1:78840], power_out[i] <= y / 4 # rate allowed to discharge battery (4 hr battery)
        balance[i=1:78840], load_v[i] + BAT_EFF*power_in[i] + pv_rpf[i] == power_out[i]/BAT_EFF + (sol[i] * x) + shed[i] ## load balance equation
        shedding[i=1:78840], shed[i] <= load_v[i] ## can't shed load you don't have
        reversing[i=1:78840], pv_rpf[i] <= sol[i] * x ## can't send power back to grid you don't produce
        sum(shed[i] for i in 1:78840) <= tot_load * shed_amt ## reliability constraint
    end)

    @objective(m, Min, x * PV_COST * PV_RATE + y * BAT_COST * BAT_RATE) 

    status = solve(m)
    
    #optimization solution
    result[1, :pv] = getvalue(x)
    result[1, :storage] = getvalue(y)
    result[1, :shed_frac] = shed_amt
    result[1, :id] = case * sol_id
    result[1, :load] = tot_load
    result[1, :ann_cost] = getobjectivevalue(m)
    result[1, :solar_tot] = getvalue(x) * sum(sol[i] for i in 1:78840)
    result[1, :rfp] = sum(getvalue(pv_rpf[i]) for i in 1:78840)
    result[1, :shed_tot] = sum(getvalue(shed[i]) for i in 1:78840)
    
    #optimization results
    outcome[1:78840, :sol] = sol * getvalue(x)
    outcome[1:78840, :load] = load_v
    outcome[1:78840, :power_in] = getvalue(power_in)
    outcome[1:78840, :power_out] = getvalue(power_out)
    outcome[1:78840, :shed] = getvalue(shed)
    outcome[1:78840, :pv_rpf] = getvalue(pv_rpf)
    outcome[1:78840, :bat_chg] = getvalue(bat_chg[1:78840])
    outcome[1:78840, :shed_frac] = fill(shed_amt,78840)
    outcome[1:78840, :id] = fill(case * sol_id,78840)
     
    return result, outcome
end

#Run Model
results = []
outcomes = [] 
for i in 1:9
    result, outcome = solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, DIR, INPUT, i) 
    results = append!(results, result)
    outcomes = append!(outcomes, outcome)
end

# Create parallelization
outputs = pmap(1:7) do i #
    solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, DIR, INPUT, i) 
end

# Collect results
#write status
array = outputs[1] |> collect
df = DataFrame(status = array) 
save(DIR * OUT * "\\status_v5.csv", df) 

#write results table
array = outputs |> @map(x->x[2]) |> collect
for i = 2:length(array) 
    df = array[1]
    df = append!(df, array[i])
end
save(DIR * OUT * "\\results_v5.csv", df)

#write outcome table
array = outputs |> @map(x->x[3]) |> collect
for i = 2:length(array) 
    df = array[1]
    df = append!(df, array[i])
end
save(DIR * OUT * "\\outcome_v5.csv", df)

function test()

end