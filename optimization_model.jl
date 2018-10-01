## wrtie optimization function
function solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, DIR, INPUT, i)
 
    # set-up data collection objects
    result = DataFrame([Float64,Float64,Float64,String,Float64,Float64, Float64, Float64,Float64], [:pv,:storage,:shed_frac,:id, :load, :ann_cost, :solar_tot,:pv_curtail,:shed_tot], 1)
    outcome = DataFrame([Float64, Float64, Float64,Float64, Float64, Float64, Float64, Float64, String], [:sol,:load,:power_in,:power_out,:shed,:pv_curtail,:bat_chg,:shed_frac,:id], 78840)

    #pick reliability level
    rel_length = length(LOAD_SHED)
    if i % rel_length == 0
        index = rel_length
    else
        index = i % 2 
    end
    shed_amt = LOAD_SHED[index]
        
    #select geography location
    g = ceil(Int, i/6) 
    sol_id = (ID_G[g,3]*"_"*ID_G[g,4])

    #select load case
    if i % 6 == 1 || i % 6 == 2
        case = "BASE_"
    elseif i % 6 == 3 || i % 6 == 4
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
    m = Model(solver = GurobiSolver()) # ClpSolver() specify the solver, pass along to model

    @variables m begin
        x >= 0. ## solar capacity
        y >= 0. ## storage capacity
        power_in[i=0:78840] >= 0. ## charging amount
        power_out[i=0:78840] >= 0. ## discharging amount
        bat_chg[i=0:78840] >= 0. ## battery state of charge
        shed[i=1:78840] >= 0. ## total load shed with battery
        pv_curtail[i=1:78840] >= 0.  ## total extra pv with battery
    end

    @constraints(m, begin
        charging[0], bat_chg[0] == y / 2 ## beginning state of charge
        charge1[0], power_in[0] == 0
        charge2[0], power_out[0] == 0
        charging2[i=1:78840], bat_chg[i] == bat_chg[i-1] + BAT_EFF*power_in[i-1] - power_out[i-1]/BAT_EFF ## shows battery cycling
        charging3[i=1:78840], bat_chg[i] <= y ## can't have a higher charge than capacity of battery
        powering[i=1:78840], power_in[i] <= y / 4 # rate allowed to charge battery (4 hr battery)
        depowering[i=1:78840], power_out[i] <= y / 4 # rate allowed to discharge battery (4 hr battery)
        balance[i=1:78840], load_v[i]  == power_out[i] - power_in[i] + (sol[i] * x) + shed[i] - pv_curtail[i] ## load balance equation
        sum(shed[i] for i in 1:78840) <= tot_load * shed_amt ## reliability constraint
        #reversing[i=1:78840], pv_curtail[i] <= sol[i] * x ## can't curtail non-produced power
        #shedding[i=1:78840], shed[i] <= load_v[i] ## can't shed load you don't have
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
    result[1, :pv_curtail] = sum(getvalue(pv_curtail[i]) for i in 1:78840)
    result[1, :shed_tot] = sum(getvalue(shed[i]) for i in 1:78840)
    
    #optimization results
    outcome[1:78840, :sol] = sol * getvalue(x)
    outcome[1:78840, :load] = load_v
    outcome[1:78840, :power_in] = getvalue(power_in[1:78840])
    outcome[1:78840, :power_out] = getvalue(power_out[1:78840])
    outcome[1:78840, :shed] = getvalue(shed)
    outcome[1:78840, :pv_curtail] = getvalue(pv_curtail)
    outcome[1:78840, :bat_chg] = getvalue(bat_chg[1:78840])
    outcome[1:78840, :shed_frac] = fill(shed_amt,78840)
    outcome[1:78840, :id] = fill(case * sol_id,78840)
     
    return result, outcome
end