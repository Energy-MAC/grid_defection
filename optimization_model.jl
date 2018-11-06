## wrtie optimization function
function solar_opt(ID_G, i)
 
    # set-up data collection objects
    #result = DataFrame([Float64,Float64,Float64,String,Float64,Float64, Float64, Float64,Float64], [:pv,:storage,:shed_frac,:id, :load, :ann_cost, :solar_tot,:pv_curtail,:shed_tot], 1)
    result = DataFrame([Float64,Float64,Float64], [:pv,:storage,:pv_curtail], 1)
    outcome = DataFrame([Float64, Float64, Float64,Float64, Float64, Float64, Float64], [:sol,:load,:power_in,:power_out,:shed,:pv_curtail,:bat_chg], 78840)
       
    #select geography location
    g = ceil(Int, i/3) 
    sol_id = (ID_G[g,3]*"_"*ID_G[g,4])

    #select load case
    if i % 3 == 1 
        case = "BASE_"
    elseif i % 3 == 2
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
    min_l = minimum(load_v)
    min_l = min(min_l,0.3)

    # Set-up optimization model
    m = Model(solver = GurobiSolver(OutputFlag=0)) 

    @variables m begin
        x >= 0. ## solar capacity
        y >= 0. ## storage capacity
        power_in[i=1:78840] >= 0. ## charging amount
        power_out[i=1:78840] >= 0. ## discharging amount
        bat_chg[i=1:78841] >= 0. ## battery state of charge
        shed[i=1:78840] >= 0. ## total load shed with battery
        pv_curtail[i=1:78840] >= 0.  ## total extra pv with battery
    end

    @constraints(m, begin
        charging[1], bat_chg[1] == y / 2 ## beginning state of charge
        charging2[i=1:78840], bat_chg[i+1] == bat_chg[i] + BAT_EFF*power_in[i] - power_out[i]/BAT_EFF ## shows battery cycling
        charging3[i=1:78840], bat_chg[i] <= y ## can't have a higher charge than capacity of battery
        powering[i=1:78840], power_in[i] <= y/4 # rate allowed to charge battery (4 hr battery)
        depowering[i=1:78840], power_out[i] <= y/4 # rate allowed to discharge battery (4 hr battery)
        balance[i=1:78840], load_v[i]  == power_out[i] - power_in[i] + (sol[i] * x) + shed[i] - pv_curtail[i] ## load balance equation
        sum(shed[i] for i in 1:78840) <= tot_load * LOAD_SHED ## reliability constraint
        #reversing[i=1:78840], pv_curtail[i] <= sol[i] * x  ## can only curtail power which is available and not being used
        #shedding[i=1:78840], shed[i] <= load_v[i] ## can't shed load you don't have
        min_load[i=1:78840], load_v[i] - shed[i] >= min_l ## always have a base load on
    end)

    @objective(m, Min, x * PV_COST * PV_RATE + y * BAT_COST * BAT_RATE + sum(power_out[i] for i in 1:78840) * 0.001) 

    status = solve(m)
    
    #optimization solution
    result[1, :pv] = getvalue(x)
    result[1, :storage] = getvalue(y)
    #result[1, :shed_frac] = shed_amt
    #result[1, :id] = string(LOAD_SHED) * "_" * case * sol_id 
    #result[1, :load] = tot_load
    #result[1, :ann_cost] = getobjectivevalue(m)
    #result[1, :solar_tot] = getvalue(x) * sum(sol[i] for i in 1:78840)
    result[1, :pv_curtail] = sum(getvalue(pv_curtail[i]) for i in 1:78840)
    #result[1, :shed_tot] = sum(getvalue(shed[i]) for i in 1:78840)
    
    #optimization results
     outcome[1:78840, :sol] = sol * getvalue(x)
     outcome[1:78840, :load] = load_v
     outcome[1:78840, :power_in] = getvalue(power_in[1:78840])
     outcome[1:78840, :power_out] = getvalue(power_out[1:78840])
     outcome[1:78840, :shed] = getvalue(shed[1:78840])
     outcome[1:78840, :pv_curtail] = getvalue(pv_curtail)
     outcome[1:78840, :bat_chg] = getvalue(bat_chg[1:78840])
     #outcome[1:78840, :shed_frac] = fill(shed_amt,78840)
     #outcome[1:78840, :id] = fill(case * sol_id,78840)
    GC.gc()
    #output
    save(DIR * OUT * "\\1200pv_400stor\\results_" * string(LOAD_SHED) * "_" * case * sol_id * ".csv", result)
    save(DIR * OUT * "\\1200pv_400stor\\outcome_" * string(LOAD_SHED) * "_" * case * sol_id * ".csv", outcome)
end