## wrtie optimization function
function solar_opt(ID_G, i)
        
    #select geography location
    g = ceil(Int, i/3) 
    sol_id = (ID_G[g,3]*"_"*ID_G[g,4])

    #select load case
    if i % 3 == 1 
        case = "_BASE"
    elseif i % 3 == 2
        case = "_LOW"
    else 
        case = "_HIGH"
    end

    # Load in solar and load data
    data = load(DIR * INPUT * sol_id * case *  ".csv") |> DataFrame
    sol = data[:,4]
    load_v = data[:,5]
    tot_load = sum(load_v)
    min_l = minimum(load_v)
    min_l = min(min_l,0.3)

    #length optimization
    tot_leng = length(load_v)

    # Set-up optimization model
    m = Model(Clp.Optimizer) 

    @variables m begin
        x >= 0. ## solar capacity
        y >= 0. ## storage capacity
        power_in[i=1:tot_leng] >= 0. ## charging amount
        power_out[i=1:tot_leng] >= 0. ## discharging amount
        bat_chg[i=1:(tot_leng+1)] >= 0. ## battery state of charge
        shed[i=1:tot_leng] >= 0. ## total load shed with battery
        pv_curtail[i=1:tot_leng] >= 0.  ## total extra pv with battery
    end

    @constraints(m, begin
        charging[1], bat_chg[1] == 0 ## beginning state of charge
        charging2[i=1:tot_leng], bat_chg[i+1] == bat_chg[i] + BAT_EFF*power_in[i] - power_out[i]/BAT_EFF ## shows battery cycling
        charging3[i=1:tot_leng], bat_chg[i] <= y ## can't have a higher charge than capacity of battery
        powering[i=1:tot_leng], power_in[i] <= y/4 # rate allowed to charge battery (4 hr battery)
        depowering[i=1:tot_leng], power_out[i] <= y/4 # rate allowed to discharge battery (4 hr battery)
        balance[i=1:tot_leng], load_v[i]  == power_out[i] - power_in[i] + (sol[i] * x) + shed[i] - pv_curtail[i] ## load balance equation
        sum(shed[i] for i in 1:tot_leng) <= tot_load * LOAD_SHED ## reliability constraint
        #reversing[i=1:tot_leng], pv_curtail[i] <= sol[i] * x  ## can only curtail power which is available and not being used
        #shedding[i=1:tot_leng], shed[i] <= load_v[i] ## can't shed load you don't have
        #min_load[i=1:tot_leng], load_v[i] - shed[i] >= min_l ## always have a base load on
    end)

    @objective(m, Min, x * PV_COST * PV_RATE + y * BAT_COST * BAT_RATE + sum(power_out[i] for i in 1:tot_leng) * 0.001) 

    optimize!(m)
    
    #optimization solution
    result = DataFrame(Dict(:pv=>JuMP.value.(x),:storage=>JuMP.value.(y),:id=>string(LOAD_SHED) * "_" * sol_id * case,:ann_cost=>JuMP.objective_value(m),:load=>tot_load,:solar_tot=>JuMP.value.(x) * sum(sol),:shed_tot=>sum(JuMP.value.(shed))))
    
    outcome = DataFrame(Dict(:sol=>sol*JuMP.value.(x),:pv_curt=>JuMP.value.(pv_curtail),:load=>load_v,:id=>string(LOAD_SHED) * "_" * sol_id * case,:power_in=>JuMP.value.(power_in),:power_out=>JuMP.value.(power_out),:shed=>JuMP.value.(shed),:bat_chg=>JuMP.value.(bat_chg[2:length(bat_chg)])))
     
    #output
    save(DIR * OUT * "summary\\summary_" * string(LOAD_SHED) * "_" * sol_id * case * ".csv", result)
    save(DIR * OUT * "outcome\\outcome_" * string(LOAD_SHED) * "_" * sol_id * case * ".csv", outcome)
    
end