// ********************************************************************
// *       SGSS: Smart Grid Systemic Simulation                       *
// *       copyright (C) 2011 Yves Caseau                             *
// *       file: model.cl                                             *
// ********************************************************************

// this file contains the data model for the SGSS simulation project

// ********************************************************************
// *    Part 1: Regulator (Government) & Energy types                 *
// *    Part 2: Households (smart homes) & Cities                     *
// *    Part 3: Local SmartGrid Operators                             *
// *    Part 4: Global Supply side: Energy production                 *
// *    Part 5: Experiments                                           *
// ********************************************************************

TALK:integer :: 5
SHOW:integer :: 5
DEBUG:integer :: 5
OSHOW:integer :: 5              // focus trace (SHOW) on one opertor
CSHOW:integer :: 5

Version:float :: 0.4            // branched on November 4th, 2011 - resume 20/1/2013

NIT:integer :: 20               // number of years

NOP:integer :: 100              // number of smartgrids
CARNOT:float :: 3.0             // Principe de Carnot : 3MWh primaire -> 1 MWh �lectricit�

Time :: integer                 // time is in hour

Percent :: float
Price :: float                  // price is in dollars
Power :: float                  // power is in MW
Energy :: float                 // energy is in GW.h
Measure <: ephemeral_object     // forward


// what is common to all actors
// tactic and strategy are defined locally (specialized)
Player <: object(
      satisfaction:Percent,             // yearly grade
      gradeGM:Measure,                  // record all years grade (during one simulation loop)
      gradeEM:Measure,                  // record all years grade (Experiment: steps average)
      tacticProperties:list<property>,  // list of slots that need to be tuned
      income:Price = 0.0,               // savings from negawatt investments
      expense:Price = 0.0,              // annualized investments
      nashTactic:object,                // copy of the previous tactic (measure of Nash->distance)
      walkTactic:object)                // copy of the previous tactic (for rWalk method - simul.cl)

(instanced(Player))

// forward definition
Supplier <: Player
Operator <: Player
Affine <: object
Household <: thing

// ********************************************************************
// *    Part 1: Regulator (Government) & Energy types                 *
// ********************************************************************

// there are four energy classes : nuclear, PV&wind , Oil&Gaz, Storage
EnergyCapacity <: object(
    cost:Price,         // �/ MWh  - TCO : initial value does not change
    duration:integer,   // lifetime, expressed in Years
    power:Power,        // installed power
    capacity:Energy,    // yearly production capacity, or storage max capacity
    // computed slot
    energy:Energy,      // yearly energy
    fixedCost:Price,    // fixed cost
    invest:Power,       // sum of investments (in MW)
    marginalCost:Price, //
    trend:Percent)      // price trend over the years


(instanced(EnergyCapacity))

// Nuclear : flat price, zero CO2
Nuclear <: EnergyCapacity(
    min%:Percent,       // min capacity (one cannot shut it down)
    dailyMax%:Percent)  // max variation in production from one day to the other

// Oil&Gaz have a fluctuating price
Oil&Gaz <: EnergyCapacity(
    rateYM:Measure,     // Yearly Measure : ratio of (local prod fossile price / wholesale EDF)
    usageYM:Measure)    // measure the average rate of usage

// combination of solar and wind
Green <: EnergyCapacity(
    rateYM:Measure,     // Yearly Measure : ratio of (local prod fossile price / wholesale EDF)
    intermittent:Percent,  // green energy is available for a fraction of the day
    usageYM:Measure,       // measure the averarage rate (actual power / max power)
    // debug yearly
    flowOut:Energy,
    income:Price) 

// Storage is seen as an energy capacity
Storage <: EnergyCapacity(
         // computed slots
        buffer:Energy,          // part of the capacity is used as a production buffer
        reserve:Energy,         // the other part is used as a  reserve
                                // (buy when cheap and use/sell when expensive)
        yearProduction:Energy,  // yearly production (computes the efficiency !)
        // debug yearly (cummulated) data
        flowIn:Energy,
        flowOut:Energy,
        flowR:Energy,           // special tracking of inReserve
        prodCost:Price,         // fixedCost + OPEX
        expense:Price,          // cost of electricity for reserve  (flowR x buy price)
        incomeB:Price,           // money made by the buffer usage (use)
        incomeR:Price)           // money made by the reserve storage (at market price when consumed + used)

self_print(x:Storage) : void
  -> printf("STORE(~I:~I/~I)",pF(x.buffer,3),pF(x.reserve,3),pF(x.capacity,3))

// The regulator represents the government and controls taxes & regulations.

// the strategy of each actor is defined through 3 KPI & their targets
RegulatorStrategy <: object(
    co2Amount:float,        // yealy emissions of CO2
    balance:Price,          // expected balance (CO2 tax - incentives)
    energy:Energy)          // target output (total yearly production) in GWh (without negawatts !)


// The regulator is our first player - its levers (tactic driven) are
//  - CO2 tax (generate cash)   :  � / MWh
//  - green investment support (cost cash)   : Percentage
// <todo> (a) we could add the garanteed acquisition price for green energy (e.g. for Solar)
//        (b) this is a dumb tactic - a real one would react to the strategic goals
RegulatorTactic <: object(
      co2FirstPrice:Price,              // price of CO2 tax
      co2Trend:Percent,                 // yearly trend
      greenShare:Percent,               // share of the green invest that is supported through tax exemption
      greenToStorage:Percent)           // each local opertor must have a storage capacity that is
                                        // associated to green production

ListRegulator :: list<property>(greenShare,co2Trend) // ,greenToStorage)         
 
// assumes for the time being that the start values are strategic parameters (makes tuning easier)

// The regulator's goal is to reduce CO2 emission, encourage green investment, and maintain productivity
// (energy output minus energy saving investments)
//
Regulator <: Player(
      power:Power,                      // max power conso for the country - input parameter
      energy:Energy,                    // yearly conso (to check)
   //   trend:Percent,                    // yearly trend
      yearPattern:Affine,               // energy conso yearly pattern
      nbHouseholds:integer,             // 20 millions in France
      strategy:RegulatorStrategy,
      tactic:RegulatorTactic,
      // status - computed slots
      co2Amount:float = 0.0,
      fixedCost:float = 0.0,            // sum of shared investment
      co2GM:Measure,                    // record CO2 level  (record in GM all KPI relevant to satisfaction)
      balanceGM:Measure,                // KPI #2
      energyGM:Measure,                 // KPI #3
      taxGM:Measure,                    // CO2 tax
      co2EM:Measure,                    // record CO2 level  (record in GM all KPI relevant to satisfaction)
      balanceEM:Measure,                // KPI #2
      energyEM:Measure,                 // KPI #3
      taxEM:Measure,                    // CO2 tax
      negaAlpha:float)                  // coefficient to compute the efficiency of negawatt ...

self_print(x:Regulator) : void -> princ("France")


// ********************************************************************
// *    Part 2: Households (smart homes) & Cities                     *
// ********************************************************************

// Household is a type of home (from a consumption profile perspective)
// in v0.4 there is only one kind of household per city ! (need to speed up)
Household <: thing(
       energyVariation:Percent = 0.0,   // variability of energy demand
       shavingFactor:Percent,           // level of effort at current price,
       shavingSensitivity:Percent,      // derivative of the shaving  S-curve
       shavingCapacity:Percent,         // max fraction of energy consumption that may be shaved
       dayPattern:Affine,               // daily conso pattern for this type
       dayPatterns:list<float>,         // cashed into a list v0.4
       // computed slot
       index:integer = 0,               // ident
       population:integer = 0)              // total population


// local markets are made of housholds (could also include factories) that are
// grouped within cities. We do not simulate each household, but create different
// types, which is a door for future improvements
CityStrategy <: thing(
    price:Price,                // average price
    worstFear:Price,            // worst price = worst price seen * 5-times yearly increase
    shave:Percent)              // level of incomfort, expressed as an average shaving percent

// a city is our second player - it has two levers:
//   (1) instant : react to price and "shaves" its consumption (effacement) - demand/response
//                 -> impact on productivity & comfort
//   (2) yearly  : invest in "negawatts" - energy reduction investments
//                 switch from opertor to supplier
// v0: single tactic
// v1: segment cities according to greenFactor & energy Factor
CityTactic <: thing(
         negaEfficiency:Percent = 1.0)            // factor applied to current avg price to decide to invest
                                                  // in negawatt

ListCity :: list<property>(negaEfficiency)

// a city is a collection of dwellings - a typical simulation produced 1000 cities
City <: Player(
        opertor:Operator,
        tactic:CityTactic,
        strategy:CityStrategy,                     // shared ! one common strategy
        // status - computed slots
        index:integer,                             //
        holds:Household,                           // v0.4 : simple = one household per city
        nbHouseholds:integer = 0,                  // v0.4 : size of city
        climateFactor:Percent = 1.0,               // propensivity to green energies (sun, wind, etc.)
        yearPattern:Affine,                        // local (and modified copy) of national, assumed to be known to all
        negaRatio:Percent = 0.0,                   // energy saving (as a ratio) from Negawatt investments
        negaSum:Price,                             // total money commited (fixed Cost)
        shavingPattern:Affine,                     // consommation reduction
        // income:Price = 0.0,                        // savings from negawatt investments
        expense:Price = 0.0,                       // annualized investments
        energy:Energy = 0.0,                       // actual electricity consumption
        negaEnergy:Energy = 0.0,                   // sum of savings because of negawatt investment
        shavedEnergy:Energy = 0.0,                 // unmet demand (shaved because of price)
        power:Power,                               // Power demand
        opower:Power,                              // v0.2: part of city with local operator
        spower:Power,                              // v0.2: part that stays with supplier (according to MShare)
        yearPatternDay:float,                      // v0.4 cash the get(Affine)
        // stats
        dayPower:Power = 0.0,                     // daily average for demand (MW)
        shaveYM:Measure,                           // sum of all shaving factor = measure of in-comfort
        powerYM:Measure,
        shaveGM:Measure,                           // KPI : compfort
        fearGM:Measure,
        negaGM:Measure,
        priceGM:Measure,                           // price that is paid for actual MW
        billGM:Measure,                            // "bill price" = total cost / total demand
        energyGM:Measure,
        shaveEM:Measure,                           // KPI : compfort
        fearEM:Measure,
        negaEM:Measure,
        priceEM:Measure,                           // price that is paid for actual MW
        billEM:Measure,                           // "bill price" = total cost / total demand
        negaSumEM:Measure,
        negaRatioEM:Measure)

CY[i:(1 .. NOP)] : City := unknown
self_print(c:City) : void -> printf("CY[~A]",c.index)    

// ********************************************************************
// *    Part 3: Local SmartGrid Operators                             *
// ********************************************************************

// the opertor is our third player
OperatorStrategy <: thing(
    balance:Price,              // EBITDA goal :)
    marketShare:Percent,        // market share goal
    income:Price,               // total sales goal
    basePower:Power)            // fixed price until base (pivot for demand-to-price affine curve)

// it holds many levers
//  (a) instant:  store or use stored energy, run local gaz turbine or buy from supplier
//                (based on dynamic prices)
//  (b) yearly:   invest in Oil&Gaz, in Green, in storage
// the storage zone is divided into buffer (store & retrieve local green) and "reserve" (play the market)
// v0 note: use a single tactic that is shared
OperatorTactic <: object(
    firstPrice:Price,           // base price (constant)
    secondPrice:Price,          // variable price that is applied to demand
    storeIn%:Percent,           // fraction of avg price that causes a inflow: get energy to fill storage
    storeUse%:Percent,          // fraction of avg price that causes to use the stored energy to fill demand
    storeOut%:Percent,          // fraction of avg buy price that causes an outflow: sell energy to supplier
    buffer%:Percent,            // fraction of the store that is used as a buffer, the rest is a reserve
    usageRatio:Percent,         // decides to invest when usageRatio is high,
    rateTarget:Percent,         // and when rate(production cost / whole sale) is low
    storageRatio:Percent,       // decides to invest in storage when usage ratio is high
    greenRatio:Percent)         // factor in favor of green

ListOperator :: list<property>(firstPrice,secondPrice,storeIn%,storeUse%,storeOut%,buffer%,usageRatio,
                               rateTarget,storageRatio,greenRatio)

// change for CLAIRE4 : OperatorStat is a collection of stats for operators, to avoid monstruous objects
// with too many slots.
OperatorStat <: object(
        localIn:Energy,
        localOut:Energy,
        supplyIn:Energy,
        supplyOut:Energy,
        dayPrice:float = 0.0,         // daily average for price
        dayFPower:float = 0.0,        // Fossile average production
        dayGPower:float = 0.0,        // Green average production
        forecastYM:Measure,     // Yearly Measure: precision of forecast
        powerYM:Measure,        // Yearly Measure: power
        priceYM:Measure,        // Yearly Measure: price (sell price)
        // gradeGM:Measure,        // Global Measure: inherited from Player
        priceGM:Measure,
        balanceGM:Measure,
        incomeGM:Measure,
        fossile%GM:Measure,
        shareGM:Measure,
        gradeEM:Measure,        // Global Measure:
        priceEM:Measure,
        balanceEM:Measure,
        incomeEM:Measure,
        fossile%EM:Measure,
        shareEM:Measure,
        fossileEM:Measure,
        greenEM:Measure,
        storageEM:Measure)

// an opertor is the local smartgrid opertor associated to a city. It manages and operates
// its own local production (Oil&Gaz type) and the distributed green capacity of its territory
// (organised into a smart grid) : PV, thermal solar or wind turbines. It also holds some storage
// capacity
Operator <: Player(
        sellIncome:float,       // debug
        edfExpense:float,       // debug
        cusExpense:float,
        fuelExpense:float,
        fossile:Oil&Gaz,        // cheaper than from supplier (no transportation costs) but need usage
        green:Green,            // no CO2
        storage:Storage,        // divided into "buffer" and "reserve"
        supplier:Supplier,
        strategy:OperatorStrategy,
        tactic:OperatorTactic,
        customerCost:Price,     // distribution (local node -> customer) + customer management
        city:City,              // v0.2: if e.oneOp? single city (experiment for comparison)
        // status - computed slots
        index:integer,          // easy access + self_print
        usageTime:Time,         // store the usage ratio for the Oil&Gaz turbine (key for investing)
        energy:Energy,          // total energy produced by the opertor
        greenEnergy:Energy,     // local production
        fossileEnergy:Energy,   // fossile yearly energy
        greenPower:Power,       // local production
        fossilePower:Power,     // energy produced by the Oil&Gaz turbine
        output:Power,           // total power procured to the city
        // 5 energy flows that capture the opertor's production (see getEnergy method in tactic.cl)
        inBuffer:Power,         // energy that is put into the storage (buffer part)
        outBuffer:Power,        // energy that is consumed locally from the buffer
        inReserve:Power,        // energy that is stored in the reserve part
        sellReserve:Power,       // energy that is sold from the reserve to the supplier
        outReserve:Power,       // energy that is consumed locally from the reserve
        buy:Power,              // what is bought from the supplier
        sell:Power,             // what is sold to the supplier
        // financial info
        price:Price,            // energy price that is charged to the city
        peakPrice:Price,        // max price in the Year (naive = v0.0)
        prevPrice:Price = 100.0,        // price from previous year
        supplyCost:Price,       // money spent to the supplier
        localCost:Price,        // current production cost
        marketShare:Percent,    // % of city that uses the local opertor
        income:Price = 0.0,             // savings from negawatt investments
        expense:Price = 0.0,            // annualized investments
        prevIncome:Price = 0.0,         // previous value (end of previousDay)
        prevExpense:Price = 0.0,
        satisfaction:Percent,           // satisfaction grade (according to strategy)
        wholesale:Energy = 0.0,         // total amount of energy bought from supplier
        // stats: (1) daily stats  (2) Measures  (3) Experiment Measures
        stat:OperatorStat)


// there are 1000 opertors, stored in an array OP
OP[i:(1 .. NOP)] : Operator := unknown
self_print(o:Operator) : void -> printf("OP[~A]",o.index)

   
// ********************************************************************
// *    Part 4: Global Supply side: Energy Production                 *
// ********************************************************************

// The supplier is our fourth player
// Note: the margin objective makes more sense with a Monte-Carlo simulation (test robustness)
// we add four "system meta parameters" which define how the supplier operates:
//    when to apply variable pricing (set up by regulator)
//    oilMargin : ask EDF ...
//    customerPrice: regulated through the sale price
//    shavingFactor: less feedback info + less sense of community -> shaving is reduced (v0.2)
SupplierStrategy <: object(
        balance:Price,          // expected revenue (can't do more or less :) - regulated)
        margin:Percent,         // difference between capacity and peak power (to be maintained)
        share:Percent,          // defend its customer share
        basePower:Power = 40000.0,        // max power up to which base price is applied
        oilMargin:Percent = 2.0,          // buy external energy at X time oil price (when scarce) - less than CARNOT :)
        customerPrice:Price = 60.0,       // markup to wholesale price, compensate distrib & CRM costs
        shavingFactor:Percent = 0.5)      // shaving works less with global operator

//  a supplier tactic defines:
//     - the price variation as a function of production price and planed demand
SupplierTactic <: object(
        basePrice:Price = 100.0,          // default price  �/MWh
        variablePrice:float = 100.0,      // margin over production cost, when demand > basePower
        usageRatio:Percent = 0.50)        // keep fossile usageRatio under X% (capacity to shave peaks)

ListSupplier :: list<property>(basePrice,variablePrice,usageRatio)

// same thing: create a stat object for suppliers
SupplierStat <: object(
        dayPrice:float = 0.0,
        dayFPower:float = 0.0,
        dayNPower:float = 0.0,
        powerYM:Measure,                // Yearly Measure : power
        priceYM:Measure,                // Yearly Measure : wholesale price
        customerPriceYM:Measure,        // Yearly Measure : customer price
        balanceGM:Measure,
        marginGM:Measure,
        fossile%GM:Measure,
        priceGM:Measure,
        shareGM:Measure,
        balanceEM:Measure,
        marginEM:Measure,
        fossile%EM:Measure,
        priceEM:Measure,
        shareEM:Measure,
        fossileEM:Measure,
        nuclearEM:Measure)


// a supplier produces the energy for a global territory (country or region) using large-scale means.
Supplier <: Player(
        nuclear:Nuclear,
        fossile:Oil&Gaz,
        opertors:list<Operator>,
        marginalCost:Price = 31.0,              // setup & controlled by regulator assumed to be constant (Nuke)
        transportCost:Price,                    // cost per MW.h from production to local node
        customerCost:Price,                     // distribution (local node -> customer) + customer management
        strategy:SupplierStrategy,
        tactic:SupplierTactic,
        // computed slots
        prevIncome:Price = 0.0,                        // previous value (end of previousDay)
        prevExpense:Price = 0.0,                       // same
        firstCapacity:Power,
        nukeCapacity:Power,                       // set up daily - cannot change too fast
        nukeCost:Price,
        fossileCost:Price,
        price:Price,                    // (sell) dynamic wholesale price for 1MWh
        buy:Price,                      // buy price
        customerPrice:Price,            // sell price for 1MWh, for end customers
        peakPrice:Price,                // max customer price in the Year (naive = v0.0)
        prevPrice:Price = 100.0,        // price from previous year
        power:Power,                    // current energy production (MW)
        peakPower:Energy,               // yearly Peak Power
        energy:Energy,                  // total energy produced by the supplier
        wholesale:Energy,               // total energy sold to operators
        wBuy:Price = 0.0,
        wPrice:Price = 0.0,             // weighted average
        crisis:Energy,                  // quantity of energy bought on the open market (crisis mode)
        nukeEnergy:Energy,              // nuclear yearly production
        fossileEnergy:Energy,           // fossile yearly production
        // stats: (1) daily stats  (2) Measures  (3) Experiment measures
        stat:SupplierStat)

self_print(s:Supplier) : void -> princ("EDF")

// ********************************************************************
// *    Part 5: Experiments                                           *
// ********************************************************************

// Scenario describe a view of the world (parameters for initialization)
Scenario <: thing(
  tag:string,                   // a small label
  timeUnit:float = 3.0,         // default (should be 3)  - v0.4 => float !
  climateFactor:Percent,        // climate-induced demand variation from one year to the other
  cityVariation:Percent,        // +/- variation of one city yearly pattern,
  energyVariation:Percent,      // +/- variation of daily usage (max value for a city)
  energyTrend:Percent,          // overall trend in energy consumption (to play with)
  co2FirstPrice:Price,          // Co2 Tax ex: 20�/T
  // supplier's hypothesis
  nuclearTrend:Percent,         // nuclear assets growth or reduction trend
  customerCost:Price,           // distribution (local node -> customer) + customer management
  localStorage:Energy,          // typical storage (capacity) for opertor
  maxBasePrice:Price,           // first step of local base price may be kept low (by France)
  maxVariablePrice:Price,       // first step of local variable price may be kept low (by France)
 // storage & green parameters
  storageCost:Price,            // typical cost of storage (TCO for 1MWh -> cf init(s) in simul)
  storageTrend:Percent,         // price is declining ...
  greenCost:Price,              // price of 1MWh of green energy
  greenPower:Power,             // typical green power for a local
  greenVariation:Percent,       // +/- 20% (e.g.) -efficiency variation from one city to another
  intermittent:Percent,          // green works from 0h to i h (% of the day)
  powerIncrement:Power,         // size of additional local unit (green or oil)
  // oil parameters
  oilPower:Power,               // typical oil&gaz power for a local
  oilPrice:Price,               // current price (re-evaluated continously with 90 day average)
  oilTrend:Percent,                      // current price (re-evaluated continously with 90 day average)
  oilAmplitude:Percent = 1.0,    // price varies between 50% and 200% with cycles
  oilPeriod:float,              // theta = 2pi / pediode
  oilNoise:Percent,             // daily noise
  // negaWatt efficiency ,
  negaCapacity:Percent,         // the maximum energy saving possible (expressed in %)
  negaEfficiency:Price,         // prix pour �conomiser 1W sur 10W (10%)
  // S-curve for operator marketshare according to price difference
  marketShare:Percent,          // default market share (start, iso-price)
  marketSensitivity:float,       // sensitivity
  marketLimit:Percent)           // max market share


// experiment contains the strategy and the GTES parameters (randomization, etc.)
// TODO: add the ranges for the systemic parameters that we want to randomize
//     - sensitivity/market limit for marketShare S-curve
// ATTENTION: oneOp does NOT WORK -> need to gather all demands from all cities
//            need to look for o.city (which is only 1 city vs all) in tactic.cl
Experiment <: thing(
  scenario:Scenario,
  nSample:integer = 10,                 // number of randomizations
  nashLoop:integer = 6,                 // number of nash loops (see simul.cl)
  nCities:integer = 1,                  // study the influence of N
  oneOp?:boolean = false,               // true force the creation of a single operator (DO NOT USE)
  regulator:Regulator,
  supplier:Supplier,
  oTactic:OperatorTactic,               // init value for tactics - operator
  cTactic:CityTactic,                   // city
  sTactic:SupplierTactic,               // supplier
  rTactic:RegulatorTactic,              // regulator
  oStrategy:OperatorStrategy,
  cStrategy:CityStrategy,
  sStrategy:SupplierStrategy,
  rStrategy:RegulatorStrategy,
  // randomization windows
  oilMin:Price,  oilMax:Price,          // range for oil price
  cityMin:Percent, cityMax:Percent,     // range for city Variation (one city vs another)
  demandMin:Percent, demandMax:Percent, // demand variation (size of noise)
  negaMin:Percent, negaMax:Percent,     // alpha: negaSensitivity
  shaveMin:Percent, shaveMax:Percent,   // beta: residential shavingSensitivity
  marketMin:Percent, marketMax:Percent) // gamma: market Sensitivity

// our problem solver object
// contains the global constant that do not change (the others are in Scenario)
Problem <: thing(
  nuclearFixedCost:Percent = 0.5,       // 50 % of a nuclear plant is fixed costs
  fossileFixedCost:Percent = 0.3,       // 10% of the cost is fixed (the rest is variable)
  localFixedCost:Percent = 0.5,         // small local plant cost less in investment v0.5
  co2Factor:float = 0.87,               // amount of CO2 in MWh -0,30t/MWh for Oil x CARNOT
  timeUnit:float = 3.0,                 // default (should be 3)
  // experiments parameters
  hypothesis:string = "default",  //
  supplier:Supplier,
  scenario:Scenario,
  regulator:Regulator,
  // computed slots
  houseConso:Power,                     // average house power consumption
  yearPattern:Affine,                   // global (average of all cities)
  yearPatternDay:float,                 // v0.4 : cash into a list
  time:Time = 0,
  oilPrice:Price,                       // current price (re-evaluated continously with 90 day average)
  oilPriceYM:Measure,                   // Yearly measure
  referencePrice:Price = 100.0,         // referencePrice for energy (supplier's price @ t0)
  nashDist:float,                       // v0.3 : measure the convergence rate of last steps
  nashDistGM:Measure)  

pb :: Problem()


// <start code fragment - Measure - CGS v0.0 >
// what we measure for one run
Measure <: ephemeral_object(
  sum:float = 0.0,
  square:float = 0.0,           // used for standard deviation
  num:float = 0.0)          // number of experiments

// simple methods add, mean, stdev
[add(x:Measure, f:float) : Measure 
     -> x.num :+ 1.0, x.sum :+ f, x.square :+ f * f, x ]
[mean(x:Measure) : float 
     -> if (x.num = 0.0) 0.0 else x.sum / x.num]
[stdev(x:Measure) : float
   -> let y := ((x.square / x.num) - ((x.sum / x.num) ^ 2.0)) in
         (if (y > 0.0) sqrt(y) else 0.0) ]
[stdev%(x:Measure) : Percent 
   -> stdev(x) / mean(x) ]
[reset(x:Measure) : void 
   -> x.square := 0.0, x.num := 0.0, x.sum := 0.0 ]

// <end code fragment> ------------------------------------------------------------------------------------------


// <start code fragment - Hour Time Management - SGSS - v0.0 >

// this code fragment is used to represent time in hours, over long periods (years)

year!(i:integer) : Time 
   -> (i * 24 * 365)
day!(i:integer) : Time 
  -> (i * 24)

// time is writen 3123014 = Y * 100000 + D * 1000 + H
[date(i:Time) : string 
   -> string!(year_(i)) /+ ":" /+ string!(day_(i)) /+ ":" /+ string!(hour_(i))]

// accessors
[year_(i:Time) : integer 
    -> i / (365 * 24)]
[day_(i:Time) : integer 
    -> (i mod (365 * 24)) / 24]
[hour_(i:Time) : integer 
    -> (i mod 24)]

// pseudo-equality (useful for floats)
== :: operation(precedence = precedence(=))
==(x:float,y:float) : boolean
  -> (abs(x - y) < (abs(x) + abs(y) + 1.0) * 1e-2)

pos(x:float) : float 
  -> (if (x > 0.0) x else 0.0)

// <end fragment> --------------------------------------------------------------------------------


// <start code fragment - S-curve - SOCC  - v0.0 >

// this is a useful re-usable component that is inspired from the Scurve module of
// SOCC. We borrow the concept of a 5-points scurve (two halves of an hyperbol), which
// we use in a normalized framework for a S-curve that represents a deviation from a standard value
// (a) the input (delta) is a float such that min = -inf, ref value = 0.0 and max = +inf
// (b) the output is a percentage between [0, max] and ref output is %val
// sCurve is defined by 3 parameters: val,max,derivative
[sCurve(%val:float,%max:float,%d:float,x:float) : float
  -> let  alpha := %val, beta := %d / alpha,
          delta := %max - %val, gamma := %d  / delta in
        (if (x >= 0.0)  // hyberbola
            (if (%max = %val) %max
             else %max - (delta / (1.0 + x * gamma)))
         else // other half of hyperbola
             (if (%val = 0.0) 0.0
              else alpha / (1.0 -  x * beta))) ]

[testScurve()
  -> // val = 0.3 max = 1.0 der = 1.0
     assert(sCurve(0.3,1.0,1.0,-1000.0) == 0.0),
     assert(sCurve(0.3,1.0,1.0,-0.01) == 0.29),
     assert(sCurve(0.3,1.0,1.0,0.0) == 0.3),
     assert(sCurve(0.3,1.0,1.0,0.01) == 0.31),
     assert(sCurve(0.3,1.0,1.0,100.0) == 1.0),
     // val = 0.5 max = 0.6 der = 2.0
     assert(sCurve(0.5,0.6,2.0,-1000.0) == 0.0),
     assert(sCurve(0.5,0.6,2.0,-0.01) == 0.48),
     assert(sCurve(0.5,0.6,2.0,0.0) == 0.5),
     assert(sCurve(0.5,0.6,2.0,0.01) == 0.52),
     assert(sCurve(0.5,0.6,2.0,100.0) == 0.6) ]

// <end fragment > ------------------------------------------------------------------------------

// <start code fragment - Affine curves - GWDG - v0.0>

// we need to manipulate simple curves - in version 0.1 we pick piece-wise linear
// functions, defined by a list of pairs (x,f(x))
Affine <: object(
  xValues:list<float>,
  yValues:list<float>,
  minValue:float = 0.0,
  maxValue:float = 0.0,
  n:integer = 0)

// assumes l is a list of pairs (x-i,y-i) and x-i is a strictly increasing sequence
[affine(l:listargs) : Affine
  -> let m1 := 1e9, M1 := -1e9,
         l1 := list<float>{float!(x[1]) | x in l},
         l2 := list<float>{float!(x[2]) | x in l} in
       (for i in (2 .. length(l)) (if (l1[i - 1] >= l1[i]) error("affine params decrease: ~S",l1)),
        for v in l2 (m1 :min v, M1 :max v),
        Affine(n = length(l), minValue = m1, maxValue = M1, xValues = l1, yValues = l2)) ]

[self_print(x:Affine) : void
  -> printf("affine(~I)",
        for i in (1 .. x.n)
          (if (i != 1) princ(" "),
           pF(x.xValues[i],2),princ(":"), pF(x.yValues[i],2))) ]


// simpler function to be used by a program
[make_affine(l1:list<float>,l2:list<float>) : Affine
 -> let m1 := 1e9, M1 := -1e9 in
       (for i in (2 .. length(l1)) (if (l1[i - 1] >= l1[i]) error("affine params decrease: ~S",l1)),
        if (length(l1) != length(l2)) error("affine params ~S & ~S should match in length",l1,l2),
        for v in l2 (m1 :min v, M1 :max v),
        Affine(n = length(l1), minValue = m1, maxValue = M1, xValues = l1, yValues = l2)) ]


// returns the value of the affine function for a given point between m and M
[get(a:Affine,x:float) : float
  -> let i := 0 in
       (for j in (1 .. a.n)
         (if (a.xValues[j] > x) break(i := j)),
        if (i = 0) a.yValues[a.n]       // x is bigger than all x Values
        else if (i = 1) a.yValues[1]    // x is smaller than all x value
        else let x1 := a.xValues[i - 1], x2 := a.xValues[i],
                 y1 := a.yValues[i - 1], y2 := a.yValues[i] in
               (y1 + (y2 - y1) * (x - x1) / (x2 - x1))) ]

[get(a:Affine,x:integer) : float -> get(a,float!(x)) ]

// this is a cool trick : prints an affine curve on a character terminal as nicely
// as we can
//
NX :: 70
NY :: 30
[display(a:Affine) : void
 -> let m1:float := a.xValues[1], M1 := a.xValues[a.n],
        m2:float := a.minValue, M2 := a.maxValue,
        lv := list<integer>{scale(get(a,m1 + (M1 - m1) * (float!(i) / float!(NX))),m2,M2,NY) | i in (0 .. NX)},
        l* := list<boolean>{false | i in (0 .. NX)},     // see which y-values are touched
        c := 0 in
      (princ("\n"),
       for i in (1 .. a.n)
         let y := scale(a.xValues[i],m1,M1,NX) in l*[y + 1] := true,
       for u in (0 .. NY)
         let y := NY - u,
             i := matchY(a,m2,M2,y) in
            (if (i != 0) fP(a.yValues[i],5) else princ("     "),
             if (u = 0) princ(" ^") else princ(" |"),
             for v in (0 .. NX)
               (if (lv[v + 1] = y)
                   (if l*[v + 1] princ("+") else princ("o"))
                else princ(" ")),
             princ("\n")),
       printf("      +~I>\n       ",(for i in (1 .. NX) princ("-"))),
       for i in (0 .. NX) (if l*[i + 1] princ("^") else princ(" ")),
       princ("\n       "),
       for i in (0 .. NX)
         let j := matchX(a,m1,M1,i) in         // assumption : we have printed already c chars
            (if (j != 0 & c <= 0)
                (fP(a.xValues[j],5), princ(" "), c := 5)            // forbid printing for 6 chars
             else (if (c > 0) c :- 1 else princ(" "))),
       princ("\n")) ]


// retreive a value (x-i, y-i) such that the scale value of y-i is u and returns i
[matchY(a:Affine,m:float,M:float,v:integer) : integer
 -> let u := 0 in
      (for i in (1 .. a.n)
        (if (scale(a.yValues[i],m,M,NY) = v)
             break(u := i)),
       u) ]

// retreive a value whose scale value is u
[matchX(a:Affine,m:float,M:float,v:integer) : integer
 -> let u := 0 in
      (for i in (1 .. a.n)
        (if (scale(a.xValues[i],m,M,NX) = v)
             break(u := i)),
       u) ]
       
// scale: returns the integer coordinates associated to a float between m and M
[scale(v:float,m:float,M:float,N:integer) : integer
  -> if (m = M) 0 else integer!((v - m) / (M - m) * N) ]

// useful addition: average (could be extended into integration)*
[average(a:Affine) : float
  -> let sx := 0.0, sy := 0.0, n := length(a.xValues) in
       (for i in (1 .. (n - 1))
          let x1 := a.xValues[i], x2 := a.xValues[i + 1],
              y1 := a.yValues[i], y2 := a.yValues[i + 1] in
            (sx :+ (x2 - x1), sy :+ (y1 + y2) * (x2 - x1)),
        (sy / (2.0 * sx))) ]


// <end fragment> ----------------------------------------------------------


// additional method : randomization
[randomize(a:Affine,%min:Percent,%max:Percent) : Affine
  -> let l1 := list<float>{x | x in a.xValues},
         l2 := list<float>{ (y * randomIn(%min,%max)) | y in a.yValues},
         m1 := 1e9, M1 := -1e9 in
       (for v in l2 (m1 :min v, M1 :max v),
        Affine(n = length(l1), minValue = m1, maxValue = M1,
               xValues = l1, yValues = l2)) ]

// the pF is my ugly duckling :) -------------------------------------------
// float print is now standard in v3.4.42 but this is still a cuter print ...
[pF(x:float,i:integer) : void        // prinf i numbers
  -> if (x < 0.0) (princ("-"), pF(-(x),i))
     else let frac := x - float!(integer!(x + 1e-10)) + 1e-10 in
         printf("~A.~I", integer!(x + 1e-10),
                pF(integer!(frac * (10.0 ^ float!(i))),i)) ]

// print the first i digits of an integer
[pF(x:integer,i:integer) : void
  -> if (i > 0) let f := 10 ^ (i - 1), d := x / f in
                   (princ(d), if (i > 1) pF(x mod f, i - 1)) ]

[p%(x:float) -> pF(x * 100.0,2), princ("%")]
[pF(x:float) -> pF(x,1)]


// fixed number of digits
[fP(x:float,i:integer) : void
  -> if (x >= 10.0) let n := integer!(log(x) / log(10.0)) in pF(x,i - (n + 2))
     else pF(x,i - 2) ]

// utilities ------------------------------------------------------------------

sum(l:list[float]) : float
 => (let x := 0.0 in (for y in l x :+ y, x))

avg(l:list[float]) : float
 => (let x := 0.0, n := 0.0 in (for y in l (x :+ y, n :+ 1.0), (x / n)))

// makes float! a coercion
[float!(x:float) : float 
  -> x]

// already in CLAIRE4
// [abs(x:float) : float -> (if (x >= 0.0) x else -(x)) ]
// [sqr(x:integer) : integer -> x * x]

[randomIn(a:float,b:float) : float
  -> a + (b - a) * (float!(random(1000000001)) / 1e9) ]

[randomize(x:float,a:float,b:float) : float
   => randomIn(x * a, x * b) ]

[randomIn(l:list) : any 
  -> let n := length(l) in l[1 + random(n)]]

// v0.3 : useful for tabu
[randomBut(l1:list,l2:list) : any
  -> let n := length(l1), i := 1 + random(n), c := 0 in
       (while (l1[i] % l2) (if (i = n) (if (c = 1) error("randomBut(~S,~S)",l1,l2)
                                        else (c := 1, i := 1))
                             else i :+ 1),
        l1[i]) ]

[randomChoice?(x:Percent) : boolean
  -> random(1000) < integer!(x * 1000.0) ]

// sin did not exist in earlier CLAIRE versions - use a dumb periodic function called sinus-
[sinus(x:float) : float
 -> let y := x / 6.28, f := y - float!(integer!(y)), z := f * 6.28 in
      (if (z > 3.14) -(sinus(z - 3.14))
       else if (z > (3.14 / 2.0)) sinus(3.14 - z)
       else (z / (3.14 / 2))) ]

// new and useful: random spit of n into m parts
[randomSplit(n:integer,m:integer) : list<integer>
  -> let l := list<integer>{random(100000) | i in (1 .. m)},
         d := 0 in
       (for i in (1 .. m) d :+ l[i],
        for i in (1 .. m) l[i] := integer!(float!(l[i]) * float!(n) / float!(d)),       // float conversion to avoir overflows
        d := 0,
        for i in (1 .. m) d :+ l[i],                    // compute rounding error
        if (n > d) for i in (1 .. (n - d)) l[1 + random(m)] :+ 1,        // fix the errors randomly
        l) ]

