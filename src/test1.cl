// ********************************************************************
// *       SGSS: Smart Grid Systemic Simulation                       *
// *       copyright (C) 2011 Yves Caseau                             *
// *       file: test1.cl                                             *
// ********************************************************************

// this file contains a simple description of our problem scenario
Percent :: float
Price :: float
NIT:integer :: 15               // number of years

// France contains the regulator parameters, as well as the macro-economic parameters
//
France :: Regulator(
  energy = 0.0,        // 400 TWh : 100 + 300
  power = 80000.0,          // highest peak (when all parameters are 1.0)
  nbHouseholds = 20000000,  // 20 millions
  // the yearly pattern (from day 0 to day 365)
  yearPattern = affine(list(0,1.0),list(30,0.8),list(5 * 30,0.6),list(8 * 30,0.8),list(10 * 30,0.7),list(365,1.0)))

// the supplier
// todo in v2 : add Green energy
// GROSS SIMPLIFICATION : without hydro, without Industry
EDF :: Supplier(
  nuclear = Nuclear(cost = 25.0, power = 50000.0, trend = 0.03,  // 50GW, 300 TW.h, 40�/MWh, 3% expected price increase
                    min% = 0.5, dailyMax% = 0.01),
  fossile = Oil&Gaz(power = 20000.0, capacity = 100000.0),                            // 20GW extra capacity
//   storage = Storage(cost = 200.0, capacity = 1000.0),                                 // 1 TWh de stockage, 200�/MWh
  customerCost = 30.0,                              // local distribution + customer management
  transportCost = 20.0)                             // 20�/MWh transport� (partie ind�pendante du local)

(MaxValue[basePrice] := 100.0, MaxValue[variablePrice] := 300.0)

// we start with two Household types
Residentiel :: Household(
    // daily pattern
    dayPattern =  affine(list(0,0.6),list(3,0.3),list(9,0.9),list(17,0.7),list(20,1.0),list(24,0.6)),
    // "effacement S-curve"
    shavingFactor = 0.2,                 // current effort level is 20% (would go up 20% if free)
    shavingSensitivity = 0.1,            // tuned with beta
    shavingCapacity = 0.6)               // could reduce up to 60%


// one cityTactic
CTACTIC :: CityTactic()

// one opertorTactic - hand tuned (see later)
OTACTIC :: OperatorTactic(
    firstPrice = 90.0,              // margin on local production
    secondPrice = 60.0,         // margin on supplier's electricity
    storeIn% = 0.9,             // fraction of average price that causes a inflow: get energy to fill storage
    storeUse% = 0.9,            // fraction of average price that causes to use the stored energy to fill demand
    storeOut% = 0.9,            // fraction of average buy price that causes an out flow: sell energy to supplier
    buffer% = 0.2,              // fraction of the store that is used as a buffer, the rest is a reserve (proctect against price fluctuations)
    usageRatio = 0.3 ,           // decides to invest when usageRatio is high,
    rateTarget = 1.0,           // and when production cost is low
    storageRatio = 1.0,
    greenRatio = 1.0)           // favor Green - 2.3 = DEBUG

// warning: default value is 2.0 = 200%
(MaxValue[firstPrice] := 200.0, MaxValue[secondPrice] := 400.0,
 MaxValue[rateTarget] := 5.0, MaxValue[storageRatio] := 5.0)

// Supplier tatic
STACTIC :: SupplierTactic(basePrice = 50.0,         // default price  55 �/MWh
                          variablePrice = 110.0,     // additional price when power is over the nuke capacity
                          usageRatio = 0.267)


// Regulator tactic
RTACTIC :: RegulatorTactic(co2Trend = 0.1,           // yearly trend
                           greenShare = 0.1,         // share of the green invest that is supported through tax exemption
                           greenToStorage = 0.2)     // 20% of green to storage


// E1-S1 : these are the fixed point values
// EDF       basePrice:81.556        variablePrice:0.015     usageRatio:0.267
// OP[1]     firstPrice:106.045      secondPrice:2.316       storeIn%:0.400  storeUse%:0.900 storeOut%:0.900     buffer%:0.200   usageRatio:0.550        rateTarget:1.000        storageRatio:1.000      greenRatio:1.000
// France        greenShare:0.037        co2Trend:0.105

// E1-S5 : these are the fixed point values
// EDF  basePrice:50.000        variablePrice:174.488   usageRatio:0.292
// OP[1]         firstPrice:88.842       secondPrice:110.444     storeIn%:0.500  storeUse%:1.267 storeOut%:1.837     buffer%:0.250   usageRatio:0.925        rateTarget:1.000        storageRatio:0.000      greenRatio:1.000


// v0.4 : put a max to avoid Nash fluctuations ...

// MaxValues for optimization
(MaxValue[storeIn%] := 2.0, MaxValue[storeUse%] := 2.0, MaxValue[storeOut%] := 5.0,
 MaxValue[greenRatio] := 3.0, MaxValue[storageRatio] := 3.0, MaxValue[greenShare] := 0.2)

// scenarios
S1 :: Scenario( tag = "default scenario",
  // demand parameters
  climateFactor = 0.2,           // climate-induced demand variation from one year to the other
  cityVariation = 0.1,           // variation from one city to another (climate)
  energyVariation = 0.1,         // daily variation (from prediction to real)
  powerIncrement = 100.0,        // granularity of investment
  oilPower = 1000.0,              // typical oil&gaz power for a local
  customerCost = 25.0,            // cheaper than supplier's cost :)
  nuclearTrend = 0.0,
  energyTrend = 0.0,             // stable demande (cf. EDF's report)
  maxBasePrice = 50.0,          // EDF base price is less than 100
  maxVariablePrice = 250.0,      // EDF variable price is less than 200 + cost
  // negaWatt efficiency ,
  negaCapacity  = 0.5,          // max gain is 50%,
  negaEfficiency = 0.5,         // tuned with alpha()
  // S-curve for operator marketshare according to price difference
  marketShare = 0.2,             // default market share (start, iso-price)
  marketSensitivity = 0.1,       // sensitivity, tuned with gamma
  marketLimit = 0.6,             // max market share
  // oil price
  oilPrice = 20.0,              // 100$/baril -> 460� / T -> 37� / MWh (primaire) -> 110�/ MWh
                                // ceci vaut pour le p�trole, mais c'est probablement 2 x moins cher avec le gaz.
  oilTrend = 0.02,              // +50% in 20 years
  oilAmplitude = 0.5,           // price varies between 50% and 200% with cycles
  oilPeriod = (6.3 / 8760.0),   // theta = 2pi / pediode
  oilNoise = 0.1,               // daily noise
  // storage
  localStorage = 10.0,           // typical storage : 10 MW for opertor
  storageCost = 200.0,           // TCO for 1MWh - (look at init/simul)  - 100� = equivalent to 100$/KWh buy price
  // green energy
  co2FirstPrice = 20.0,          // start price of co2 tax
  greenCost = 200.0,             // price of 1MWh of green energy
  greenPower = 50.0,             // typical green power step for a local (50MW to make it visible)
  greenVariation = 0.3,          // +/- 20%
  intermittent = 0.4)            // green works from 0h to i h (% of the day)

// strategies
RSTRATEGY :: RegulatorStrategy(   co2Amount = 40e6,     // MtC, the current value is (0.95 * 100 10^6 (TWh) * 3/12 (Tep->MWh x Carnot)) for 100TWh de non clean
                                  balance = 1e9,
                                  energy = 500000.0)

SSTRATEGY :: SupplierStrategy( basePower = 40000.0,      // max power up to which base price is applied
                              balance = 12e9,           // 12 G� EBITDA
                              margin = -0.01,           // less than 1% over-demand
                              share = 0.82,               // keep fossile usageRatio under X% (capacity to shave peaks)
                              customerPrice = 60.0,      // wholesale to B2C delta
                              oilMargin = 2.0,          // crisis price ratio (probably higher)
                              shavingFactor = 0.5)      // shaving works less with global operator

CSTRATEGY ::  CityStrategy(    price = 100.0,            // wishes a price of 100�/MWh
                               worstFear = 200.0,        // panic starts are 200�/MWh
                               shave = 0.2)             // no more than 20%  (default)

OSTRATEGY :: OperatorStrategy(    basePower = 40000.0,          // max power up to which base price is applied
                                  balance = 1e9,              // EBITDA goal :)
                                  marketShare = 0.40,         // market share goal : 30%
                                  income = 10e9)              // 10 G� to weigh something significant


// stronger (more agressive strategies)
SSTRATEGY2 :: SupplierStrategy( basePower = 40000.0,      // max power up to which base price is applied
                                balance = 14e9,           // 14 G� EBITDA
                                margin = -0.01,           // less than 1% over-demand
                                share = 0.82,               // keep fossile usageRatio under X% (capacity to shave peaks)
                                customerPrice = 60.0,      // wholesale to B2C delta
                                oilMargin = 2.0,          // crisis price ratio (probably higher)
                                shavingFactor = 0.5)      // shaving works less with global operator

OSTRATEGY2 :: OperatorStrategy(    basePower = 4000.0,          // max power up to which base price is applied
                                   balance = 13e8,              // EBITDA goal :)
                                   marketShare = 0.4,         // market share goal : 30%
                                   income = 14e9)              // 15 G� to weigh something significant


// bolder (focus on market share)
SSTRATEGY3 :: SupplierStrategy( basePower = 40000.0,      // max power up to which base price is applied
                                balance = 7e9,             // 8 G� EBITDA
                                margin = -0.01,           // less than 1% over-demand
                                share = 1.0,               // keep fossile usageRatio under X% (capacity to shave peaks)
                                customerPrice = 60.0,      // wholesale to B2C delta
                                oilMargin = 2.0,          // crisis price ratio (probably higher)
                                shavingFactor = 0.5)      // shaving works less with global operator

OSTRATEGY3 :: OperatorStrategy(    basePower = 4000.0,          // max power up to which base price is applied
                                   balance = 8e8,              // EBITDA goal :)
                                   marketShare = 0.43,         // market share goal : 30%
                                   income = 8e9)              // 7 G� to weigh something significant


// Experience
E1 :: Experiment(
  scenario = S1,
  nSample = 10,                     // later : 30 steps :)  -> 10000s (3h !)
  nashLoop = 5,                     // default NashLoop = 5,  tester avec 10 (v�rifier la stabilit�)
  regulator = France,
  supplier = EDF,
  nCities = 10,                    // first step: play with one city !
  cTactic = CTACTIC,  oTactic = OTACTIC, sTactic = STACTIC,  rTactic = RTACTIC,
  cStrategy = CSTRATEGY, oStrategy = OSTRATEGY, sStrategy = SSTRATEGY, rStrategy = RSTRATEGY,
  oilMin = 20.0,  oilMax = 30.0,          // range for fossile price  (gaz) en �/MWh (primaire)
  cityMin = 0.05, cityMax = 0.2,          // range for city Variation (one city vs another)
  demandMin = 0.05, demandMax = 0.2,      // demand variation (size of noise)
  negaMin = 0.3, negaMax = 0.7,                 // alpha: negaSensitivity
  shaveMin = 0.02, shaveMax = 0.2,              // beta: residential shavingSensitivity
  marketMin = 0.2, marketMax = 0.4)             // gamma: market Sensitivity


// each "scenario" is either an experiment or a new scenario
// - E2: variation (test de robustesse)
//      - E3: importance de la variation locale (l operateur s adapte aux variations locales, le supplier aux
//        variations nationales)
//      - E4: oil price
//      - S2: EDF nuclear reduction sc�nario ! Only way to make SG interesting
//            (Nuke is not sensitive to CO2 tax and to oil price)
//      - S3: carbon tax
//      - S4: storage cost
//      - S5: variant where maxBase is lower => better for marginal prices :)
//      - S6: lower Green price

// E1 x S1 -> nash(2) works, this is the reference
// note: best tactic for EDF is flat price

E2 :: copy(E1)
E3 :: copy(E1)
E4 :: copy(E1)
E5 :: copy(E1)

// strategy matrix E1 = E1-SS (OP:soft x EDF:soft)
E1-SH :: copy(E1)
E1-HH :: copy(E1)
E1-HS :: copy(E1)
E1-BB :: copy(E1)
E1-BS :: copy(E1)
E1-SB :: copy(E1)
E1-BH :: copy(E1)
E1-HB :: copy(E1)


// E2 : more variation  -> nash(5)  ... small price war
(put(name,E2,symbol!("E2")), E2.demandMin := 0.3, E2.demandMax := 0.3)

// E3 : more city variation - each city is different from each other
// nash(6) -> equilibrium
(put(name,E3,symbol!("E3")), E3.cityMin := 0.3, E3.cityMax := 0.3)

// E4 : oil price
// nash(6) -> perfect equilibrium
(put(name,E4,symbol!("E4")), E4.oilMin := 30.0, E4.oilMax := 50.0)

// simpler experiment to get results faster (but less precise, not to be used for published work)
G1 :: copy(E1)
G2 :: copy(E2)
G3 :: copy(E3)
(put(name,G1,symbol!("G1")), G1.nSample := 1, G1.nashLoop := 5,
 put(name,G2,symbol!("G2")), G2.nSample := 1, G2.nashLoop := 5,
 put(name,G3,symbol!("G3")), G3.nSample := 1, G3.nashLoop := 5)

// last : play with 3 and 20 cities
H1 :: copy(E3)
H2 :: copy(E3)
(put(name,H1,symbol!("H1")), H1.nCities := 3,
 put(name,H2,symbol!("H2")), H2.nCities := 20)

// ------------------ scenario variants ------------------------------------------
S2 :: copy(S1)
S3 :: copy(S1)
S3a :: copy(S1)
S4 :: copy(S1)
S5 :: copy(S1)
S6 :: copy(S1)

// EDF de-nuke scenario
(put(name,S2,symbol!("S2")), S2.nuclearTrend := -0.07, S2.tag := "reduction of Nuke")

// carbon tax scenario
(put(name,S3,symbol!("S3")), S3.co2FirstPrice := 200.0, S3.tag := "co2Tax at 200$/t")
(put(name,S3a,symbol!("S3a")), S3a.co2FirstPrice := 200.0, S3a.greenCost := 150.0,
 S3a.tag := "co2Tax at 200$/t + green ")

// storage cost  (requires a fine time unit)
(put(name,S4,symbol!("S4")), S4.storageCost := 30.0, S4.co2FirstPrice := 100.0,
 S4.greenCost := 140.0,
 S4.tag := "storage cost at 30$/MW + co2tax")

// scenario where EDF base price is capped -> S5 / to be defined
(put(name,S5,symbol!("S5")), S5.maxBasePrice := 100.0, S5.tag := "maxBasePrice = 100.0")

// lower price for green (requires tuning)
(put(name,S6,symbol!("S6")), S6.greenCost := 100.0, S6.tag := "greenCost = 100")

// strategy matrix Edf x Operator
(put(name,E1-SH,symbol!("E1-SH")), E1-SH.oStrategy := OSTRATEGY2,
 put(name,E1-HH,symbol!("E1-HH")), E1-HH.oStrategy := OSTRATEGY2, E1-HH.sStrategy := SSTRATEGY2,
 put(name,E1-HS,symbol!("E1-HS")), E1-HS.sStrategy := SSTRATEGY2)

(put(name,E1-SB,symbol!("E1-SB")), E1-SB.oStrategy := OSTRATEGY3,
 put(name,E1-BB,symbol!("E1-BB")), E1-BB.oStrategy := OSTRATEGY3, E1-BB.sStrategy := SSTRATEGY3,
 put(name,E1-BS,symbol!("E1-BS")), E1-BS.sStrategy := SSTRATEGY3)

( put(name,E1-BH,symbol!("E1-BH")), E1-BH.oStrategy := OSTRATEGY2, E1-BH.sStrategy := SSTRATEGY3,
  put(name,E1-HB,symbol!("E1-BH")), E1-HB.oStrategy := OSTRATEGY3 , E1-HB.sStrategy := SSTRATEGY2)

// ------------------------- debug & memory refresh : run OneDay & OneYear -------------------------------

// runs one Day, SHOW level, with n cities
[day(n:integer)
  -> E1.nCities := n,
     verbose() := 2,
     init(E1),
     MONTHTALK := 1,
     ISTOP := day!(1) + 1,
     run(pb,true)]

[month(n:integer)
  -> E1.nCities := n,
     verbose() := 1,
     MONTHTALK := 1,            // print day by day summary
     init(E1),
     ISTOP := day!(31),
     run(pb,true)]


// run one Year, TALK level, with n cities
[year(n:integer)
  -> E1.nCities := n,
     verbose() := 1,
     init(E1),
     ISTOP := year!(1) + 1,
     YINVEST := 5,              // should be 5, for 5 first years of invest
     INVEST := 1,               // trace investments
     GSHOW := 1,                // should be 2 (except for grading debug)
     run(pb,true)]

// ------------------------- regular experiments : could be stored on file -------------------------------

EXP:Experiment :: E1

// Verbosity level to play with S3G
//  run : verbose = 0, only stats
//  go : verbosity = 0, show the years
//  go + verbose = 1 : show every 10 days
//  SHOW : show all hours, controlled by DWINDOW
//
//  loop: quiet
[go() -> go(true) ]
[run() -> go(false) ]
[go(b:boolean) : void
  -> if (size(Operator) = 0)
       (// EXP.nCities := 10, // 10,
        init(EXP),
        verbose() := 0,
        OPTI := 1, OPTI2 := 1,
        //ISHOW := 1,                      // debug : start SHOW at some index value
        // NIT := 15,                         // 15 years (5 invest and 10 to see the benefits)
        run(pb,b))
      else (runLoop(b),seeEnd(pb)) ]

// used for scenario testing
[go(e:Experiment,s:Scenario) : void
  -> e.scenario := s,
     init(e),
     verbose() := 0,
     run(pb,false) ]

// same for one year
[go1(e:Experiment,s:Scenario) : void
  -> e.scenario := s,
     init(e),
     verbose() := 0,
     ISTOP := year!(1) - 1,
     run(pb,false) ]


// equivalent to run.cl
[run(e:Experiment,s:Scenario) : void
  -> e.scenario := s,
     e.nSample := 1,
     e.nashLoop := 3,
     OPTD := 1,
     run(e) ]

// ====================== PART I : tuning operators =============================================================
// simple what-if for operators
// works on OP[1], i is the property index (1
[go(i:integer,x:Percent)
   -> go(false),
      let o := OP[1], p := o.tacticProperties[i] in
         (if (NIT = 1) seeYear(o) else seeEnd(o),
          ss(o),
          //[0] ================ whatif ~S(~S) =================== // p,o,
          whatif(o,p,x),
          ss(o),
          if (NIT = 1) seeYear(o) else seeEnd(o)) ]

// step1 : optimize(OP[1]), step by step (and check that it makes sense)
[go(i:integer)
   -> go(false),
      let o := OP[1], p := o.tacticProperties[i] in
         (if (NIT = 1) seeYear(o) else seeEnd(o),
          OPTI := 0,
          //[0] ================ optimize ~S(~S) =================== // p,o,
          optimize(o,p),optimize2(o,p),
          runLoop(o),
          if (NIT = 1) seeYear(o) else seeEnd(o)) ]

[check()  -> runLoop(OP[1]), seeYear(OP[1]), tac(OP[1]) ]

// debug
[go2(i:integer,x:Percent)
   -> go(false),
      let o := OP[2], p := o.tacticProperties[i] in
         (if (NIT = 1) seeYear(o) else seeEnd(o),
          ss(o),
          //[0] ================ whatif ~S(~S) =================== // p,o,
          whatif(o,p,x),
          ss(o),
          if (NIT = 1) seeYear(o) else seeEnd(o)) ]


// v0.4 : price optimization = what-if for OP[i] on x:firstPrice/y:secondPrice
[wpr(i:integer,x:float,y:float) : void
 -> let o := OP[i], p1 := o.tacticProperties[1], p2 := o.tacticProperties[2] in
         (runLoop(o),
          //seeEnd(o),
          ss(o),
          //[0] ================ wpr ~S at ~A / ~A  =================== // o,x,y,
          whatif(o,p1,x,p2,y),
          // seeEnd(o),
          ss(o)) ]
          

[whatif(c:Player,p1:property,x:float,p2:property,y:float) : void
  -> let val := runLoop(c), x1 := read(p1,c.tactic), y1 := read(p2,c.tactic), v2 := 0.0 in
       (printf("== with ~S(~S) = ~A & ~S(~S) = ~A => ~A \n",p1,c,x1,p2,c,y1,val),
        write(p1,c.tactic,x), write(p2,c.tactic,y),
        v2 := runLoop(c),
        printf(">> with ~S(~S) = ~A & ~S(~S) = ~A => ~A \n",p1,c,x,p2,c,y,v2),
        write(p1,c.tactic,x1), write(p2,c.tactic,y1)) ]

// cute toy try all price beween 0� and 100� by increment of 5�
[wpr(i:integer)
 -> let c := OP[i], p1 := c.tacticProperties[1], p2 := c.tacticProperties[2],
        x1 := read(p1,c.tactic), y1 := read(p2,c.tactic), bestv := c.satisfaction * 0.99 in
      (for k1 in (0 .. 40)
       for k2 in (0 .. 50)
        let x := 2.0 * k1, y := 10.0 * k2, v2 := 0.0 in
            (write(p1,c.tactic,x), write(p2,c.tactic,y),
             v2 := runLoop(c),
             if (v2 > bestv)
                (bestv := v2, printf(">> with ~S(~S) = ~A & ~S(~S) = ~A => ~A \n",p1,c,x,p2,c,y,v2))),
        write(p1,c.tactic,x1), write(p2,c.tactic,y1)) ]

// hand tune for OP[1] - (one unique tactic shared for all ops)
// last run: August 5th, using go(1 .. 6)  - attention ! function of strategy (marketshare !)
// i    p               value   semantic                          comment
// 1    margin1         0.05    margin on top of local price      need many iterations !
// 2    margin2         0.17    margin on top of EDF price        same :)  (tune the sensitivity of S-curve)
// 3    storeIn%        1.2     fraction of average price that causes a inflow: get energy to fill storage
// 4    storeUse%       1.13    fraction of average price that causes to use the stored energy to fill demand
// 5    storeOut%       1.2     fraction of average buy price that causes an out flow: sell energy to supplier
// 6    buffer%         0.04    fraction of the store that is used as a buffer, the rest is a reserve (proctect against price fluctuations)
// need a 20 years period to see the benefits of investment !
// 7    usageRatio      0.3     (actually 0.0 if EDF is expensive and 1.0 otherwise :)  // decides to invest when usageRatio is high,
// 8    rateTarget      1.0     and when rate(production cost / whole sale) is low
// 9    storageRatio    1.0     decides to invest in storage when usage ratio is high
// 10   greenRatio,     1.0     factor in favor of green


// small scenario that makes building fossile more logical
// play with go(7) and go(8)
[sc1() -> sc1(500.0)]
[sc1(x:float) -> S1.oilPower := x,  EDF.tactic.basePrice := 70.0,
                 EDF.tactic.variablePrice := 200.0, go(false)]
// sc1(500.0) -> default
// go(7) -> 0.35


// play with green  - useful to compare 10MW vs 500MW (x)
// then, sc2(500) + go(10,x) to tune greenRatio
[sc2() -> sc2(200.0)] // default
[sc2(x:float) -> S1.greenPower := x,
                 S1.oilTrend := 0.1,                            // +4% bof, +10% : int�ressant
                 EDF.tactic.basePrice := 100.0,                  // only works when electricity is expensive :)
                 EDF.tactic.variablePrice := 200.0,
                 France.tactic.co2FirstPrice := 100.0,          // strong co2Tax
                 go(false) ]


// play with storage (20 years)
[sc3() -> sc3(2.0) ]
[sc3(x:float) -> S1.localStorage := x,
                 S1.oilTrend := 0.1,                     // this makes storage more attractive
                 S1.storageTrend := 0.2,                 // need to reduce price to make it work !
                 OTACTIC.storeIn% := 1.3,                // best values for sc3
                 OTACTIC.storeUse% := 1.9,
                 OTACTIC.storeOut% := 0.2,
                 France.tactic.co2FirstPrice := 100.0,     // strong co2Tax
                 go(false) ] // , YSHOW := OP[1]]

// ================ PART II : tune Suppliers ===========================================================

[ge(i:integer)
   -> go(false),
      let o := EDF, p := o.tacticProperties[i] in
         (o.satisfaction := mean(o.gradeGM),
          if (NIT = 1) seeYear(o) else seeEnd(o),
          OPTI := 0,
          //[0] ================ optimize ~S(~S) =================== // p,o,
          optimize(o,p),optimize2(o,p),
          runLoop(o),
          if (NIT = 1) seeYear(o) else seeEnd(o)) ]


// simple what-if
[ge(i:integer,x:Percent)
   -> go(false),
      let o := EDF, p := o.tacticProperties[i] in
         (if (NIT = 1) seeYear(o) else seeEnd(o),
          //[0] ================ whatif ~S(~S) =================== // p,o,
          whatif(o,p,x),
          ss(o),
          if (NIT = 1) seeYear(o) else seeEnd(o)) ]


// v0.4 : price optimization = what-if for OP[i] on x:firstPrice/y:secondPrice
[wpr(x:float,y:float) : void
 -> let o := EDF, p1 := o.tacticProperties[1], p2 := o.tacticProperties[2] in
         (runLoop(o),
          //seeEnd(o),
          ss(o),
          //[0] ================ wpr ~S at ~A / ~A  =================== // o,x,y,
          whatif(o,p1,x,p2,y),
          // seeEnd(o),
          ss(o)) ]
          

// cute toy try all price beween 0� and 100� by increment of 5�
[wpr()
 -> let c := EDF, p1 := c.tacticProperties[1], p2 := c.tacticProperties[2],
        x1 := read(p1,c.tactic), y1 := read(p2,c.tactic), bestv := c.satisfaction * 0.99 in
      (for k1 in (0 .. 60)
       for k2 in (0 .. 60)
        let x := 2.0 * k1, y := 2.0 * k2, v2 := 0.0 in
            (write(p1,c.tactic,x), write(p2,c.tactic,y),
             v2 := runLoop(c),
             if (v2 > bestv)
                (bestv := v2, printf(">> with ~S(~S) = ~A & ~S(~S) = ~A => ~A \n",p1,c,x,p2,c,y,v2))),
        write(p1,c.tactic,x1), write(p2,c.tactic,y1)) ]


// we need to adjust EDF's price to maintain a plausible balance
[adjust() : void
  -> ADJUSTBALANCE := true,
     ge(2),
     seeEnd(pb),
     ADJUSTBALANCE := false ]


// hand tune for EDF  (base = 40GW)
// 1    basePrice       37.0,          // default price  �/MWh
// 2    variablePrice   100.0,      // additional price when power is twice the nuke capacity
// this one is for investment
// 3    usageRatio      0.50,        // keep fossile usageRatio under X% (capacity to shave peaks)

// note : this cannot be optimized with single opt loops - two parameters are required

// ================= PART III : tune City & system parameters  (alpha,beta,gamma) ===============================

// tuning of the unique parameter (i = 1) - requires that alpha-testing is complete
[gf(i:integer)
   -> go(false),
      let c := CY[1], p := c.tacticProperties[i] in
         (c.satisfaction := mean(c.gradeGM),
          if (NIT = 1) seeYear(c) else seeEnd(c),
          OPTI := 0,
          //[0] ================ optimize ~S(~S) =================== // p,c,
          optimize(c,p),optimize2(c,p),
          runLoop(c),
          if (NIT = 1) seeYear(c) else seeEnd(c)) ]

// simple what-if
[gf(i:integer,x:Percent)
   -> go(false),
      let c := CY[1], p := c.tacticProperties[i] in
         (if (NIT = 1) seeYear(c) else seeEnd(c),
          //[0] ================ whatif ~S(~S) =================== // p,c,
          whatif(c,p,x),
          ss(c),
          if (NIT = 1) seeYear(c) else seeEnd(c)) ]

// hand tuned for CY[1]
// 1   negaEfficiency  -> 1.0  (function of the grading parameters :) !)

// (1) alpha = negawatt tuning (half S-curve)
// x & y represent the S-curve parameters, z is the oil trend (controls energy price, yields shaving)
[alpha(x:float,y:float,z:float) : void
 -> S1.oilPrice := z,
    let c := CY[1], val := runLoop(c), x1 := S1.negaEfficiency, y1 := S1.negaCapacity, v2 := 0.0 in
       (printf("[oil price: ~A] with sensivity = ~A & nega capacity = ~A  => ~A \n",z,x1,y1,val,z),
        S1.negaEfficiency := x,
        S1.negaCapacity := y,
        v2 := runLoop(c),
        printf("[oil price: ~A] with sensivity = ~A & nega capacity = ~A  => ~A \n",z,x,y,v2),
        S1.negaEfficiency := x1,
        S1.negaCapacity := y1,
        if (NIT = 1) seeYear(CY[1]) else seeEnd(CY[1])) ]

//  == Alpha results with z = 20  (price first year =
//  0.1/0.5 ->
//  0.2/0.5 ->
//  z = 40 => price ratio = 1.58
//  0.3/0.5  -> 31 / 400 TW
//  z = 80 => price = 1.80
//  0.2/0.5 -> 1.8 / 37
//  0.5/0.5 -> 4/ 37 (max !!)
// good range is  [0.2, 1.0]

// (2) beta = shaving S-curve tuning
// x & y represent the S-curve parameters, z is the oil trend (controls energy price, yields shaving)
[beta(x:float,y:float,z:float) : void
 -> S1.oilPrice := z,
    let c := CY[1], val := runLoop(c),
        x1 := Residentiel.shavingSensitivity, y1 := Residentiel.shavingCapacity, v2 := 0.0 in
       (printf("[oil price: ~A] with sensivity = ~A & shaving capacity = ~A  => ~A \n",z,x1,y1,val,z),
        Residentiel.shavingSensitivity := x,
        Residentiel.shavingCapacity := y,
        v2 := runLoop(c),
        printf("[oil price: ~A] with sensivity = ~A & shaving capacity = ~A  => ~A \n",z,x,y,v2),
        Residentiel.shavingSensitivity := x1,
        Residentiel.shavingCapacity := y1,
        if (NIT = 1) seeYear(CY[1]) else seeEnd(CY[1])) ]

// === beta results with z = 20, price = 128$
//  0.1/0.5 ->  21% shaved @ 20$, 22% at 40$(140), 23% at 80$(174)
//  0.5/0.6 ->  24%, 27%, 31%
//  1.0/0.6 ->  27%,30%,34%
//  good range is [0.02, 0.2]  - easier to see with year(1) + verbose


// (3) gamma (market share S-curve tuning)
// x & y represent the S-curve parameters
[gamma(x:float, y:float,z:float) : void
  -> OP[1].tactic.firstPrice := z,
     let val := runLoop(OP[1]), x1 := S1.marketSensitivity, y1 := S1.marketLimit, v2 := 0.0 in
       (printf("[op fPrice = ~A] with sensivity = ~A & market share limit = ~A  => ~A \n",z,x1,y1,val),
        S1.marketSensitivity := x,
        S1.marketLimit := y,
        v2 := runLoop(OP[1]),
        printf("[op fPrice = ~A] with sensivity = ~A & market share limit = ~A  => ~A \n",z,x,y,v2),
        S1.marketSensitivity := x1,
        S1.marketLimit := y1,
        if (NIT = 1) seeYear(OP[1]) else seeEnd(OP[1])) ]

// results
// 0.5/0.5   prix = 140 => 20%  120 = 26%
// 0.2/0.6   prix = 120 => 22%;  90 = 26%
// 0.1/0.5   prix = 120 => 21%,  90 = 23%
// good range is [0.2, 0.4]

// ================= PART IV : tune Regulator    ===============================

// tuning of the unique parameter (i = 1) - requires that alpha-testing is complete
[gg(i:integer)
   -> go(false),
      let c := France, p := c.tacticProperties[i] in
         (c.satisfaction := mean(c.gradeGM),
          if (NIT = 1) seeYear(c) else seeEnd(c),
          OPTI := 0,
          //[0] ================ optimize ~S(~S) =================== // p,c,
          optimize(c,p),optimize2(c,p),
          runLoop(c),
          if (NIT = 1) seeYear(c) else seeEnd(c)) ]

// simple what-if
[gg(i:integer,x:Percent)
   -> go(false),
      let c := France, p := c.tacticProperties[i] in
         (ss(c),
          if (NIT = 1) (seeYear(CY[1]), seeYear(c))
          else (seeEnd(CY[1]), seeEnd(c)),
          //[0] ================ whatif ~S(~S) =================== // p,c,
          whatif(c,p,x),
          ss(c),
          if (NIT = 1) (seeYear(CY[1]), seeYear(c))
          else (seeEnd(CY[1]), seeEnd(c))) ]

// hand tuned for CY[1]
// 1   co2Trend ->

// ================= PART V : tune envt parameters =====================================================

// this is the last section: run experiments -> GTES or GTES+Scenario
[f()
  -> NIT := 15,
     RANDOMIZE := true,
     E1.nSample := 10,
     verbose() := 0,
     run(E1)]

// ================== Part VI : fine tuning of optim methods during Nash loops ======================

// v0.4 restart nash tuning

// simple Nashloop test
[f1(n:integer)
  -> E1.nSample := 1,
     E1.nCities := 10,        // should be 10, but try 5, 10 and 20 to see if there are differences
     NIT := 15,               // should be 15, 5 is only for tuning
     go(E1,S1),               // default setting for experiment & scenario
     nash(n) ]

// creates a problem with partially optimized
[f6(n:integer)
  -> pb.timeUnit := 27,     // DEBUG => does not work with green or solar
     E1.nSample := 10,
     E1.nCities := 3,
     NIT := 5,               // should be 15, 5 is only for tuning
     verbose() := 0,
     go(E1,S1),
     nash(n) ]

[f6(n:integer,x:float) : void
  -> //[0] =============== K = ~A ============================= // x,
     K := x, f6(n) ]


// -------------------------- fragment for testing random walk --------------------------------------
// short cut
[rw(c:Player) -> rWalk(c,100,1.0,true,3) ]

[rw2(c:Player)
  -> RWALK := 1,
     time_set(),
     rWalk(c,1000,0.05,true,4),
     time_show(),
     RWALK := 0,
     rWalk(c,1,0.05,true,4) ]

// to tune we use a variant
[rw3(c:Player)
  -> RWALK := 1,
     time_set(),
     rWalk(c,1000,0.01,true,4),
     time_show(),
     RWALK := 0,
     rWalk(c,1,0.01,true,4) ]



// tests sur OP[1] (apr�s f6)  ---------------------
//
// optStep(OP[1],true,true)  ->  0.943559 (FP)
// twoOpt(OP[1],100) ->
// rw2(OP[1]) [37s]       -> 89.27911767%, 89.28777216%,
// rw3(OP[1]) [tabu = 4]  ->

// tests sur Supplier (apr�s f6) --------------------



//  Special set for tuning quickly with a small sample
[f6(e:Experiment,s:Scenario)
  -> pb.timeUnit := 3,
     E1.nSample := 1,
     E1.nCities := 3,
     NIT := 10,               // should be 15, 5 is only for tuning
     go(e,s),
     nash(1) ]


// results    EDF x OP
//  E             OP[1] sat                                      EDF sat
//  E1-SS         93.73% (19.98% ms, 1276 balance)             97.41  (79.78,  11625)
//  E1-HH         89% (20% 1517)                               96% (79.9 11700)
//  E1-SH         88% (19.8% 1401)                             97.38 (80.19 11700)
//  E1-HS         93% (20.26, 1245)                            96 (79.7 11744)


[stopt()
  -> OP[1].tactic.storeIn% := 1.15,
     OP[1].tactic.storeUse% := 0.15,
     OP[1].tactic.storeOut% := 0.9 ]


// bug with nuclear

[bug()
 -> TALK := 0,
    INVEST := 0,
    go(E1,S1) ]

