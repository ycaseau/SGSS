// ********************************************************************
// *       SGSS: Smart Grid Systemic Simulation                       *
// *       copyright (C) 2011 Yves Caseau                             *
// *       file: display.cl                                             *
// ********************************************************************

// this file contains the display & init methods

// ********************************************************************
// *    Part 1: Yearly display                                        *
// *    Part 2: Period diplay                                         *
// *    Part 3: Init                                                  *
// *    Part 4: Experiment Display                                    *
// ********************************************************************

// ********************************************************************
// *    Part 1: Yearly display                                        *
// ********************************************************************


// show the year (write later)
[seeYear(pb:Problem) : void
   -> seeYear(pb.regulator),
      seeYear(pb.supplier),
      seeYearCities(),
      seeYearOperators() ]

// these methods are what makes taking the yearly decision possible -----------------------------------
// see tactic.cl for actually taking the decision

// for a city, there are two questions:
//    - what was the final price ? (opertor vs supplier)
//    - is the current price high enough to invest in negawatt ?
[seeYearCities() : void
   -> printf("---- Cities uses ~ITWh (demand ~I=> ~InegW+~Ishaved) [~I] @ ~I$/MW -----\n",
             pF(sum(list{(c.energy / 1000.0) | c in City}),1),
             pF(sum(list{(c.energy + c.negaEnergy + c.shavedEnergy) | c in City}) / 1000.0,1),
             pF(sum(list{(c.negaEnergy / 1000.0) | c in City}),1),
             pF(sum(list{(c.shavedEnergy / 1000.0) | c in City}),1),
             p%(avg(list{c.satisfaction | c in City})),
             pF(sum(list{(c.expense / ((c.energy + c.negaEnergy + c.shavedEnergy) * 1000.0)) | c in City}),1)),
      printf("  average opertor price: ~I [dev ~I] supplier price:~I [dev ~I]\n",
             pF(avg(list{mean(c.opertor.stat.priceYM) | c in City}),2),
             p%(avg(list{stdev%(c.opertor.stat.priceYM) | c in City})),
             pF(mean(pb.supplier.stat.customerPriceYM),2),
             p%(stdev%(pb.supplier.stat.customerPriceYM))) ]

// for an opertor
//     - invest more in green
//     - invest more in oil
//     - invest more in storage
[seeYearOperators() : void
  ->   printf("---- Operators [~I @ ~I$] (forecast accuracy ~I) ---------------------\n",
              pF(avg(list{(o.satisfaction * 100.0) | o in Operator}),5),
              pF(avg(list{mean(o.stat.priceYM) | o in Operator}),3),
              p%(avg(list{mean(o.stat.forecastYM) | o in Operator}))),
       printf("  marketshare : ~I,  average production: ~IGW,  total energy: ~I/~I TWh\n",
              p%(avg(list{o.marketShare | o in Operator})),
              pF(sum(list{(mean(o.stat.powerYM) / 1000.0) | o in Operator}),3),
              pF(sum(list{(o.energy / 1000.0) | o in Operator}),3),
              pF(sum(list{(o.wholesale / 1000.0) | o in Operator}),3)),
       printf("  expenses: ~I, income:~I  -> balance: ~I M$\n",
              pF(sum(list{(o.expense / 1e6) | o in Operator}), 3),
              pF(sum(list{(o.income / 1e6) | o in Operator}), 3),
              pF(sum(list{((o.income - o.expense) / 1e6) | o in Operator}),6)),
       printf("  fossile average RoU:~I, average cost:~I of EDF \n",
              p%(avg(list{mean(o.fossile.usageYM) | o in Operator})),
              p%(avg(list{mean(o.fossile.rateYM) | o in Operator}))),
       printf("  ~I TWh of green energy, average RoU:~I, average cost:~I of EDF \n",
              pF(sum(list{(o.greenEnergy / 1000.0) | o in Operator}),3),
              p%(avg(list{mean(o.green.usageYM) | o in Operator})),
              p%(avg(list{mean(o.green.rateYM) | o in Operator}))),
       printf("  store: in (L/supply) = ~I/~I, out (L/supply) = ~I/~I GWh [~I]\n",
              pF(sum(list{(o.stat.localIn / 1e3) | o in Operator}),1),
              pF(sum(list{(o.stat.supplyIn / 1e3) | o in Operator}),1),
              pF(sum(list{(o.stat.localOut / 1e3) | o in Operator}),1),
              pF(sum(list{(o.stat.supplyOut / 1e3) | o in Operator}), 1),
              p%(avg(list{((o.stat.localOut + o.stat.supplyOut) / (8760.0 * o.storage.power)) | o in Operator}))) ]

// same for a single operator
[seeYear(o:Operator) : void
  ->   printf("~S [~Isat]  marketshare : ~I,  average production: ~IGW,  total energy: ~I/~I TWh\n",o,
              p%(o.satisfaction), p%(o.marketShare), pF(mean(o.stat.powerYM) / 1e3,3),
              pF(o.energy / 1000.0,3), pF(o.wholesale / 1000.0,3)),
       printf("  expenses: ~I, income:~I  -> balance: ~I M$\n",
              pF(o.expense / 1e6, 3), pF(o.income / 1e6, 3), pF((o.income - o.expense) / 1e6,3)),
       printf("  fossile energy: ~ITWh - average RoU:~I, average cost:~I of EDF \n",
              pF(o.fossileEnergy / 1e3,3), p%(mean(o.fossile.usageYM)), p%(mean(o.fossile.rateYM))),
       printf("  ~I TWh of green energy, average RoU:~I, average cost:~I of EDF \n",
              pF(o.greenEnergy / 1000.0,3), p%(mean(o.green.usageYM)), p%(mean(o.green.rateYM))),
       printf("  store: in (L/supply) = ~I/~I, out (L/supply) = ~I/~I GWh [~I]\n",
              pF(o.stat.localIn / 1e3,1), pF(o.stat.supplyIn / 1e3,1), pF(o.stat.localOut / 1e3,1),
              pF(o.stat.supplyOut / 1e3, 1), p%((o.stat.localOut + o.stat.supplyOut) / o.storage.yearProduction)) ]

// for a supplier, we assume that there is no yearly decision in the currunt v0
// in the future, we could invest in storage or change prices dynamically
//     - invest more in storage
[seeYear(s:Supplier) : void
    -> printf("---- Supplier -  ~S total energy: ~ITWh (~ITWh nc) [~I] -----------------\n",s,
              pF(s.energy / 1e3,2), pF(s.nukeEnergy / 1e3,2), p%(s.satisfaction)),
       printf("  avg. prod: ~IGW, peak:~IGW , oilPrice:~I$/MW\n",
              pF(mean(s.stat.powerYM) / 1e3,2),pF(s.peakPower / 1000.0,2),pF(mean(pb.oilPriceYM))),
       printf("  nuke: ~ITWh @ ~I$, fossile: ~ITWh @ ~I$\n",
              pF(s.nukeEnergy / 1e3),pF((s.nukeCost + s.nuclear.fixedCost) / (s.nukeEnergy * 1e3)),
              pF(s.fossileEnergy / 1e3),pF((s.fossileCost + s.fossile.fixedCost) / (s.fossileEnergy * 1e3))),
       printf("  expenses: ~I, income:~I  -> balance: ~I M$\n",pF(s.expense / 1e6, 3),
              pF(s.income / 1e6, 3), pF((s.income - s.expense) / 1e6,3)),
       printf("  fossile average RoU: ~I, average wholesale price: ~I, crisis ~I TWh\n",
              p%(mean(s.fossile.usageYM)), pF(mean(s.stat.priceYM),3), pF(s.crisis / 1000.0,3))]


// for the regulator : we show the CO2 emissions +
[seeYear(g:Regulator) : void
    -> printf("---- Regulator ~S [~S]------------------------------------------------------\n",g,g.satisfaction),
       printf("  total CO2: ~IMt,  total output: ~ITWh, balance:~I$\n",
              pF(g.co2Amount / 1e6,3), pF(g.energy / 1000.0,3), pF((g.income - g.expense) / 1e6,3)) ]


// make a business case for storage
// stats for last year
[seeStore(o:Operator) : void
  -> let sto := o.storage in
        (seeYear(o),
         printf("======   storage unit ~A MWh -> ~A% for buffer  ====\n",sto.capacity,o.tactic.buffer%),
         printf("average flowIn = ~AGWh, flowOut = ~AGWh compared to max = ~AGWh (~I)\n",
               sto.flowIn / 1e3,sto.flowOut / 1e3,sto.capacity,              // 1MWh cap -> 1Gw whole life
               p%(sto.flowOut / (sto.capacity * 1000.0))),                   // 1000 cycles
         printf("yearly cost = ~A + ~A [~A fixed] \n",sto.expense, sto.prodCost,sto.fixedCost),
         printf("yearly gain = ~A(buffer) + ~A(reserve)\n",sto.incomeB,sto.incomeR),
         printf("economic efficiency = ~I, usage price = ~A, balance = ~A\n",
                p%(((sto.incomeR + sto.incomeB) / sto.flowOut)  / (sto.expense / sto.flowR)),
                (sto.incomeR + sto.incomeB) / sto.flowOut,
                (sto.incomeB + sto.incomeR - sto.expense) - sto.prodCost)) ]


// make a business case for green
// stats for last year
[seeGreen(o:Operator) : void
  -> let gre := o.green in
        (seeYear(o),
         printf("======   green unit ~AMW   ====\n",gre.power),
         printf("average flowOut = ~AGWh compared to max = ~AGWh (~I)\n",
               gre.flowOut / 1e3, gre.capacity / 1e3,              // 1MWh cap -> 1Gw whole life
               p%(gre.flowOut / gre.capacity )),                   //=
         printf("yearly cost = ~A M$ prod price = ~A \n",gre.fixedCost / 1e6,
                gre.fixedCost / (gre.flowOut + 1.0)),
         printf("yearly gain = ~AM$ ",gre.income / 1e6),
         printf("usage price = ~A, balance = ~AM$\n",
                gre.income / gre.flowOut,
                (gre.income - gre.fixedCost) / 1e6)) ]


// look at the influence of price variation
//  (a) give some price data point: average, max, stdev
//  (b) give some info about energy production
//  (c) give some info about the city (shaving & negaWatt)
[seeDelta(o:Operator)
  -> let s := pb.supplier in
       (printf("CY power = ~I MW (~I dev)\n",pF(mean(o.city.powerYM)), pF(stdev(o.city.powerYM))),
        printf("EDF price=~I (~I dev) max = ~I\n",pF(mean(s.stat.priceYM)),
               pF(stdev(s.stat.priceYM)),pF(s.peakPrice - s.strategy.customerPrice)),
        printf("OP price=~I (~I dev) max = ~I\n",pF(mean(o.stat.priceYM),3),
               pF(stdev(o.stat.priceYM)),pF(o.peakPrice)),
        printf("OP produced ~I TWh\n",pF(o.fossileEnergy / 1e3,3)))
]


// ********************************************************************
// *    Part 2: Period diplay                                         *
// ********************************************************************


// seeEnd() show all actors, their satisfaction (grade) and the associated KPIs
// we also want a summary of investment, so that we can play

[seeEnd(pb:Problem)
  -> for a in Player  a.satisfaction := mean(a.gradeGM),
     seeEnd(pb.regulator),
     seeEnd(pb.supplier),
     seeEndCities(),
     seeEndOperators()]

[seeEndCities() : void
  -> printf(" Cities [~I]: price=~I$ bill=~I$ fear=~I shave=~I\n",
            p%(avg(list{mean(c.gradeGM) | c in City})),
            pF(avg(list{mean(c.priceGM) | c in City}),3),
            pF(avg(list{mean(c.billGM) | c in City}),3),
            pF(avg(list{mean(c.fearGM) | c in City}),3),
            p%(avg(list{mean(c.shaveGM) | c in City}))),
     printf("    => invest ~I$ : negaRatio = ~I -> negaEnergy = ~I/~ITWh\n",
            pF(sum(list{(c.negaSum / 1e6) | c in City}), 3),
            pF(avg(list{c.negaRatio | c in City}),3),
            pF(avg(list{(mean(c.negaGM) / 1000.0) | c in City}),3),
            pF(avg(list{(mean(c.energyGM) / 1000.0) | c in City}),3)) ]

[seeEnd(c:City) : void
  -> printf(" ~S [~I]: price=~I$ bill=~I fear=~I shave=~I\n",c,
            p%(mean(c.gradeGM)), pF(mean(c.priceGM),3), pF(mean(c.billGM),3),
            pF(mean(c.fearGM),3), p%(mean(c.shaveGM))),
     printf("    => invest ~I$ : negaRatio = ~I -> negaEnergy = ~I/~ITWh\n",
            pF(c.negaSum / 1e6, 3), pF(c.negaRatio,3), pF(mean(c.negaGM) / 1000.0,3),
            pF(mean(c.energyGM) / 1000.0,3)) ]


[seeEndOperators() : void
  -> printf(" operators [~I]: balance=~IM$[~I] income=~I share=~I price = ~I\n",
            p%(avg(list{mean(o.gradeGM) | o in Operator})),
            pF(sum(list{(mean(o.stat.balanceGM) / 1e6) | o in Operator}),3),
            p%(avg(list{stdev%(o.stat.balanceGM) | o in Operator})),
            pF(sum(list{(mean(o.stat.incomeGM) / 1e6) | o in Operator}),3),
            p%(avg(list{mean(o.stat.shareGM) | o in Operator})),
            pF(avg(list{mean(o.stat.priceGM) | o in Operator}),2)),
     printf("    => invest fossile:~IMW (RoU:~I), green:~IMW, store:~IMW\n",
            pF(sum(list{o.fossile.invest | o in Operator}),2),
            p%(avg(list{mean(o.stat.fossile%GM) | o in Operator})),
            pF(sum(list{o.green.invest | o in Operator}),2),
            pF(avg(list{o.storage.invest | o in Operator}),2)) ]

[seeEnd(o:Operator) : void
  -> printf("~S [~I]: balance=~IM$[~I] income=~I share=~I price=~I\n",o,
            p%(mean(o.stat.gradeGM)), pF(mean(o.stat.balanceGM) / 1e6,3),p%(stdev%(o.stat.balanceGM)),
            pF(mean(o.stat.incomeGM) / 1e6,3),
            p%(mean(o.stat.shareGM)), pF(mean(o.stat.priceGM),2)),
     printf("    => invest fossile:~IMW (RoU:~I), green:~IMW, store:~IMW\n",
            pF(o.fossile.invest,2), p%(mean(o.stat.fossile%GM)), pF(o.green.invest,2), pF(o.storage.invest,2)) ]

[seeEnd(s:Supplier) : void
  -> printf(" ~S [~I]: balance=~IM$[~I] margin=~I share=~I price=~I\n",s,p%(mean(s.gradeGM)),
            pF(mean(s.stat.balanceGM) / 1e6,3),p%(stdev%(s.stat.balanceGM)),
            p%(mean(s.stat.marginGM)), p%(mean(s.stat.shareGM)), pF(mean(s.stat.priceGM),2)),
     printf("    => invest nuke:~IMW, fossile:~IMW (RoU:~I)\n",
            pF(s.nuclear.invest,2), pF(s.fossile.invest,2), p%(mean(s.stat.fossile%GM))) ]

[seeEnd(g:Regulator) : void
  -> printf(" ~S [~I]: co2=~IMt balance=~IM$ output=~ITWh tax=~IM$\n",g,p%(mean(g.gradeGM)),
            pF(mean(g.co2GM) / 1e6,3), pF(mean(g.balanceGM) / 1e6,3),
            pF(mean(g.energyGM) / 1000.0,3), pF(mean(g.taxGM) / 1e6, 3)) ]


// ********************************************************************
// *    Part 3: Init                                                  *
// ********************************************************************



RANDOMIZE:boolean :: true
// initialisation
// create n cities and their opertors, with associated tactics
[init(e:Experiment) : void
  -> let r := e.regulator, s := e.supplier,
         sc := e.scenario, n := e.nCities,
         ct := e.cTactic, ot := e.oTactic,
         nop := (if e.oneOp? 1 else n),
         lgf := list<float>{randomIn(1.0 - sc.greenVariation, 1.0 + sc.greenVariation) | i in (1 .. n)} in
     (//[0] --- initiatization ----- (t:~A, s:~A, v:~A) ---- //, TALK, SHOW, verbose(),
     if RANDOMIZE (randomize(e),     // debug: remove randomize to make sure that we get the same result
                   printGTES(e)),
     pb.regulator := r,
     pb.supplier := s,
     pb.timeUnit := sc.timeUnit,
     pb.scenario := sc,
     s.tactic := copy(e.sTactic),
     r.tactic := copy(e.rTactic),
     pb.oilPrice := sc.oilPrice,
     pb.oilPriceYM := Measure(),
     pb.nashDistGM := Measure(),
     pb.yearPattern := randomize(r.yearPattern,1.0,1.0),    // change in v0.3 !
     pb.referencePrice := s.tactic.basePrice + s.customerCost + s.transportCost,
     r.strategy := e.rStrategy, s.strategy := e.sStrategy,
     r.tacticProperties := ListRegulator, s.tacticProperties := ListSupplier,
     assert(n <= NOP),
     for i in (1 .. size(Household)) (Household.instances[i].index := i),
     MaxValue[basePrice] := sc.maxBasePrice,                  // v0.2 - cap is set in Scenario
     MaxValue[variablePrice] := sc.maxVariablePrice,          // v0.4 - same (avoid too much variable)
     s.tactic.basePrice :min sc.maxBasePrice,
     // adapt the strategic objective to the number of cities !
     e.oStrategy.balance :/ n,
     e.oStrategy.income :/ n,
     e.oStrategy.basePower :/ n,              // v0.4 !
     //[SHOW] --- create cities ---- //,
     for i in (1 .. n)
         CY[i] := City(index = i,tactic = copy(ct), strategy = e.cStrategy, tacticProperties = ListCity),
     for i in (1 .. n)               // create operators
         let o := Operator(
                     index = i,
                     marketShare = sc.marketShare,                          // need to reset for runLoop
                     customerCost = sc.customerCost,
                     fossile = Oil&Gaz( power = sc.oilPower / nop),
                     green = Green(cost = sc.greenCost,                     // �/ MWh  - TCO
                                   power = sc.greenPower * lgf[i] / n ,        // power is prop to regional factor
                                   intermittent = lgf[i] * sc.intermittent),    // will work from 0 to i
                     storage = Storage( power = sc.localStorage / n,
                                        cost = sc.storageCost),
                     gradeGM = Measure(), 
                     gradeEM = Measure(), 
                     stat = init(OperatorStat()),
                     supplier = s,
                     prevPrice = pb.referencePrice,
                     tacticProperties = ListOperator,
                     strategy = e.oStrategy,
                     tactic = copy(ot)) in
            (if e.oneOp? (for c in City c.opertor := o, o.city := CY[1], break())
             else (CY[i].opertor := o, o.city := CY[i])),
     //[SHOW] --- setup the init value of computed slots --- //,
     for c in City init(c),                  // sets h.population :)
     init(s),
     init(r),
     nashStore(),
     walkStore(),
     cashAffine(),
     for e in EnergyCapacity init(e)) ]

// init an OperatorStat
[init(o:OperatorStat) : OperatorStat
  -> o.priceYM := Measure(), 
     o.powerYM := Measure(), 
     o.forecastYM := Measure(),
     o.balanceGM := Measure(), 
     o.incomeGM := Measure(), 
     o.shareGM := Measure(),
     o.priceGM := Measure(), 
     o.fossile%GM := Measure(),
     o.gradeEM := Measure(), 
     o.balanceEM := Measure(), 
     o.incomeEM := Measure(), 
     o.shareEM := Measure(),
     o.priceEM := Measure(), 
     o.fossile%EM := Measure(),
     o.fossileEM := Measure(), 
     o.greenEM := Measure(), 
     o.storageEM := Measure(),        
    o]


// v0.4: cash the affine curve
[cashAffine()
  -> for h in Household
       h.dayPatterns := list<float>{get(h.dayPattern,i) | i in (0 .. 24)}]

// reinit(e) is called at the end of an experiment step
// it resets the strategy and uses reinit(pb), which does the work
[reinit(e:Experiment) : void
  -> if RANDOMIZE (randomize(e),     // debug: remove randomize to make sure that we get the same result
                   printGTES(e)),
     reinit(pb),
     for c in City c.tactic := copy(e.cTactic),
     for o in Operator o.tactic := copy(e.oTactic),
     pb.supplier.tactic := copy(e.sTactic),
     pb.regulator.tactic := copy(e.rTactic),
     pb.regulator.tactic.co2FirstPrice := pb.scenario.co2FirstPrice ]          // a parameter
     
   

// city initialization
// rdx is the typical value of the city-part of the climate yearly change factor
// rdy is the daily variation
[init(c:City) : void
  -> let i := c.index, o := c.opertor, sc := pb.scenario,
         rdx := sc.cityVariation,
         n1 := pb.regulator.nbHouseholds / size(City) in
      (//[5] --- init ~S --- // c,
       CY[i] := c, OP[i] := o,
       c.negaRatio := 0.0,           // v0.2 : we measure incremental negaWatt when price is higher than pref
       c.negaSum := 0.0,
       c.yearPattern := randomize(pb.regulator.yearPattern,1.0,1.0),                 // a copy
       c.climateFactor := randomIn(0.0,rdx),       //  energy conso variation factor due to climate -> yearPattern
       c.holds := Household.instances[1],         // v0.4 = simple !
       c.nbHouseholds := n1,
       //[5] --- ~S -> ~S // c, c.holds,
       o.output := 0.0,   // dumb
       c.shaveYM := Measure(),
       c.powerYM := Measure(),
       c.gradeGM := Measure(),
       c.priceGM := Measure(),
       c.billGM := Measure(),
       c.fearGM := Measure(),
       c.negaGM := Measure(),
       c.shaveGM := Measure(),
       c.energyGM := Measure(),
       c.gradeEM := Measure(),
       c.priceEM := Measure(),
       c.billEM := Measure(),
       c.fearEM := Measure(),
       c.negaEM := Measure(),
       c.shaveEM := Measure(),
       c.negaSumEM := Measure(),
       c.negaRatioEM := Measure(),
       for h in Household h.population :+ n1) ]


// Energy type initialization (computes fixed part)
[init(e:Nuclear) : void
   ->  e.marginalCost := e.cost * (1 - pb.nuclearFixedCost),
       init(e,e.power) ]

[init(e:Nuclear,p:Power) : void
  -> e.capacity := p * 8000.0,                                       // naive but makes not difference :)
     e.fixedCost := pb.nuclearFixedCost * e.cost * e.capacity ]      // all in fixed cost

// Fossile production plant
[init(e:Oil&Gaz) : void
  ->  e.usageYM := Measure(), e.rateYM := Measure(),
      init(e,e.power)]                                   // sets the capacity and fixed cost as a function of power

// this is where we compute the fixed cost (yearly cost = invest * 8% - 20yrs- + TRI -> 6%)
// ext source: 1MW -> 1M� invest -> 80000� pour 5000MWh
// with oil(gaz/coal), 1MW -> 60 (2 * CARNOT) * 5000(h) -> 300000�  (pb.fixedCost = 0.3)
FOSSILE_HOURS :: 5000.0
[init(e:Oil&Gaz,p:Power) : void
 ->   e.capacity := e.power * 5000,
      e.fixedCost := e.capacity * pb.scenario.oilPrice * CARNOT * pb.fossileFixedCost ]   // capacity is assumed to be one year of prod

// Green production plant
// v0.4 : simpler formula based on 8000 hours
// + split between operator and regulator
[init(e:Green) : void
  ->   e.usageYM := Measure(), e.rateYM := Measure(),
      init(e,e.power) ]
[init(e:Green,p:Power) : void
   ->   let r := pb.regulator, d : = r.tactic.greenShare in
          (e.capacity := e.power * 8000.0 * e.intermittent,
           e.fixedCost := e.capacity * e.cost * (1 - d),                   // all in fixed costs (sun & wind are free)
           r.fixedCost :+ e.capacity * e.cost * d) ]


// all fixed costs, TCO is based on 5 years x 1000 cycles (TO FIX !!!)
//  = investment cost / 5 (duration)
//  example : 1MWh battery (4h x 250kW) costs 100k�  (at 100�/KWh )
//  on 5 years it delivers 1 GWh -> cost is 100� / MWh
//   fixed Cost is sell price / 5 (to make it simple) or 200 charge
//  1MWh fixed cost for 1 year is 20k�
RATE :: 0.05
[init(e:Storage) : void 
  -> init(e,e.power)]
[init(e:Storage,p:Power) : void
  -> e.capacity := p * 4,                         // capacity is 4 hours
     e.fixedCost := e.capacity * 1000.0 * RATE * e.cost ]   // TRI = 5%

DENUKE:boolean :: true

// addInvest adds an increment p to the power at a new cost c
[addInvest(e:Nuclear,p:Power) : void
  -> e.invest :+ p,         // keep sums of delta to allow for reverse ...
     e.power :+ p,          // adds the power
     let fc := e.fixedCost in
       (init(e,e.power),
        if DENUKE e.fixedCost :max fc) ]

// same for fossil fuel
[addInvest(e:Oil&Gaz,p:Power) : void
  -> e.invest :+ p,
     e.power :+ p,
     init(e,e.power ) ]

// capacity & costs have been tuned in the past
[addInvest(e:Green,p:Power,c:Price) : void
  -> let r := pb.regulator, d : = r.tactic.greenShare in
      (e.invest :+ p,
       e.power :+ p,
       e.cost := c,            // should be an average ...
       let r := pb.regulator, d : = r.tactic.greenShare, deltaC := p * 8000.0 * e.intermittent in
          (e.capacity :+ deltaC,
           e.fixedCost :+ deltaC * e.cost * (1 - d),                   // all in fixed costs (sun & wind are free)
           r.fixedCost :+ deltaC * e.cost * d)) ]

[addInvest(e:Storage,p:Power,c:Price) : void
  -> e.invest :+ p,
     e.power :+ p,
     init(e,e.power) ]

// init for a SupplierStat
[init(s:SupplierStat) : SupplierStat
  -> s.priceYM := Measure(),
    s.customerPriceYM := Measure(),
    s.powerYM := Measure(),
    s.priceGM := Measure(),
    s.balanceGM := Measure(),
    s.marginGM := Measure(),
    s.shareGM := Measure(),
    s.fossile%GM := Measure(),
    s.priceEM := Measure(),
    s.balanceEM := Measure(),
    s.marginEM := Measure(),
    s.shareEM := Measure(),
    s.fossile%EM := Measure(),
    s.nuclearEM := Measure(),
    s.fossileEM := Measure(),
    s]

// init for a supplier
[init(s:Supplier) : void
 -> //[0] init for supplier ~S // s, 
    s.firstCapacity := s.nuclear.power,
    s.stat := init(SupplierStat()),
    s.gradeGM := Measure(),   
    s.gradeEM := Measure(),
    s.prevPrice := pb.referencePrice,
    s.tactic.basePrice :min pb.scenario.maxBasePrice,    // v0.4
    // compute the max theorerical conso and computes the factor houseConso so that it matches the max country demand
    let r := pb.regulator, p := 0.0 in
      (for h in Household p :+  h.population * (1.0 - h.shavingFactor),
       pb.houseConso := r.power / p) ]

[init(r:Regulator) : void
  -> r.tactic.co2FirstPrice := pb.scenario.co2FirstPrice,          // a parameter
     r.gradeGM := Measure(),
     r.co2GM := Measure(),
     r.taxGM := Measure(),
     r.balanceGM := Measure(),
     r.energyGM := Measure(),
     r.gradeEM := Measure(),
     r.co2EM := Measure(),
     r.taxEM := Measure(),
     r.balanceEM := Measure(),
     r.energyEM := Measure() ]

// ********************************************************************
// *    Part 4: Experiment Display                                    *
// ********************************************************************

// display the result ---------------------------------------------------------------------------------

// this method is called at the end and shows the average results for all randomization
[display(e:Experiment)
  -> display(pb.regulator),
     display(pb.supplier),
     displayCities(),
     displayOperators(),
     printf("average convergence: ~I (Nash Distance)\n",p%(mean(pb.nashDistGM))) ]

[displayCities() : void
  -> printf(" Cities [~I]: price=~I$ bill=~I$ fear=~I shave=~I\n",
            p%(avg(list{mean(c.gradeEM) | c in City})),
            pF(avg(list{mean(c.priceEM) | c in City}),3),
            pF(avg(list{mean(c.billEM) | c in City}),3),
            pF(avg(list{mean(c.fearEM) | c in City}),3),
            p%(avg(list{mean(c.shaveEM) | c in City}))),
     printf("    => invest ~I$ : negaRatio = ~I -> negaEnergy = ~ITWh\n",
            pF(sum(list{(mean(c.negaSumEM) / 1e6) | c in City}), 3),
            pF(avg(list{mean(c.negaRatioEM) | c in City}),3),
            pF(avg(list{(mean(c.negaEM) / 1000.0) | c in City}),3)) ]


[displayOperators() : void
  -> printf(" operators [~I]: balance=~IM$[~I] income=~I share=~I price = ~I\n",
            p%(avg(list{mean(o.gradeEM) | o in Operator})),
            pF(sum(list{(mean(o.stat.balanceEM) / 1e6) | o in Operator}),3),
            p%(avg(list{stdev%(o.stat.balanceEM) | o in Operator})),
            pF(sum(list{(mean(o.stat.incomeEM) / 1e6) | o in Operator}),3),
            p%(avg(list{mean(o.stat.shareEM) | o in Operator})),
            pF(avg(list{mean(o.stat.priceEM) | o in Operator}),2)),
     printf("    => invest fossile:~IMW (RoU:~I), green:~IMW, store:~IMW\n",
            pF(sum(list{mean(o.fossileEM) | o in Operator}),2),
            p%(avg(list{mean(o.fossile%EM) | o in Operator})),
            pF(sum(list{mean(o.greenEM) | o in Operator}),2),
            pF(avg(list{mean(o.storageEM) | o in Operator}),2)) ]

[display(s:Supplier) : void
  -> printf(" ~S [~I]: balance=~IM$[~I] margin=~I share=~I price=~I\n",s,p%(mean(s.gradeEM)),
            pF(mean(s.balanceEM) / 1e6,3),p%(stdev%(s.balanceEM)),
            p%(mean(s.marginEM)), p%(mean(s.shareEM)), pF(mean(s.priceEM),2)),
     printf("    => invest nuke:~IMW, fossile:~IMW (RoU:~I)\n",
            pF(mean(s.nuclearEM),2), pF(mean(s.fossileEM),2), p%(mean(s.fossile%EM))) ]

[display(g:Regulator) : void
  -> printf(" ~S [~I]: co2=~IMt balance=~IM$ output=~ITWh tax=~IM$\n",g,p%(mean(g.gradeEM)),
            pF(mean(g.co2EM) / 1e6,3), pF(mean(g.balanceEM) / 1e6,3),
            pF(mean(g.energyEM) / 1000.0,3), pF(mean(g.taxEM) / 1e6, 3)) ]




