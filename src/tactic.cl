// ********************************************************************
// *       SGSS: Smart Grid Systemic Simulation                       *
// *       copyright (C) 2011-2012 Yves Caseau                        *
// *       file: tactic.cl                                            *
// ********************************************************************

// this file contains the four tactic-driven-sub-models that determine energy production
// and consumption

// DEBUG - need to clean for performance !

DWINDOW:Interval :: (-1 .. 0)   // debug time interval with more verbosity
MONTHTALK:integer :: 10         // talks one day every 10 days
[show?() : boolean 
   => false]    // (pb.time % DWINDOW | verbose() >= SHOW)]
[talk?() : boolean 
   => false]    // ((day_(pb.time) mod MONTHTALK = 0) & verbose() > 0)]

SEESTAT:boolean :: true          // may be turned to off when no precise stats are needed

// ********************************************************************
// *    Part 1: Consumption model for households + yearly strategy    *
// *    Part 2: Operator buy/supply model + yearly strategy           *
// *    Part 3: Supplier model + investment strategy                  *
// *    Part 4: Government model                                      *
// *    Part 5: Grading (measuring satisfaction)                      *
// ********************************************************************

// ********************************************************************
// *    Part 1: Consumption model for households + yearly strategy    *
// ********************************************************************

// computes the energy consumption
ESHOW:integer :: 5

[getEnergy(c:City) : void
  -> let d := 0.0, o := c.opertor, sp := 0.0, op := 0.0, cs := 0.0,
         h := c.holds, n := c.nbHouseholds,
         dh := 0.0, cf := pb.scenario.energyVariation in   // dh: init demand
       (dh := n * pb.houseConso * c.yearPatternDay *
              randomize(h.dayPatterns[hour_(pb.time) + 1],1.0 - cf, 1.0 + cf),
        //[ESHOW] take the effect of negawatt ~A into account, starting with ~A MW // c.negaRatio,dh,
        pb.regulator.energy :+ (dh * c.negaRatio * pb.timeUnit / 1000.0),
        // we count negaWatts in total output for regulator (economy output)
        c.negaEnergy :+ (dh * c.negaRatio * pb.timeUnit / 1000.0),
        dh :* (1.0 - c.negaRatio),
        // compute shaving (d -> p) due to price signal for both operator & supplier side
        let oShave := hShave(h,1.0,o.price),
            sShave := hShave(h,pb.supplier.strategy.shavingFactor,pb.supplier.customerPrice) in
               (//[ESHOW] [~A:~A] shaving factor: ~A (price ratio = ~A) -> ~A // pb.time,o.price,oShave, log(o.price / pb.referencePrice),dh,
                cs :+ oShave * dh * o.marketShare,
                op :+ (1.0 - oShave) * dh * o.marketShare,
                cs :+ sShave * dh * (1.0 - o.marketShare),
                sp :+ (1.0 - sShave) * dh * (1.0 - o.marketShare)),
         if show?() printf("[~A] getEnergy(~S) = ~I+~I MW (~I shave)\n",pb.time,c,pF(op,2),pF(sp,2),pF(cs,2)),
         c.opower := op, c.spower := sp,
         c.power := c.opower + c.spower,
         if SEESTAT c.dayPower :+ c.power,
         c.powerYM :add c.power,
         c.shaveYM :add cs / (cs + c.power),
         c.shavedEnergy :+ (cs * pb.timeUnit) / 1000.0,
         c.energy :+ (c.power * pb.timeUnit) / 1000.0) ]


// access to the Scurve
[hShave(p:Price) : float 
      ->  let c := CY[1], h := c.holds in hShave(h,1.0,p) ]
[hShave(h:Household,f:float,p:Price) : float
      -> f * sCurve(h.shavingFactor,h.shavingCapacity,h.shavingSensitivity,
                    (p - pb.referencePrice) / pb.referencePrice) ]

// this is the master pattern curve, from which the actual consumption is derived
// (and from which the supplier makes its predictions)
// it is the combination of a yearly pattern (day by day) and a daily pattern (hour by hou
// generic version
[patternConso(t:Time) : float
  -> let p := 0.0 in
      (for h in Household      // minimum expected shaving
          p :+ (h.dayPatterns[hour_(t) + 1] * h.population *  (1.0 - h.shavingFactor)),
       p :* pb.yearPatternDay,
       p * pb.houseConso)]

// city specific version for the supplier
[patternConso(t:Time, c:City) : float
  -> let  p := 0.0, h := c.holds, n := c.nbHouseholds in
      (p :+ n * c.yearPatternDay * h.dayPatterns[hour_(t) + 1],
       p * pb.houseConso * (1.0 - c.negaRatio)) ]

INVEST:integer :: 2

// make the yearly changes for a city:
//   (a) change the marketshare according to the S-curve formula
[endYear(c:City) : void
  -> let pop := mean(c.opertor.stat.priceYM),  psup := mean(pb.supplier.stat.customerPriceYM),
         sc := pb.scenario,  pf := log(psup / pop) in                                   // index for marketShare S-curve
       (c.opertor.marketShare := sCurve(sc.marketShare,sc.marketLimit,sc.marketSensitivity,pf),
        //[INVEST] [~A] ~S's marketshare becomes ~A since price ratio is ~A/~A // year_(pb.time),c,c.opertor.marketShare,psup,pop,
        nil) ]

// City Invest: invest in negawatt according to the average price
//         use a 1/2 S-curve :  x -> axM / (1 + ax)    (0->0,  inf-> M,  a = dF/dx(O) / M
// the tactic tells how aggressive or careful c is, by multiplying the real price
// note:
//  (a) We assumes a 5% ROI, that is the yearly cost is 5% less than the energy cost
//  (b) we only take into account "marginal" negawatt when price is higher than referencePrice
NINVEST:integer :: 2
[runInvest(c:City) : void
  -> let psup := mean(pb.supplier.stat.customerPriceYM),
         pref := pb.referencePrice,
         ptactic := psup * c.tactic.negaEfficiency,             // c considers that "real" price is ptactic
         sc := pb.scenario, M := sc.negaCapacity, a := sc.negaEfficiency / M,
         cf := c.negaRatio,                                    // current nega factor
         x := pos(ptactic - pref) / pref,  nf := (a * x * M / (1.0 + a * x))  in             // 1/2 S-curve
       (//[NINVEST] negaRatio moves from ~S to ~S (x = ~A%) [~A/~A] // cf, nf,x * 100.0,ptactic,pref,
        if (nf > cf)
           let cost := (nf - cf) * c.energy * ptactic * 0.95 in          // ROI is based on 5% return rate
           (c.negaRatio := nf,
            c.negaSum :+ cost)) ]


// ********************************************************************
// *    Part 2: Operator buy/supply model + yearly strategy           *
// ********************************************************************

STALK:integer :: 5               // verbosity debug level for store flows
TSHOW:integer :: 5               // verbosity control for EnergyTactic

// marginal production cost for operator
[fossilePCost() : Price 
    => CARNOT * pb.oilPrice + co2Tax(pb.regulator)]

// this is the energy production tactic for a given demand d (which is either real or estimated)
// [ this is the core of the model ! the main method of the main player ]
// this methods follows the decision process in three steps (green, storage, oil)
// and records the associated quantities (flow variables)
// the reference prices are either the suppliers (buy/sell) or the market price (buy/sell)
// WARNING: this method is called twice, once with the false (estimated) demand, once with the true
//          figure => NO book-keeping is allowed
[energyTactic(o:Operator,d:Power) : void
   -> let s := o.supplier, ot := o.tactic, d1 := d, alpha := ot.buffer%,     //
          x1 := s.price, y1 := s.buy,
          x2 := s.wPrice, y2 := s.wBuy,         // (weighted is a cheap average)
          p := production(o.green,pb.time),     // (production is a function of time)
          qty := o.storage.power, u := o.storage.buffer, f := (o.storage.capacity * alpha - u),
          // qty, u(sed capacity) and f(ree buffer) are storage stats
          w := o.fossile.power, w1 := fossilePCost() in    // w1 is marginal production cost
      (o.localCost := 0.0, o.greenPower := p,
       o.inBuffer := 0.0, o.outBuffer := 0.0,              // buffer is local usage
       o.inReserve := 0.0, o.outReserve := 0.0, o.sellReserve := 0.0, // 5 storage flows
       o.sell := 0.0, o.buy := 0.0,
       //[TSHOW] --- step 1: find out what to do with the ~A MW produced by green versus demand ~A // p,d,
       if (d < p)
          let p2 := p - d, f2 := min(qty,f / pb.timeUnit) * alpha,    // f2: alpha-share available for store
              p3 := min(p2,f2) in
            (d1 := 0.0,                      // totally covered
             p2 :- p3,
             o.inBuffer := p3,               // fill buffer when we have too much
             if (o.inBuffer + u > o.storage.capacity) error("[0] storage debug at ~A for ~S",pb.time,o),
             if (p2 > 0.0)        // still too much
                (o.localCost :- p2 * y1, o.sell := p2))
       else (d1 := d - p),         // all local green is used
       o.localCost :+ p * o.green.cost,
       //[TSHOW] ---- step 2: find out what to do with the store (alpha and non-alpha) for demand ~A // d1,
       let u1 := min(qty, u / pb.timeUnit) * alpha, u2 := min(u1,d1) in     // management of alpha part
          (//[TSHOW] get ~A MW from (buffer) store [~S/~S*~S] // u2,u,o.storage.capacity,alpha,
           d1 :- u2,
           o.outBuffer := u2),                       // no cost : all fixed cost for storage (better use it !)
        let v := o.storage.reserve, f3 := (o.storage.capacity * (1.0 - alpha) - v) in
          (if (v > 0.0 & x1 > x2 * ot.storeUse%)          // decide to use stock for our consumption
              let d2 := min(d1,min(qty,v / pb.timeUnit)) in
                 (//[STALK] [~A] ~S decide to get ~A MW from (reserve) store since price is ~A vs ~A [~S/~S*~S] // pb.time, o, d2, x1,x2,v,o.storage.capacity,(1.0 - alpha),
                  d1 :- d2,
                  o.outReserve := d2)
           else if (v > 0.0 & y1 > y2 * ot.storeOut%)                    // sell energy
              (o.sellReserve := min(qty, v / pb.timeUnit))
           else if (f3 > 0.0 & x1 < x2 * ot.storeIn%)                    // buy energy to store, it's cheap
              (//[STALK] [~A] ~S decide to buy ~A MW into reserve since price is ~A vs ~A // pb.time, o, f3 / pb.timeUnit, x1, x2,
               o.inReserve := min(qty,f3 / pb.timeUnit))),
       //[TSHOW] ---- step3 :  produce or buy whatever is still necessary (~A) from supplier // d1,
       //[DEBUG] [~A] power = ~A -> ~A vs EDF:~A [oil:~A$] // pb.time,d,w1,x1,pb.oilPrice,
       if (w1 < x1)
          let d2 := min(d1,w) in                  // produce the first part with oil&gaz
            (//[TSHOW] produce ~S MW from local fossile prod (d:~S -> ~S vs cap ~S) // d2,d,d1,w,
             d1 :- d2,
             o.fossilePower := d2,
             o.localCost :+ d2 * w1)
       else o.fossilePower := 0.0,
       (//[TSHOW] buys ~A MW from supplier (local price =~A vs EDF ~A) // d1,w1,x1,
        o.buy := d1, o.localCost :+ d1 * x1),
       if (w1 < y1 & o.fossilePower < w)         // extra capacity can be sold to supplier
          (o.sell :+ (o.fossilePower - w)),
       o.localCost :/ d)]                          // average local cost over 3 forms (green, stored, oil&gaz)


// opertor price fixing is simple (v0) :
// local production is sold with a simple markup
// imported energy is sold with a higher markup
// the key question is the balance between local/imported
[getPrice(o:Operator) : void
  -> let ot := o.tactic,
         p := o.output,                    // previous power (T - 1)
         p0 := o.strategy.basePower,       // base power (flat price)
         p1 := patternConso(pb.time - 1,o.city),
         p2 := patternConso(pb.time,o.city),
         d := p * (p2 / p1) in    // expected power, differential method
      (if (p = 0.0) d := p2 * o.marketShare,        // necessary for first run !
       energyTactic(o,d),         // find the energy mix for pow
       let q1 := o.greenPower + o.fossilePower + o.outBuffer + o.outReserve, q2 := o.buy in
          (if show?() //[DEBUG] ... ~S produces ~A MW @ ~A$ and buys ~A @ ~A$ // o,q1,o.localCost,q2,o.supplier.price,
           o.price := ot.firstPrice + o.customerCost + pos(o.city.power - p0) * ot.secondPrice / p0,
           if show?() //[0] price = ~A (customer cost = ~A, price = ~A / ~A) x ~AGW/~A // o.price, o.customerCost,ot.firstPrice,ot.secondPrice,o.city.power,p0,
           //[5] ~S demand = ~A vs pivot = ~A => price = ~A // o,o.city.power,p0,o.price,
           o.stat.priceYM :add o.price,
           o.peakPrice :max o.price,
           o.output := d,                          // what we believe at that point
           if show?() printf("[~A] getPrice(~S)= ~I $/MW (for ~I MW)\n",pb.time,o,pF(o.price,2),pF(d,2)))) ]

// older method
//          o.price := ( (o.localCost * q1) * (1.0 + ot.margin1) +
//                        (o.supplier.price * q2) * (1.0 + ot.margin2)) / (q1 + q2) +
//                      o.customerCost,

STO1:boolean :: false

// energy policy : how to cope with demand d from city
// use the master method "energyTactic" to compute the following flows:
//     o.sell                           : sell capacity to supplier
//     o.buy                            : buy capacity to supplier
//     o.inReserve / o.outReserve       : addition/removal of MWh into the local store (reserve part)
//     o.sellReserve                    : sell MWh to supplier
//     o.inBuffer / o.outBuffer         : addition/removal from the buffert part
//
[getProduction(o:Operator) : void
  -> let c := o.city, d := c.opower, s := o.supplier, sto := o.storage in
       (if (sto.buffer > sto.capacity) error("[1] storage debug at ~A for ~S",pb.time,o),
        energyTactic(o,d),                  // computes the right flow
        add(o.stat.forecastYM, (o.output / d)),
        o.output := d,                      // we serve the demand (stored for future use)
        // storage management
        sto.reserve :+ (o.inReserve - o.outReserve - o.sellReserve) * pb.timeUnit,
        if (sto.reserve < 0.0 & sto.reserve > -1e-10) sto.reserve := 0.0,  // strange FP rounding
        if (sto.reserve < 0.0 | sto.reserve > sto.capacity) error("storage bug at ~A for ~S",pb.time,o),
        sto.buffer :+ (o.inBuffer - o.outBuffer) * pb.timeUnit,
        if (sto.buffer < 0.0 & sto.buffer > -1e-10) sto.buffer := 0.0,  // strange FP rounding
        if (sto.buffer < 0.0 | sto.buffer > sto.capacity) error("storage b bug at ~A for ~S",pb.time,o),
        ;if (o.inReserve > 0.0 | o.outReserve > 0.0 | o.sellReserve > 0.0 |
        ;    o.inBuffer > 0.0 | o.outBuffer > 0.0)
        ;   trace(STALK,"~S>>> ~S@~A$: reserve ~A /~A/s~A  buffer ~A/~A\n",o, sto,s.price,
        ;         o.inReserve, o.outReserve, o.sellReserve, o.inBuffer, o.outBuffer),
        o.stat.localIn :+ (pb.timeUnit * o.inBuffer),               // buffer se remplit (local production)
        o.stat.supplyIn :+ (pb.timeUnit * o.inReserve),             // reserve se remplit
        o.stat.localOut :+ (pb.timeUnit * (o.outBuffer + o.outReserve)),   // storage est utilis�
        o.stat.supplyOut :+ (pb.timeUnit * o.sellReserve),                 // storage est vendu
        // assert((o.outReserve + o.outBuffer + o.sellReserve) <= o.storage.power),
        // local production
        let co := o.fossilePower,
            t := co * co2Tax(pb.regulator) * pb.timeUnit in               // CO2 tax
                  (o.expense :+ t, pb.regulator.income :+ t,
                   if (co > 0.0) o.fossile.rateYM :add (fossilePCost() / s.price),
                      // rate = oil price / EDF price
                   if SEESTAT
                      (o.stat.dayFPower :+ co, 
                       o.stat.dayGPower :+ o.greenPower, 
                       o.stat.dayPrice :+ o.price),
                   o.fossileEnergy :+ co * pb.timeUnit / 1000.0,
                   o.fossile.usageYM :add (co / o.fossile.power)),              // usage is %
        let p := (o.greenPower + o.fossilePower + o.outBuffer + o.outReserve) in
           (o.stat.powerYM :add p,                                  // power that is produced locally (MW)
            o.green.usageYM :add (o.greenPower / o.green.power),
            if (o.greenPower > 0.0)  o.green.rateYM :add (o.green.cost / s.price),
            o.greenEnergy :+ (o.greenPower * pb.timeUnit / 1000.0),
            o.energy :+ (p * pb.timeUnit / 1000.0),           // GWh
        pb.regulator.energy :+ (p * pb.timeUnit / 1000.0),
        pb.regulator.co2Amount :+ (o.fossilePower * pb.timeUnit * pb.co2Factor)),
        // finance management
        ; if (o.index = OSHOW) trace(0,"--- ~S: produces ~A/~A MW, buys ~AMW, storage(~A,~A)\n",o,
        ;                           o.greenPower,o.fossilePower,o.buy,sto.buffer,sto.reserve),
        o.income :+ (o.sell + o.sellReserve) * s.buy * pb.timeUnit,      // B2B revenue,
        o.sellIncome :+ (o.sell + o.sellReserve) * s.buy * pb.timeUnit,   // debug
        // price is in �/MW.h -> x timeUnit !
        let flow := d * o.price * pb.timeUnit in
            (o.income :+ flow, c.expense :+ flow),           // B2C revenue
        o.expense :+ d * o.customerCost * pb.timeUnit,        // customer management
        //[5] ~S : +~A -> ~A // o, (o.sell + o.sellReserve) * s.buy * pb.timeUnit,o.income,
        //[5] ~S : ~A + ~A // o, o.expense, o.buy * s.price * pb.timeUnit,
        if show?() printf("[~A] getProduction(~S)=~I(buy) ~I(fossile) ~I(green) ms:~I@~I\n",pb.time,o,
                           pF(o.buy,2),pF(o.fossilePower,2),pF(o.greenPower,2),p%(o.marketShare),
                           pF(o.price,2)),
        o.wholesale :+ (o.buy * pb.timeUnit / 1000.0),
        o.expense :+ o.fossilePower * CARNOT * pb.oilPrice * pb.timeUnit,
        if SEESTAT // this fragment is not necessary for the simulation, but helps to produce seeStore(op)
        let sto := o.storage, gre := o.green in
          (o.fuelExpense :+ o.fossilePower * CARNOT * pb.oilPrice * pb.timeUnit, // DEBUG
           o.edfExpense :+ (o.buy + o.inReserve) * s.price * pb.timeUnit, // DEBUG
           o.cusExpense :+ d * o.customerCost * pb.timeUnit,
           gre.flowOut :+ (o.greenPower * pb.timeUnit),
           gre.income :+ (o.greenPower * s.price * pb.timeUnit),
           ;if (o.index = 1 & (STALK = 0 | STO1))
           ;   printf("[~A] OP[1] In:(L~A+~A) Out:(L~A+~A+~A) flowIn:~A+~A@~A\n",pb.time,
           ;           o.inBuffer,o.inReserve,
           ;           o.outBuffer, o.outReserve, o.sellReserve,
           ;           sto.flowIn,o.inBuffer * pb.timeUnit,s.price),
           sto.yearProduction :+ pb.timeUnit * sto.power,
           sto.flowIn :+ pb.timeUnit * (o.inReserve + o.inBuffer),
           sto.flowOut :+ pb.timeUnit * (o.outBuffer + o.outReserve + o.sellReserve),
           sto.flowR :+ pb.timeUnit * o.inReserve,               // flow from outside
           sto.prodCost :+ pb.timeUnit * (o.outBuffer + o.outReserve + o.sellReserve) * sto.cost,
           sto.expense :+ o.inReserve * s.price * pb.timeUnit,
           sto.incomeB :+ o.outBuffer * s.price * pb.timeUnit,
           sto.incomeR :+ (o.sellReserve * s.buy + o.outReserve * s.price) * pb.timeUnit),
        if (sto.buffer > sto.capacity) error("[2] storage debug at ~A for ~S",pb.time,o),
        o.expense :+ pb.timeUnit * (o.outBuffer + o.outReserve + o.sellReserve) * sto.cost,
        o.expense :+ (o.buy + o.inReserve) * s.price * pb.timeUnit) ]


// production for a Green energy source
[production(x:Green,t:Time) : Power
  -> let h := hour_(t) in
         (if (h <= integer!(24.0 * x.intermittent)) x.power else 0.0) ]

// investment strategy for operators
// - add fossile production capability (according to tactic)
// - add green; increment by 10% when the REX is positive
// - add storage: increment (same green
[runInvest(o:Operator) : void
   -> let ot := o.tactic, pDelta := pb.scenario.powerIncrement in
       (let cf := o.fossile.power,oil% := mean(o.fossile.usageYM), rate% := mean(o.fossile.rateYM) in
          (if (oil% > ot.usageRatio & rate% < ot.rateTarget)
              (//[INVEST] ~S adds fossile capacity to lower usage ratio (~A) : ~AMW // o, oil%, pDelta,
               addInvest(o.fossile,pDelta))
            else trace(INVEST,"~S does not change fossile : oil% = ~A and rate% = ~A\n",o,oil%,rate%)),
        let g := o.green, green% := mean(g.usageYM), sDelta := pb.scenario.localStorage,  // pDelta * pb.regulator.tactic.greenToStorage,
            avgBuy := mean(pb.supplier.stat.priceYM), sto := o.storage in
          (if (greenPrice(pb.time) * (green% / g.intermittent) <= ot.greenRatio * avgBuy)
             (//[INVEST] [~A] increase the green capacity of ~S by ~S [g:~A vs buy:~A] (ratio: ~A) // year_(pb.time),o,pDelta,greenPrice(pb.time),avgBuy,ot.greenRatio,
              addInvest(g,pDelta, greenPrice(pb.time)))
           else trace(INVEST," greenPrice = ~A : too expensive vs ~A\n",greenPrice(pb.time),avgBuy),
           trace(INVEST," storage ~S[~AMW] produces ~SGWh, at a cost of ~S\n",o,sto.capacity,
                   sto.flowOut / 1000,sto.cost),
           if (storagePrice(pb.time) <= ot.storageRatio * avgBuy)
              (//[INVEST] [~A] increase the storage capacity of ~S by ~S [st:~A vs buy:~A] // year_(pb.time), o, sDelta ,storagePrice(pb.time),avgBuy,
               addInvest(sto, sDelta, storagePrice(pb.time)))
           else trace(INVEST," storagePrice =~A: too expensive vs ~A\n",storagePrice(pb.time),avgBuy))) ]


// greenPrice(t:Time) is the production price (TCO) at the current time (factors in the price decline)
// note that a fraction is discounted by regulator's subsidy
[greenPrice(t:Time) : Price
  -> let s := pb.scenario, discount := pb.regulator.tactic.greenShare  in
        (s.greenCost * (1.0 - discount)) ]


// same for storage price
[storagePrice(t:Time) : Price
  -> let s := pb.scenario in
        (s.storageCost * ((1.0 - s.storageTrend) ^  float!(year_(t)))) ]


// ********************************************************************
// *    Part 3: Supplier model + investment strategy                  *
// ********************************************************************

CTmin:integer :: 0
CTmax:integer :: 0

// dynamic behavior : price fixing  (wholesale price)
// price depends on:
//    (a) demand at T - 1
//    (b) pattern
//    (c) oil price  (marginal price)
// simple formula = A + B x Sup(conso - NukeProd, 0)  - A is the low price, B is the cost factor
//
[getPrice(s:Supplier) : void
  -> let p := s.power,            // previous power (T - 1)
         p1 := patternConso(pb.time - 1),    p2 := patternConso(pb.time),
         p0 := s.strategy.basePower,                             // pivot (cannot be more than nuke capacity)
         w1 := s.nukeCapacity,                                   // what can be done with nuke
 //        p0 := min(w1,s.strategy.basePower),                     // pivot (cannot be more than nuke capacity)
         pow := (if (p = 0.0) p2 else p * (p2 / p1)),            // expected power,
         w2 := s.fossile.power,                                  // what can be done with fossile
         oilp := pb.oilPrice,
         px := (if (pow > w1) ((pos(pow - (w1 + w2)) * OPENFACTOR +
                                pos(pow - w1) * CARNOT) * oilp / pos(pow - w1)) +
                              s.transportCost + co2Tax(pb.regulator)  // co2Tax on non-nuke -> passed to customers
                else 0.0) in
      (s.price := (min(pow,p0) * s.tactic.basePrice + pos(pow - p0) * (px + s.tactic.variablePrice)) / pow,
       if (pow < p0) s.buy := s.marginalCost                      // do not want to buy, really :)
       else s.buy := pb.oilPrice * s.strategy.oilMargin,            // buy at market price
       if show?() printf("[~A] getPrice(~S) -> ~I(sell) / ~I(buy) [expect ~I-~I GW]\n", date(pb.time), s,
                         pF(s.price,2),pF(s.buy,2),pF(pow / 1000.0,2),pF(patternConso(pb.time) / 1000.0,2)),
       s.customerPrice := s.price + (s.strategy.customerPrice),           // end price adds a markup
       s.peakPrice :max s.customerPrice,
       s.stat.dayPrice :+ s.price,
       s.stat.priceYM :add s.price,
       s.stat.customerPriceYM :add s.customerPrice,
       s.wPrice := (0.05 * s.price + 0.95 * s.wPrice),
       s.wBuy := (0.05 * s.buy + 0.95 * s.wBuy)) ]


// energy management
OPENFACTOR :: 10.0                  // when we buy at the worst possible time, we pay 3 times the oil-production price ...
[getProduction(s:Supplier) : void
  -> let p := 0.0, cn := s.nukeCapacity, fn := s.fossile.power in
       (//[SHOW] === energy production for ~S // s,
        for o in Operator
           (// part of the demand that is direcly addressed to supplier (1 - marketshare)
            p :+ o.city.spower,
            let flow :=  o.city.spower * s.customerPrice * pb.timeUnit in
                   (s.income :+ flow, o.city.expense :+ flow),                   // City -> Supplier B2C
            s.expense :+ o.city.spower * s.customerCost * pb.timeUnit,           // operation cost for CM
            // part of the demand that goes through the local opertor
            s.income :+ (o.buy * s.price * pb.timeUnit),
            s.wholesale :+ (o.buy * pb.timeUnit / 1000.0),
            s.expense :+ (o.sell + o.sellReserve) * s.buy * pb.timeUnit,
            p :+ (o.buy - o.sell - o.sellReserve)),
        s.expense :+ cn * s.nuclear.marginalCost * pb.timeUnit,
        //[DEBUG] [~A] total demand is ~A vs nuke:~A // date(pb.time),p,cn,
        if (p < 0.0) p := 0.0, // too much supply, no need for production  (there should be a max on sell)
        if SEESTAT (s.stat.dayNPower :+ min(cn,p),
                    s.peakPower :max p),
        s.stat.powerYM :add p,
        s.energy :+ (p * pb.timeUnit / 1000.0),                  // GWh
        s.nukeEnergy :+ (min(cn,p) * pb.timeUnit / 1000.0),      // total nuke energy
        s.nukeCost :+ (min(cn,p) * pb.timeUnit * (1 - pb.nuclearFixedCost) * s.nuclear.cost),
        s.expense :+ (min(cn,p) * pb.timeUnit * (1 - pb.nuclearFixedCost) * s.nuclear.cost),
        let co := min(pos(p - cn),fn) in                         // co : difference = fossil
           (s.stat.dayFPower :+ co,
            s.fossile.usageYM :add (co / fn),
            pb.regulator.energy :+ (p * pb.timeUnit / 1000.0),
            pb.regulator.co2Amount :+ (co * pb.timeUnit * pb.co2Factor),
            s.expense :+ p * s.transportCost * pb.timeUnit,
          ;  if show?() printf("[~A] getProduction(~S) = ~I (~I nuke ~I fossile)\n",pb.time,s,
          ;                    pF(p,2),pF(cn,2),pF(co,2)),
            if (p > cn)
              (//[DEBUG] --- need to buy ~A MW from oil & gas // co,
               let t := co * co2Tax(pb.regulator) * pb.timeUnit in               // CO2 tax  -> paid to government
                  (s.expense :+ t, pb.regulator.income :+ t),
               s.expense :+ co * CARNOT * pb.oilPrice * pb.timeUnit,             // marginal cost
               s.fossileEnergy :+ co * pb.timeUnit / 1000.0,
               s.fossileCost :+ co * CARNOT * pb.oilPrice * pb.timeUnit,
               if (p > (cn + co))  // need to by the remaining energy on the open market
                  (//[SHOW] [~A] ------- CRISIS: need to buy ~A on open market (~A GW)------- // date(pb.time), p - (cn + co),p / 1000.0,
                   s.crisis :+ (((p - (cn + co))  * pb.timeUnit) / 1000.0),
                   s.expense :+ ((p - (cn + co)) * OPENFACTOR * pb.oilPrice * pb.timeUnit))))) ]


// the capacity is setup once a day, trying to follow the pattern variation
[computeNukeCapacity(s:Supplier) : void
  -> let n := s.nuclear, nc := s.nukeCapacity,                   // 
         m1 := max(n.power * n.min%, nc * (1 - n.dailyMax%)),    // min possible production
         M1 := min(n.power, nc * (1 + n.dailyMax%)),             // max possible production
         d := day_(pb.time),
         ex := s.firstCapacity * get(pb.yearPattern,d) / get(pb.yearPattern,1) in   // expected nuke production should follow the pattern variation
       (ex :max m1,        // m1 is the min level
        ex :min M1,        // M1 is the max level
        //[SHOW] <~A> ... set nuke capacity to ~A% // pb.time, 100.0 * ex / nc,
        s.nukeCapacity := ex) ]

// investment strategy  -------------------------------------------------------------------------------

// in v0.0, two simple decisions:
//         invest in nuclear (according to the macro parameter sc.nuclearTrend)
//         invest in OilProduction (according to %use)
//
[runInvest(s:Supplier) : void
  -> let n := s.nuclear in
         (//[INVEST] ~S adds ~A of nuclear energy // s, n.power * pb.scenario.nuclearTrend,
          addInvest(n, n.power * pb.scenario.nuclearTrend)),     // could be negative
     let st := s.tactic, cf := s.fossile.power,
         oil% := mean(s.fossile.usageYM) in
         (if (oil% > st.usageRatio)
            (//[INVEST] [~A] ~S adds fossile capacity to lower usage ratio (~A) -> ~AMW // year_(pb.time), s, oil%,cf * (oil% - st.usageRatio),
             addInvest(s.fossile, cf * (oil% - st.usageRatio)))
          else trace(INVEST,"~S does not change fossile: oil% = ~A\n",s,oil%)) ]


// ********************************************************************
// *    Part 4: Government model                                      *
// ********************************************************************

// compute the daily oilPrice
[computeOilPrice(sc:Scenario)
  -> let t := pb.time, y := float!(year_(t)), theta := sc.oilPeriod in
       (pb.oilPrice := sc.oilPrice * ((1.0 + sc.oilTrend) ^ y) *
                                 (1.0 + sc.oilAmplitude * sinus(theta * t)) *
                                 (1.0 + sc.oilNoise * randomIn(-1.0,1.0)),
       // if show?() printf("<~A> ... set oil price to ~A$\n",t,pb.oilPrice),
        pb.oilPriceYM :add pb.oilPrice) ]


// co2Price
[co2Tax(r:Regulator) : Price
  -> let t := pb.time, y := float!(year_(t)), rt := r.tactic in
        (rt.co2FirstPrice * ((1.0 + rt.co2Trend) ^ y)) ]


// empty for first shot ... should raise CO2 price according to current results
// versus strategic objectives


// ********************************************************************
// *    Part 5: Grading (measuring satisfaction)                      *
// ********************************************************************

// the grade (called satisfaction in other GTES project) measures the result
// of the simulation from each actor's point of view, expressed as a fraction of
// the strategic objectives
// grading is done once a year -> a cummulative view is kept in
[gradePlayers()
  -> for c in City grade(c),
     for o in Operator grade(o),
     for s in Supplier grade(s),
     for r in Regulator grade(r) ]

GSHOW:integer :: 3
ADJUSTBALANCE:boolean :: false           // by setting to true, we force the balance to match the goal

// this is the customer's view point:
//   - minimize energy bill !  (use billprice = total expense / total demand)
//   - minimize worst case (worst price of the year * year-to-year price increase )
//   - comfort : sum of all shaving-induced pains :)
[grade(c:City) : void
   -> let s := c.strategy, w := 0.0, o := c.opertor, ms := o.marketShare, u := pb.supplier,
          oprice := mean(o.stat.priceYM), sprice := mean(u.stat.customerPriceYM),
          billprice := c.expense / ((c.energy + c.negaEnergy + c.shavedEnergy) * 1000.0),
          pr := ms * oprice + (1.0 - ms) * sprice,
          peak :=  ms * fear(o.peakPrice,oprice - o.prevPrice)  +
                   (1.0 - ms) * fear(u.peakPrice,sprice - u.prevPrice) in
          (w :+ gradeDown(billprice,s.price,0.5),
           w :+ gradeDown(peak,s.worstFear,0.2),
           w :+ gradeDown(mean(c.shaveYM),s.shave,0.2),
           c.satisfaction := (w / 3.0),
           //[GSHOW] --- grade(~S) = ~A  bill=~A fear=~A shave=~A // c,c.satisfaction,billprice,peak,mean(c.shaveYM),
           add(c.gradeGM,c.satisfaction),
           add(c.priceGM,pr),
           add(c.energyGM,c.energy),
           add(c.billGM,billprice),
           add(c.negaGM,c.negaEnergy),
           add(c.fearGM,peak),
           add(c.shaveGM,mean(c.shaveYM))) ]

// prints the KPI that helps understanding City's satisfaction
[ss(c:City) : void
   -> let s := c.strategy, w := 0.0, o := c.opertor, ms := o.marketShare, u := pb.supplier,
          oprice := mean(o.priceYM), sprice := mean(u.stat.customerPriceYM),
          billprice := c.expense / ((c.energy + c.negaEnergy + c.shavedEnergy) * 1000.0),
          pr := ms * oprice + (1.0 - ms) * sprice,
          peak :=  ms * fear(o.peakPrice,oprice - o.prevPrice)  +
                   (1.0 - ms) * fear(u.peakPrice,sprice - u.prevPrice),
          w1 := gradeDown(billprice,s.price,0.5),
          w2 := gradeDown(peak,s.worstFear,0.2),
          w3 := gradeDown(mean(c.shaveYM),s.shave,0.2) in
       printf("ss[~S]: ~A from (bill ~A=~A, fear ~A=~A, shave ~A=~A)\n",s,(w1 + w2 + w3) / 3.0,
              billprice,w1,peak,w2,mean(c.shaveYM),w3)  ]


// this is how the customer sees his worst fear: five year of projecting the current raise
[fear(x:Price,y:Percent) : Price 
   -> x + 5.0 * max(0.0,y)]

// this is the operator's view : marketshare,  sales,  balance (usual stuff)
[grade(o:Operator) : void
   -> let s := o.strategy, w := 0.0, bal := o.income - o.expense in
          (w :+ gradeEqual(bal,s.balance,0.2),          // v0.4 : we have tried 0.1 ... + gradeEqual
           w :+ gradeUp(o.income,s.income,0.2),
           w :+ gradeUp(o.marketShare,s.marketShare ,0.5),
           o.satisfaction := (w / 3.0),
           //[GSHOW] --- grade(~S) = ~A  ebitda=~AM$ income=~AM$ share=~A // o,o.satisfaction,bal / 1e6,o.income / 1e6,o.marketShare,
           add(o.gradeGM,o.satisfaction),
           add(o.stat.priceGM,mean(o.stat.priceYM)),
           add(o.stat.balanceGM,bal),
           add(o.stat.incomeGM,o.income),
           add(o.stat.fossile%GM,mean(o.fossile.usageYM)),
           add(o.stat.shareGM,o.marketShare)) ]

[ss(o:Operator) : void
   -> let s := o.strategy, w := 0.0, bal := o.income - o.expense,
          w1 := gradeUp(bal,s.balance,0.2),
          w2 := gradeUp(o.income,s.income,0.2),
          w3 := gradeUp(o.marketShare,s.marketShare ,1.0) in
        printf("ss[~S]:~A (bal~S:~S + inc~S:~S + shr~S:~S)\n",o,(w1 + w2 + w3) / 3.0,
               bal,w1,o.income,w2,o.marketShare,w3) ]


// Utility :  balance, robustness (distance between yearPeak and capacity),marketshare
[grade(u:Supplier) : void
    -> let s := u.strategy, w := 0.0, bal := u.income - u.expense,
           %margin := (u.nuclear.power + u.fossile.power - u.peakPower) / (u.nuclear.power + u.fossile.power),
           %share := 0.0 in
          (for o in Operator %share :+ (1.0 - o.marketShare),
           %share :/ float!(size(Operator)),
           w :+ gradeEqual(bal,s.balance,0.2),            // v0.2 : we do not want a balance that is too high
           w :+ gradeUp(1.0 + %margin,1.0 + s.margin,0.5),
           w :+ gradeUp(%share,s.share,0.5),
           u.satisfaction := (if ADJUSTBALANCE gradeEqual(bal,s.balance,0.2) else (w / 3.0)),
           //[GSHOW] --- grade(~S) = ~A  ebitda=~AM$ margin=~A share=~A // u,u.satisfaction,bal / 1e6,%margin,%share,
           add(u.gradeGM, u.satisfaction),
           add(u.stat.balanceGM,bal),
           add(u.stat.priceGM,mean(u.stat.priceYM)),
           add(u.stat.fossile%GM,mean(u.fossile.usageYM)),
           add(u.stat.marginGM,%margin),
           add(u.stat.shareGM,%share)) ]

[ss(u:Supplier) : void
    -> let s := u.strategy, bal := u.income - u.expense,
           %margin := (u.nuclear.power + u.fossile.power - u.peakPower) / (u.nuclear.power + u.fossile.power),
           %share := 0.0, w1 := 0.0, w2 := 0.0, w3 := 0.0 in
          (for o in Operator %share :+ (1.0 - o.marketShare),
           %share :/ float!(size(Operator)),
           w1 := gradeEqual(bal,s.balance,0.2),            // v0.2 : we do not want a balance that is too high
           w2 := gradeUp(1.0 + %margin,1.0 + s.margin,0.5),
           w3 := gradeUp(%share,s.share,0.5),
           printf("~S:~A  (bal~A:~S + margin~A:~S + share~A:~S)\n",u,(w1 + w2 + w3) / 3.0,
                  bal,w1,%margin,w2,%share,w3)) ]


// Regulator : keep economy running (production/output), balance budget and reduce CO2
[grade(g:Regulator) : void
    -> let s := g.strategy, w := 0.0, bal := g.income - g.expense in
          (w :+ gradeDown(g.co2Amount,s.co2Amount,0.4),
           w :+ gradeUp(bal,s.balance,1.0),
           w :+ gradeUp(g.energy,s.energy,0.2),
           g.satisfaction := (w / 3.0),
           // NEW (v0.2) : a politician must satisfy his voters :)
           g.satisfaction := 0.5 * g.satisfaction + 0.5 * CY[1].satisfaction,
           //[GSHOW] --- grade(~S) = ~A  co2=~A balance=~A energy=~ATWh// g,g.satisfaction,g.co2Amount,bal / 1e6,g.energy / 1e3,
           add(g.gradeGM,g.satisfaction),
           add(g.co2GM,g.co2Amount),
           add(g.taxGM,g.income),
           add(g.balanceGM,bal),
           add(g.energyGM,g.energy)) ]

// show satisfaction
[ss(g:Regulator) : void
    -> let s := g.strategy, bal := g.income - g.expense, cs := CY[1].satisfaction,
           w1 := gradeDown(g.co2Amount,s.co2Amount,0.4),
           w2 := gradeUp(bal,s.balance,1.0),
           w3 := gradeUp(g.energy,s.energy,0.2),
           w := ((w1 + w2 + w3) / 3.0) in
           printf("~S:~A  half(co2~A:~S + bal~A:~S + en~A:~S) + ~S:~A\n",g,(w * 0.5 + cs * 0.5),
                  g.co2Amount,w1,bal,w2,g.energy,w3,CY[1],cs) ]

// two simple utilities (truncated linear between 0 and 1, G is the goal, s is the slope
// v0.2 : do not truncate downwise, since equal worse perf => strange behavior
// v0.4 : introduce a constant K  .. but it must stay at 0, otherwise the system shifts as a whole
K:float :: 0.0             //
[gradeUp(x:float,G:float,s:Percent) : Percent
  -> if (x > G) 1.0  + ((x - G) * s * K / abs(G))
     else  1.0 - ((G - x) * s / abs(G)) ]    // G-x > 0 => worse than 1.0

[gradeDown(x:float,G:float,s:Percent) : Percent
  -> if (x < G) 1.0 + ((G - x) * s * K / abs(G))
     else max(0.0, 1.0 - ((x - G) * s / abs(G))) ]

// Up & Down : try to reach a target
[gradeEqual(x:float,G:float,s:Percent) : Percent
  -> if (x < G) max(0.0, 1.0 - ((G - x) * s / abs(G)))
     else max(0.0, 1.0 - ((x - G) * s / abs(G))) ]




