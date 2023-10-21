// ********************************************************************
// *       SGSS: Smart Grid Systemic Simulation                       *
// *       copyright (C) 2011 Yves Caseau                             *
// *       file: simul.cl                                             *
// ********************************************************************

// this file contains the overall simulation engine

// ********************************************************************
// *    Part 1: Daily simulation                                      *
// *    Part 2: Yearly simulation                                     *
// *    Part 3: Tactic local optimization                             *
// *    Part 4: Simulation & Results                                  *
// *    Part 5: Experiments & Init                                    *
// ********************************************************************

// ********************************************************************
// *    Part 1: Daily simulation                                      *
// ********************************************************************

// run a new day
// note: if pb.timeUnit > day, this may be many days  -> endDay produces (wrong) daily stats
[runDay(pb:Problem) : void
  -> if show?() //[2] [~A] --- start day ~A ---- // pb.time,date(pb.time),
     for c in City c.yearPatternDay := get(c.yearPattern,day_(pb.time)),
     pb.yearPatternDay := get(pb.yearPattern,day_(pb.time)),
     computeOilPrice(pb.scenario),             // compute the oil price
     computeNukeCapacity(pb.supplier),
     let d := day_(pb.time) in
        (while (day_(pb.time) = d) runTime(pb)),
     endDay(pb) ]

SHOWSP:boolean :: false            // debug : show supply price

// run a time unit - classical control variable (ITALK, ISHOW, ISHOP) based on pb.time
ITALK:integer :: 10000000
ISHOW:integer :: 10000000
ISTOP:integer :: 10000000
[runTime(pb:Problem) : void
  -> pb.time :+ integer!(pb.timeUnit),
     if (pb.time >= ITALK) (TALK := 0,STALK := 0),
     if (pb.time >= ISHOW | pb.time = ISTOP) (SHOW := 0, TALK := 0), // , ESHOW := 0),
     // compute price
     getPrice(pb.supplier),
     for o in Operator getPrice(o),
     // compute consommation
     for c in City getEnergy(c),
     // compute local and global production
     for o in Operator getProduction(o),
     getProduction(pb.supplier),
     if (SHOWSP & fossilePCost() < pb.supplier.price)    // DEBUG - remove
        trace(0,"[~A] cost =~A (~A+~A), supply= ~A => ~A MW - ~A GWh\n",
              pb.time,fossilePCost(),CARNOT * pb.oilPrice, co2Tax(pb.regulator),pb.supplier.price,OP[1].fossilePower,OP[1].fossileEnergy),
     if (pb.time >= ISTOP) error("look ! stopped @ ~A:~A",date(pb.time),pb.time) ]

// end of a Day show some stats
SHOWOP:integer :: 1            // show EDF + first operator by default
[endDay(pb:Problem) : void
  -> let s := pb.supplier, o := OP[SHOWOP], c := CY[SHOWOP] in
       (if talk?() printf("<~A> [oil=~I] ~S:(nuke=~I,oil=~I)@~I, ~S:(green=~I,oil=~I)@~I for ~IGW\n",
               day_(pb.time),pF(pb.oilPrice,2), s,
               pF(s.stat.dayNPower / 8000.0),pF(s.stat.dayFPower / 8000.0),pF(s.stat.dayPrice / 8.0),
               o, pF(o.stat.dayGPower / 8000.0),pF(o.stat.dayFPower / 8000.0),pF(o.stat.dayPrice / 8.0),
               pF(c.dayPower / 8000.0)),
        s.stat.dayNPower := 0.0, s.stat.dayFPower := 0.0, s.stat.dayPrice := 0.0,
        s.prevIncome := s.income,
        s.prevExpense := s.expense),
     for c in City c.dayPower := 0.0,
     for o in Operator
       (o.stat.dayGPower := 0.0, o.stat.dayFPower := 0.0, o.stat.dayPrice := 0.0,
        o.prevIncome := o.income,
        o.prevExpense := o.expense) ]



// ********************************************************************
// *    Part 2: Yearly simulation                                     *
// ********************************************************************

// randomize all yearPatterns  (note: this violates Affine encapsulation but is
// more efficient ...)
// the goal is to produce in one run
//   - a randomization for the current year (sc -> pb) - used as the main pattern for the whole territory
//   - a localized version that adds some local noise
//   - a coherent state where the global (national) pattern is the average of the local ones
//
[randomPatterns(pb:Problem)
  -> let yf := (1.0 + pb.scenario.energyTrend) ^ float!(year_(pb.time)),       // year factor
         a1 := pb.regulator.yearPattern, cf := pb.scenario.climateFactor,       // climate factor = year-to-year variation
         l1 := a1.xValues,  n := length(l1), m := 0.0,
         l2 := list<float>{ 0.0 | y in l1},                                            // will hold average (France)
         l3 := list<float>{ randomize(y,yf - cf, yf + cf) | y in a1.yValues} in         // shared pattern (climate)
    (//[DEBUG] --- compute the patterns for all cities from ~S // l3,
    for c in City
      (m :+ 1.0,
       for i in (1 .. n)
         let ccf := c.climateFactor, val := randomize(l3[i], 1.0 - ccf, 1.0 + ccf) in   // ccf : local variation
            (l2[i] :+ val,
             c.yearPattern.yValues[i] := val)),
     for i in (1 .. n) l2[i] :/ m,               // l2 is now the average for all cities
     // ~S ... average pattern  : ~S // (for c in City printf("~S:~S\n",c,c.yearPattern)), l3,
     pb.yearPattern.yValues := l2) ]


YSHOW:any :: unknown    // debug: show a special year
YINVEST:integer :: 5    // run in

// run a new year
[runYear(pb:Problem,talk?:boolean) : void
  -> randomPatterns(pb),
     startYear(pb),
     let y := year_(pb.time) in
       (while (year_(pb.time) = y) runDay(pb),       // main loop
       if (y < YINVEST)
           (for c in City runInvest(c),                 // yearly investment = f (year results)
            for o in Operator runInvest(o),
            runInvest(pb.supplier))),
     gradePlayers(false),
     if (talk? | year_(pb.time) = YSHOW)
        (//[0] ========================== YEAR ~A ================================= // year_(pb.time),
         seeYear(pb)),                              // show year
     endYear(pb) ]
    

// start the year by adding all fixed costs to energy opertors
//
[startYear(pb:Problem) : void
  -> for c in City
         (reset(c.shaveYM),reset(c.powerYM),
          c.expense := c.negaSum,
          c.energy := 0.0,
          c.negaEnergy := 0.0,                   // sum of savings because of negawatt investment
          c.shavedEnergy := 0.0),                 // unmet demand (shaved because of price)
     reset(pb.oilPriceYM),
     let g := pb.regulator in
        (g.energy := 0.0,
         g.expense := g.fixedCost,
         g.income := 0.0,
         g.co2Amount := 0.0),
     let s := pb.supplier in
        (reset(s.stat.priceYM), reset(s.stat.powerYM), 
         reset(s.stat.customerPriceYM), reset(s.fossile.usageYM),
         s.nukeCapacity :=  s.nuclear.power,         // 100% on January 1st
         s.nukeEnergy := 0.0,
         s.nukeCost := 0.0,
         s.fossileCost := 0.0,
         s.fossileEnergy := 0.0,
         s.peakPower := 0.0,
         s.peakPrice := 0.0,
         s.energy := 0.0,
         s.crisis := 0.0,
         s.income := 0.0,
         s.wholesale := 0.0,
         s.power := 0.0,
         s.expense := s.nuclear.fixedCost,
         s.expense :+ s.fossile.fixedCost),
     for o in Operator
       (reset(o.stat.priceYM), reset(o.stat.powerYM), reset(o.stat.forecastYM),
        reset(o.green.usageYM), reset(o.green.rateYM), reset(o.fossile.usageYM),reset(o.fossile.rateYM),
        o.storage.yearProduction := 0.0,
        o.output := 0.0,
        o.peakPrice := 0.0,
        o.wholesale := 0.0,
        o.energy := 0.0,
        o.greenEnergy := 0.0,
        o.fossileEnergy := 0.0,
        o.green.flowOut := 0.0,
        o.green.income := 0.0,
        o.income := 0.0,
        o.edfExpense := 0.0,
        o.cusExpense := 0.0,
        o.fuelExpense := 0.0,
        o.sellIncome := 0.0,
        // store instrumentation
        o.stat.localIn := 0.0,
        o.stat.localOut := 0.0,
        o.stat.supplyIn := 0.0,
        o.stat.supplyOut := 0.0,
        o.storage.flowIn := 0.0,
        o.storage.flowOut := 0.0,
        o.storage.flowR := 0.0,
        o.storage.expense := 0.0,
        o.storage.incomeB := 0.0,
        o.storage.incomeR := 0.0,
        o.storage.prodCost := o.storage.fixedCost,
        // fixed costs
        o.expense := o.green.fixedCost,
        o.expense :+ o.storage.fixedCost,
        o.expense :+ o.fossile.fixedCost * pb.localFixedCost) ]   // v0.4 : cheaper when local


// endYear : called at the end of a one year simulation
// reset all yearly measures and yearly stats - move key PKI into GM (Global measures)
[endYear(pb:Problem) : void
  -> let s := pb.supplier in s.prevPrice := mean(s.stat.customerPriceYM),
     for c in City endYear(c),                              // compute marketshares
     for o in Operator o.prevPrice := mean(o.stat.priceYM) ]



// ********************************************************************
// *    Part 3: Tactic local optimization                             *
// ********************************************************************

// ---------------------- generic optimization engine [float flavor] -------------------------
// [this is a reusable code fragment - source: project PSR Game - 2007 ======================]

OPTI:integer :: 1                  // TRACE/DEBUG verbosity
NUM1:integer :: 5                  // number of steps in a loop (1/2, 1/4, ... 1/2^5) => precision
MULTI:integer :: 5                 // number of successive optimization loops

MaxValue[p:property] : float := 1.0            // default value is for percentage => 1.0 is max

// define maxValues for prices

// optimize all players
[optimize() : void
  -> for i in (1 .. MULTI) for c in Player optimize(c) ]

// optimise the tactic a company
[optimize(x:Player) : void
  -> // runLoop(x),
     for p in x.tacticProperties (optimize2(x,p),optimize(x,p)),
     trace(TALK,"--- end optimize(~S) -> ~A \n",x,x.satisfaction) ]

// first approach : relative steps (=> does not cross the 0 boundary, keeps the sign) ----------

// optimize a given slot in a set of two dichotomic steps
[optimize(c:Player,p:property)
  -> for i in (1 .. NUM1) optimize(c,p,float!(2 ^ (i - 1))),
     trace(OPTI,"best ~S for ~S is ~A => ~A\n", p,c,read(p,c.tactic), c.satisfaction) ]

DD:integer := 0   // debug counter
DGO:integer := 0
WHY:boolean :: false   // debug

// the seed value is problem dependant !
// it is used twice - when the value is 0, to boost the multiplicative increment loop (opt)
//                    when the value is very small, to boost the additive loop
SEED:float :: 1.0

[optimize(c:Player,p:property,r:float)
   ->  let vr := c.satisfaction, val := 0.0,
           vp := read(p,c.tactic), v0 := (if (vp > 0.0) vp else SEED),        // v0.4 do not waste cycles
           v1 := vp / (1.0 +  (1.0 / r)), v2 := vp * (1.0 + (1.0 / r)) in
        (write(p,c.tactic,v1),
         if (v1 >= 0.0) val := runLoop(c),
         DD :+ 1,
         //[OPTI] try ~A (vs.~A) for ~S(~S) -> ~A (vs. ~A) [DD:~A] // v1,vp,p,c,val,vr,DD,
         if (DD = DGO) (TALK := 0, SHOW := 0),
         if (val > vr) (vp := v1, vr := val),
         write(p,c.tactic,v2),
         if (v2 <= MaxValue[p]) val := runLoop(c) else trace(OPTI,"MAX-NO"),
         //[OPTI] try ~A for ~S(~S) -> ~A // v2,p,c,val,
         if WHY seeEnd(OP[1]),
         if (val > vr) (vp := v2, vr := val),
         write(p,c.tactic,vp),
         c.satisfaction := vr) ]

// absolute variant --------------------------------------------------------------------

// optimize a given slot in a set of two dichotomic steps
[optimize2(c:Player,p:property)
  -> for i in (1 .. NUM1) optimize2(c,p,float!(2 ^ (i - 1))),
     trace(OPTI,"best ~S for ~S is ~A => ~A\n",p,c,read(p,c.tactic), c.satisfaction) ]



[optimize2(c:Player,p:property,r:float)
   -> let vp := read(p,c.tactic), vr := c.satisfaction, val := 0.0,
          seed := max(vp, SEED),
          v1 := vp +  (seed / r), v2 := vp - (seed / r) in
        (write(p,c.tactic,v1),
         if (v1 <= MaxValue[p]) val := runLoop(c) else trace(OPTI,"MAX-NO"),
         //[OPTI] Try ~A (vs.~A) for ~S(~S) -> ~A (vs. ~A)// v1,vp,p,c,val,vr,
         if WHY seeEnd(OP[1]),
         if (val > vr) (vp := v1, vr := val),
         write(p,c.tactic,v2),
         if (v2 >= 0.0) val := runLoop(c),
         //[OPTI] Try ~A for ~S(~S) -> ~A // v2,p,c,val,
         if (val > vr) (vp := v2, vr := val),
         write(p,c.tactic,vp),
         c.satisfaction := vr) ]

// debug: test a local move
[whatif(c:Player,p:property,y:float) : void
  -> let val := runLoop(c), x := read(p,c.tactic), v2 := 0.0 in
       (printf("with ~S(~S) = ~A  => ~A \n",p,c,x,val),
        write(p,c.tactic,y),
        v2 := runLoop(c),
        printf("with ~S(~S) = ~A  => ~A \n",p,c,y,v2),
        write(p,c.tactic,x)) ]

// show tactic
[tac(x:Player) : void
  ->  printf("--- Tactic summary for ~S [~I] (d-from-prev ~I) ------------------------------\n",x,
             p%(x.satisfaction), p%(distance(x,x.tactic,x.nashTactic))),
      for p in x.tacticProperties printf("\t~S:~I",p,pF(get(p,x.tactic),3)),
      princ("\n")]

// copy the tactical slots - from tactic x to tactic y !
[copyTo(c:Player,x:object,y:object) : void
  -> for p in c.tacticProperties write(p,y,read(p,x)) ]

// nashStore : keep a copy of previous tactic
[nashStore(c:Player) : void
  -> c.nashTactic := copy(c.tactic),
     copyTo(c,c.tactic,c.nashTactic) ]

[nashStore() : void 
   -> for p in Player nashStore(p)]


// generic distance - evaluate the distance between a tactic and another one
[distance(c:Player,x:object,y:object) : float
  -> let d := 0.0, n := 0.0 in
       (for p in c.tacticProperties
          let v1 := read(p,x), v2 := read(p,y) in
             (n :+ 1.0,
              d :+ (abs(v1 - v2) / (abs(v1) + abs(v2) + 1e-10))),
        d / n) ]


// --------------- two-opt ------------

OPTI2:integer :: 1

// randomized 2-opt, borrowed from SOCC, but smarter:once the first random move is made, try to fix it with optimize
// tries more complex moves which are sometimes necessary
[twoOpt(c:Player,n:integer)
  -> // optimize(c),                      // first run a single pass
     let vr := c.satisfaction, x := c.tactic, val := 0.0 in
        (for i in (1 .. n)
         let p1 := (randomIn(c.tacticProperties) as property),
             p2 := (randomIn(c.tacticProperties) as property),
             v1 := read(p1,c.tactic), v2 := read(p2,c.tactic) in
           (if (p1 = p2) nil
            else
             (moveP2(c,p1,v1),
              trace(OPTI,"=== shift: ~S(~S) = ~A vs ~A\n",p1,c,get(p1,c.tactic),v1),
              if (get(p1,c.tactic) != v1) optimize(c,p2),
              val := c.satisfaction,
              trace(OPTI2,"=== try2opt [~A vs ~A] with ~S(~A<-~A) x ~S(~A<-~A)\n",
                 val,vr,p1,get(p1,x),v1,p2,get(p2,x),v2),
           if (val <= vr) (c.satisfaction := vr, write(p1,c.tactic,v1), write(p2,c.tactic,v2))
           else (vr := val,
                 trace(OPTI2,"*** improve ~A with ~S:~A x ~S:~A -> ~A\n",
                      val,p1,get(p1,c.tactic),p2,get(p2,c.tactic), val))))),
      runLoop(c),
      trace(OPTI2,"--- end 2opt(~S,~A) -> ~A% \n",c,n,c.satisfaction * 100.0) ]


// random move : vp is the current value
[moveP2(c:Player,p:property, vp:float) : void
  -> let d := (if randomChoice?(0.5) -1 else 1),
         y := vp + d * (max(vp,1.0) / (2.0  ^ float!(2 + random(NUM1)))) in
       (if (y >= 0.0 & y <= MaxValue[p]) write(p,c.tactic,y)) ]

// special version that looks at price (two first slots)
[twoOptP(c:Player,n:integer)
  -> // optimize(c),                      // first run a single pass
     let vr := c.satisfaction, x := c.tactic, val := 0.0 in
        (for i in (1 .. n)
         let p1 := c.tacticProperties[1],
             p2 := c.tacticProperties[2],
             v1 := read(p1,c.tactic), v2 := read(p2,c.tactic) in
           (moveP2(c,p1,v1),
            trace(OPTI,"=== shift: ~S(~S) = ~A vs ~A\n",p1,c,get(p1,c.tactic),v1),
            if (get(p1,c.tactic) != v1) val := inOptimize(c,p2),    // check that move2 was not refused :)
            trace(OPTI2,"=== try2optPrice [~A vs ~A] with ~S(~A<-~A) x ~S(~A<-~A)\n",
                  val,vr,p1,get(p1,x),v1,p2,get(p2,x),v2),
            if (val <= vr) (c.satisfaction := vr, write(p1,c.tactic,v1), write(p2,c.tactic,v2))
            else (vr := val,
                  trace(OPTI2,"*** improve ~A with ~S:~A x ~S:~A -> ~A\n",
                      val,p1,get(p1,c.tactic),p2,get(p2,c.tactic), val)))),
      runLoop(c),
      trace(OPTI2,"--- end 2optPrice(~S,~A) -> ~A% \n",c,n,c.satisfaction * 100.0) ]

// v0.4 - inner call to optimize need to forget previous satisfaction value
[inOptimize(c:Player,p:property) : float
  -> c.satisfaction := 0.0,
     optimize(c,p),
     c.satisfaction ]

// v0.4 - smarter version with a dual nested optimize
OPTI3:integer :: 1
[optimize2p(c:Player)
  -> let p1 := c.tacticProperties[1],
         p2 := c.tacticProperties[2] in
      for i in (1 .. NUM1) optimize2p(c,p1,p2,float!(2 ^ (i - 1))) ]

[optimize2p(c:Player,p1:property,p2:property,r:float) : float
   -> let vp1 := read(p1,c.tactic), vp2 := read(p2,c.tactic), vr := c.satisfaction, val := 0.0,
          v1 := vp1 / (1.0 +  (1.0 / r)), v2 := vp1 * (1.0 + (1.0 / r)) in
        (write(p1,c.tactic,v1),
         if (v1 >= 0.0) val := inOptimize(c,p2),
         if (val > vr) (vp1 := v1,  vp2 := read(p2,c.tactic), vr := val),
         //[OPTI3] try ~S(~S) = ~A -> ~A // p1,c,v1,val,
         write(p1,c.tactic,v2),
         write(p2,c.tactic,vp2),
         if (v2 <= MaxValue[p1]) val := inOptimize(c,p2),
         //[OPTI3] try ~S(~S) = ~A -> ~A // p1,c,v2,val,
         if (val > vr) (vp1 := v2, vp2 := read(p2, c.tactic), vr := val),
         write(p1,c.tactic,vp1),
         write(p2,c.tactic,vp2),
         c.satisfaction := vr, vr) ]


// ---------------------- new in v0.3 : Random Walk -----------------------------------

RWALK:integer :: 1

// this a tabu-style randoom walk (but since each move is very expensive, it is a little more
// directed:
//   - we keep the best known solutions
//   - if the distance to the best known solution is more than delta%, we return
//   - if closer? = true, we reinforce moves that produced an improvement
//   - we keep a short tabu list of size "tabu"
// this loop is run at most n times
[rWalk(c:Player,n:integer,delta:Percent,closer?:boolean,tabu:integer) : void
  ->  let bestv := c.satisfaction, val := 0.0, pval := bestv,              // pval : previous value
          nTabu := min(tabu, length(c.tacticProperties) - 1),              // there should alwys be one prop
          bt := c.walkTactic,               // bt is the best-found-so-far tactic
          ppref := owner,                   // default value to indicate unknown
          lTabu := list<property>{ppref | i in (1 .. nTabu)},
          iTabu := 1 in                     // circulating index for tabu list
        (copyTo(c,c.tactic,bt),
         for i in (1 .. n)
           let p1 := (if (ppref = owner) (randomBut(c.tacticProperties,lTabu) as property) else ppref),
               v1 := read(p1,c.tactic) in
             (moveP2(c,p1,v1),
              runLoop(c),
              val := c.satisfaction,
              trace(RWALK,"=== rWalk[~A]: ~S(~S) = ~A vs ~A -> ~A [~S]\n",i,p1,c,get(p1,c.tactic),v1,val,ppref),
              if (val <= pval) (// decreasing move
                               iTabu := addTabu(lTabu,iTabu,p1),                        // tabu
                               if closer? ppref := owner,                               // stop using ppref
                               if (val < bestv * (1.0 - delta)) (//[RWALK] reset at ~A // val,
                                                                 val := bestv,
                                                                 copyTo(c,bt,c.tactic)))  // avoid lost paths
              else (// increasing move
                    if (val > bestv) (copyTo(c,c.tactic,bt), bestv := val),
                    if closer? ppref := p1),                                         // closer => keep on this p
              pval := val),
         // use the best found tactic !
         copyTo(c,bt,c.tactic),
         runLoop(c),
         trace(RWALK,"--- end randomWalk(~S,~A) -> ~A% \n",c,n,c.satisfaction * 100.0)) ]

// add a property in the tabu list
[addTabu(lTabu:list<property>,iTabu:integer,p:property) : integer
  -> lTabu[iTabu] := p,
     if (iTabu < length(lTabu)) (iTabu + 1) else 1 ]

// initializes this slot
[walkStore()
  -> for c in Player c.walkTactic := copy(c.tactic) ]

// ---------------------- dedicated optim for each player -----------------------------

// optStep(p) makes a selection of optimization moves
// tuned for each player because we do not have the time (opt/opt2 + twoOpt would work as a generic)

// first step : optimize each parameter
// next steps: optimize the financial parameters (
OPTUPTO:integer :: 5
OPTD:integer :: 0
OPTCT:integer :: 0
[optStep(o:Operator, full?:boolean, first?:boolean) : void
  -> // gc(),              // v0.4b -> force GC !
     let lp := o.tacticProperties, n := length(lp) in  // v0.4 forget this horror ! (if first? length(lp) else OPTUPTO) in
        for i in (1 .. n)
          (if (i = 10 & o.index = OPTD) (OPTCT :+ 1, OPTI := 0, trace(0,"{~A}\n",OPTCT))
           else OPTI := 2,
           if full? (if first? optimize2(o,lp[i]), optimize(o,lp[i]))             // first & full => REGULAR
           else (optimizeShort(o,lp[i]),
                 if (i = 10 & o.index = OPTD & read(lp[i],o.tactic) >= 2.0) error("look closely why green works"))),
     OPTI := 2]

// even simpler version that only does marginal tuning
[optimizeShort(c:Player,p:property)
  -> for i in (1 .. (NUM1 - 1)) optimize2(c,p,float!(2 ^ (i - 1))),
     trace(OPTI,"Short - best ~S for ~S is ~A => ~A\n",p,c,read(p,c.tactic), c.satisfaction) ]

OTHER:boolean :: true       // debug switch  .. must be true => optimize all ops
TWOMODE:integer :: 3        // use twoOpt2P
NASTY:boolean :: false      // a NASTY optimization that causes more harm than good ?

// optimize all operators
[optOperators(first?:boolean) : void
  -> let o := OP[1] in        // use first OP as a beacon
        (time_set(),
         optStep(o,first?,first?),
         if (TWOMODE = 1) twoOptP(o,(if first? 20 else 10))
         else if (TWOMODE = 2) optimize2p(o)
         else (if first? optimize2p(o) else twoOptP(o,10)),
         trace(0,"-- first operator -> ~A in ~As: ~A\n",o.satisfaction,(time_read() / 1000),profile(o)),
         if NASTY for i in (2 .. size(Operator))
            for p in o.tacticProperties
               (if (i = OPTD & p = last(o.tacticProperties)) trace(0,"--- ~A copied from ~S",read(p,o.tactic),o),
                write(p,OP[i].tactic,read(p,o.tactic))),
         if OTHER for i in (2 .. size(Operator)) optStep(OP[i],false,first?),
         trace(0,"optimize operators -> ~A in ~As\n",
                 avg(list{o.satisfaction | o in Operator}),(time_get() / 1000))) ]

// optimize for EDF
[optStep(x:Supplier,first?:boolean)
 -> time_set(),
    // gc(),              // v0.4b -> force GC ! claire3 pragma no longer needed
    for p in x.tacticProperties (if first? optimize2(x,p), optimize(x,p)),
    if (TWOMODE = 1) twoOptP(x,(if first? 20 else 10))
    else if (TWOMODE = 2) optimize2p(x)
    else (if first? optimize2p(x) else twoOptP(x,10)),
    trace(0,"optimize(~S) -> ~A in ~As: ~A\n", x,x.satisfaction,(time_get() / 1000),profile(x)) ]

// profiles   price, marketshare, balance
[profile(o:Operator) : list<float>
  -> list<float>(mean(o.stat.priceGM),mean(o.stat.shareGM),mean(o.balanceGM)) ]
[profile(o:Supplier) : list<float>
  -> list<float>(mean(o.stat.priceGM),mean(o.stat.shareGM),mean(o.balanceGM)) ]
[profileEM(o:Operator) : list<float>
  -> list<float>(mean(o.stat.priceEM),mean(o.stat.shareEM),mean(o.balanceEM)) ]
[profileEM(o:Supplier) : list<float>
  -> list<float>(mean(o.stat.priceEM),mean(o.stat.shareEM),mean(o.balanceEM)) ]

// generic optimize
[optStep(x:Regulator) : void 
   -> optimize(x)]

[optStep(c:City) : void 
   -> optimize(c)]

OPTEDF:boolean :: true             // debug: turn off if OPT EDF is poor

// overall optimization step
[optStep(first?:boolean,store?:boolean)
  -> optStep(pb.supplier,first?),
     if store? storeStat(),
     optOperators(first?),
     for c in City optStep(c),
     if store? storeStat(),
     if store? trace(0,"avg profile => EDF ~A - OP[1] ~A\n",profileEM(pb.supplier), profileEM(OP[1])),
     if OPTEDF optStep(pb.regulator)]
    

// search for Nash equilibrium
[nash(n:integer) : void
  -> //[0] start Nashloop -------------- //,
     optStep(true,false),
     nashShow(1,n),
     for i in (2 .. n)
        (//[0] step ~A (~A%) ----------------// i,pb.nashDist,
         printf("step ~A -------------------------------------------------------------\n",i),
         optStep(i mod 3 = 2,2 * i > n),           // full cycle every 3 step => better investment
         nashShow(i,n)),
     printf("end of nash(~A) ---------------------------------------------------\n",n),
     seeEnd(pb),
     printf("--- resulting tactics ---------------------------------------------\n"),
     tac(pb.regulator),
     tac(pb.supplier),
     tac(OP[1]),
     tac(OP[2])]

// default value ------ 6  seems a good number ---------------------------------------
NASHLOOP:integer :: 6
[nash() 
   -> nash(NASHLOOP)]

[nashShow(i:integer,N:integer) : void
   -> let d := 0.0, n := 0.0 in
        (tac(pb.supplier), tac(OP[1]), seeEnd(pb.regulator), seeEnd(pb.supplier), seeEnd(OP[1]),
         for x in Player (n :+ 1.0, d :+ distance(x,x.tactic,x.nashTactic)),
         d :/ n,
         // need to store in a measure :)
         printf("[~A] --- nash distance index = ~I%\n",i,p%(d)),
         pb.nashDist := d,
         if (i >= N - 1) add(pb.nashDistGM,d),
         nashStore()) ]

// ********************************************************************
// *    Part 4: Simulation & Results                                  *
// ********************************************************************

RSEED:integer :: 0

// useful fragment: stop and resume the randomization
// randomStop() ensures that we always start with the same seed
randomStop() : void
 -> (RSEED := random(100000),
     //[5] RSEED = ~A // RSEED,
     random!(0))

randomResume() : void
  -> (random!(RSEED))

// main simulation loop
[run(pb:Problem,b:boolean) : void
  -> if b //[0] === start simulation (~A years; ~A Cities) ! ===================== // NIT,size(City),
     time_set(),
     randomStop(),                          // should be E.seed :)
     pb.time := 0,
     for y in (1 .. NIT) runYear(pb,b),
     randomResume(),
     let t := time_get() in
      (if b //[0] === end simulation [~A ms] ============================================ // t
      ),
     seeEnd(pb)]

// this methods calls the simulation loop
[runLoop(a:Player) : float
  -> runLoop(false),
     a.satisfaction ]

[runLoop(b:boolean) : void
  -> reinit(pb),
     randomStop(),
     pb.time := 0,
     for y in (1 .. NIT) runYear(pb,b),
     for a in Player  a.satisfaction := mean(a.gradeGM),
     randomResume() ]

// reinit : resets all global stats
// also, undo investments
[reinit(pb:Problem)
  -> pb.oilPrice := pb.scenario.oilPrice,
     for x in Player reset(x.gradeGM),
     for c in City              (reset(c.priceGM), reset(c.fearGM), reset(c.shaveGM), reset(c.negaGM),
                                 reset(c.billGM), c.negaSum := 0.0, c.negaRatio := 0.0),
     for o in Operator          (o.marketShare := pb.scenario.marketShare,
                                 o.prevPrice := pb.referencePrice,
                                 reinit(o.fossile),
                                 reinit(o.green),
                                 reinit(o.storage),
                                 reset(o.stat.fossile%GM),
                                 reset(o.stat.balanceGM), 
                                 reset(o.stat.incomeGM), 
                                 reset(o.stat.shareGM), 
                                 reset(o.stat.priceGM)),
     let g := pb.regulator in   (g.fixedCost := 0.0,
                                 reset(g.co2GM), reset(g.balanceGM), reset(g.energyGM), reset(g.taxGM)),
     let s := pb.supplier in    (s.prevPrice := pb.referencePrice,
                                 reset(s.stat.priceGM),
                                 reset(s.stat.balanceGM), 
                                 reset(s.stat.marginGM), 
                                 reset(s.stat.shareGM),
                                 reset(s.stat.fossile%GM),
                                 reinit(s.nuclear), 
                                 reinit(s.fossile)) ]

// reinit an energy unit
[reinit(x:EnergyCapacity) : void
  -> x.power :- x.invest,
     x.invest := 0.0,
     case x (Storage (x.buffer := 0.0, x.reserve := 0.0)),
     init(x,x.power) ]


// ********************************************************************
// *    Part 5: Experiments & Init                                    *
// ********************************************************************

LOG2FILE?:boolean :: true

// run an experiment N times (simple form with no optimization yet) and stores the results
[run(e:Experiment)
   -> let p := openFile(e), p0 := cout(), s := e.scenario in
       (time_set(),
        init(e),
        if LOG2FILE? use_as_output(p),
        for i in (1 .. e.nSample)   // number of monte-carlo simulation
           (//[0] === start Test case ~A at ~As ========================== // i, time_read() / 1000,
            printf("=== start Test case ~A at ~As ===========================\n",i,time_read() / 1000),
            run(pb,false),
            nash(e.nashLoop),
            //[0] === end Test case ~A at ~As ============================= // i, time_read() / 1000,
            printf("=== end Test case ~A at ~As =============================\n", i, time_read() / 1000),
            if (i != e.nSample) reinit(e)),
       printf("=================== end of experiment ~S/~S [~A] ================\n", e,s,s.tag),
       display(e),
       fclose(p)) ]

// log a summary
[openFile(e:Experiment) : port
  -> let s := e.scenario,
         fn := Id(*where*) /+ "\\data\\" /+ string!(name(e)) /+ "-" /+ string!(name(s)) /+ ".log",
         p := fopen(fn ,"w") in
       (use_as_output(p),
        printf("------------------  [v~A] Experiment ~S x ~S on ~A",Version,e,s,date!(1)),
        p) ]


// GTES randomization
[randomize(e:Experiment) : void
  -> let s := e.scenario in
       (s.energyVariation := randomIn(e.demandMin,e.demandMax),
        s.cityVariation := randomIn(e.cityMin,e.cityMax),
        s.oilPrice := randomIn(e.oilMin,e.oilMax),
        s.negaEfficiency := randomIn(e.negaMin,e.negaMax),
        for h in Household h.shavingSensitivity := randomIn(e.shaveMin,e.shaveMax),
        s.marketSensitivity := randomIn(e.marketMin,e.marketMax)) ]

// print randomize values (tracability, to reproduce a "special case" if needed)
[printGTES(e:Experiment) : void
   -> let s := e.scenario in
       (printf("GTES[~S] -> energy(var) = ~S, city = ~S, oilPrice = ~S\n",e,
                s.energyVariation,s.cityVariation,s.oilPrice),
        printf("GTES[~S] -> negaEfficiency = ~S, shaving = ~S, market = ~A\n",e,
                s.negaEfficiency,(Household.instances[1]).shavingSensitivity,s.marketSensitivity)) ]

// store stat : put GM measures in EM measures
// note: these are the measures from the last run !
[storeStat() : void
  -> for x in Player            add(x.gradeEM,mean(x.gradeGM)),
     for c in City              (add(c.priceEM,mean(c.priceGM)), add(c.fearEM,mean(c.fearGM)),
                                 add(c.shaveEM,mean(c.shaveGM)), add(c.negaEM,mean(c.negaGM)),
                                 add(c.negaSumEM,c.negaSum), add(c.negaRatioEM,c.negaSum),
                                 add(c.billEM,mean(c.billGM))),
     for o in Operator          (add(o.stat.fossile%EM,mean(o.stat.fossile%GM)),
                                 add(o.stat.balanceEM,mean(o.stat.balanceGM)), 
                                 add(o.stat.incomeEM,mean(o.incomeGM)),
                                 add(o.stat.shareEM,mean(o.stat.shareGM)), 
                                 add(o.stat.priceEM,mean(o.stat.priceGM)),
                                 add(o.stat.fossileEM,o.fossile.invest), 
                                 add(o.stat.greenEM,o.green.invest),
                                 add(o.stat.storageEM,o.storage.invest)),
     let g := pb.regulator in   (add(g.co2EM,mean(g.co2GM)),  
                                 add(g.balanceEM,mean(g.balanceGM)),
                                 add(g.energyEM,mean(g.energyGM)), 
                                 add(g.taxEM,mean(g.taxGM))),
     let s := pb.supplier in    (add(s.stat.priceEM,mean(s.stat.priceGM)),
                                 add(s.stat.balanceEM,mean(s.stat.balanceGM)), 
                                 add(s.stat.marginEM,mean(s.stat.marginGM)),
                                 add(s.stat.shareEM,mean(s.stat.shareGM)),
                                 add(s.stat.fossile%EM,mean(s.stat.fossile%GM)),
                                 add(s.stat.nuclearEM,s.nuclear.invest), 
                                 add(s.stat.fossileEM, s.fossile.invest))  ]



// ------------------------- our reusable trick -------------------------

// test1: simplest model
// [foo() -> sload(Id(*src* / "sgssv" /+ string!(Version) / "test1")) ]

// we load a file of interpreter code
(#if (compiler.active? = false | compiler.loading? = true)
     (load(Id(*src* / "sgssv" /+ string!(Version) / "test1")))
  else nil
)



