$oneolcom

* Setting
$setglobal  Solver                           Cplex ;

Scalar LPtimeLimit                       / 3600 / ;
Scalar LPiterationLimit                  / 2000000000 / ;

* Setting End


* Set the solver for the LP and MIP
option lp = %Solver% ;
option mip = %Solver% ;


*$call gdxxrw Input.xlsx o=sm.gdx index=myindex!a1

* Set the solution print status in the lst file
option solprint = off;

* Set the column (variable) and row (equation) listing in the lst file
option limcol = 0 ;
option limrow = 0 ;



* Update the runlog file
File runlog "Write to a report"  / "ProgressReport.txt" /;
runlog.lw = 0 ; runlog.ap = 1 ;
putclose runlog / 'SM run started at: ' system.date " " system.time /;





*Primay sets
SETS
   vs       'Stored energy value segment' /vs1 * vs50/
   ls       'Loss load tranches' /ls1 * ls10/
   i        'Pre-set number of iterations' /i1 * i1000/

   revt(t,t)                                                                    'reserve set of set t'
   reservoirplant(j,g)                                                          'list of hydro plants behind a reservoir or lake that can store water'
   reservoirjunction(j,j)                                                       'list of hydro junctions behind a reservoir or lake that can store water'
   unmapped(j)                                                                  ' '
;
Alias (n, fn, tn),   (j, fj, tj),   (g, g1, g2)
;

PARAMETERS
* Input data
   reservoirparameters(j,*)
   hydroarcparameters(fj,tj,*)
   hydrostationparameters(g,n,fj,tj,*)
   thermalstationparameters(g,n,f,*)
   fixedstationparameters(g,n,t,b)
   transmissionparameters(fn,tn,*)
   valueoflossload(n,ls,*)
   endwatervalue(vs,*)


* Output
   o_systemcost(s,t)                                                            'Saved system cost at stage t given inflow sequence s'
   o_endstorage(s,t,j)                                                          'Pre-calculated storage level at the start of time interval(t) at reservoir(j)'

   o_generation(s,t,b,g)                                                        'Saved cleared generation'
   o_shortage(s,t,b,n)                                                          'Total MW Shortage'
   o_spillwayviolation(s,t,b,g)                                                 'violation of spillway limit - cumec(s) '
   o_maxarcflowviolation(s,t,b,fj,tj)                                           'violation of max flow on hydro arc - cumec(s) '
   o_minarcflowviolation(s,t,b,fj,tj)                                           'violation of min flow on hydro arc - cumec(s) '

   o_generationcost(s,t,b)                                                      'Generation cost output for reporting'
   o_shortagecost(s,t,b)                                                        'Shortage cost output for reporting'
   o_spillwayviolationcost(s,t,b)                                               'Cost of spillway violation'
   o_maxarcflowviolationcost(s,t,b)                                             'Cost of max flow violation on hydro arc'
   o_minarcflowviolationcost(s,t,b)                                             'Cost of min flow violation on hydro arc'

   o_futurecost(s,t)                                                            'expected minimum furture cost and the and of time(t)'
   o_endstoragevalue(s,t)                                                       'value of end total storage $'


   o_slope(j)                                                                   ' '
   o_intercept                                                                  ' '

* Convergence test criterion
   upperbound                                                                   'Upper bound of system cost'
   lowerbound                                                                   'Lower bound of system cost'
   standdev                                                                     'Standard deviation of system cost'
;

SCALARS
   modelSolved  'Flag to indicate if the model solved successfully (1 = Yes)'   / 0 /
   iter         'iteration count'                                               / 0 /

;

$GDXIN "sm.gdx"
$LOAD n, t, b, j, s, g, f
$LOAD demand, blockhour, inflow
$LOAD fuelcost, co2cost, emissionrate
$LOAD reservoirparameters
$LOAD hydroarcparameters
$LOAD hydrostationparameters
$LOAD thermalstationparameters
$LOAD fixedstationparameters
$LOAD transmissionparameters
$LOAD valueoflossload
$LOAD endwatervalue
$GDXIN




* Calculating sets
revt(t,t+[card(t)-2*ord(t)+1]) = yes;

sea(j) = yes $ sameas(j,'SEA');
reservoir(j) = yes $ reservoirparameters(j,'capacity') ;
hydroarc(fj,tj) = yes $ hydroarcparameters(fj,tj,'maxflow') ;

plantnode(g,n) =
   yes $ {sum[ (fj,tj), hydrostationparameters(g,n,fj,tj,'powerfactor') ]
       or sum[ f, thermalstationparameters(g,n,f,'heatrate') ]
       or sum[ (t,b), fixedstationparameters(g,n,t,b) ] } ;

fixed(g)    =
   yes $ sum[ (n,t,b), fixedstationparameters(g,n,t,b) ] ;
thermal(g)  =
   yes $ sum[ (n,f), thermalstationparameters(g,n,f,'heatrate') ] ;
hydro(g)    =
   yes $ sum[ (n,fj,tj), hydrostationparameters(g,n,fj,tj,'powerfactor')] ;

plantfuel(g,f) =
   yes $ sum[ n, thermalstationparameters(g,n,f,'heatrate') ] ;
hydrodef(g,fj,tj) =
   yes $ sum[ n, hydrostationparameters(g,n,fj,tj,'powerfactor')] ;

reservoirjunction(j,j) = yes $ reservoir(j);
loop ( reservoir(j),

   unmapped(j1) = yes $ [not sameas(j1,j)] ;

   while ( Sum[j1 $ unmapped(j1), 1]
      and ( Sum[ sea(j1) $ reservoirjunction(j,sea), 1] = 0 ),

      loop( j1 $ unmapped(j1),
         reservoirjunction(j,j1) $ Sum[ j2 $ { reservoirjunction(j,j2)
                                           and hydroarc(j2,j1) }, 1 ]
                                 = yes ;
         reservoirjunction(j,j1) $ Sum[ (g,j2) $ { reservoirjunction(j,j2)
                                               and hydrodef(g,j2,j1) }, 1 ]
                                 = yes;
      ) ;
      unmapped(j1) $ reservoirjunction(j,j1) = no ;

   ) ;

   reservoirplant(j,hydro(g)) $ Sum[ (j1,j2) $ { reservoirjunction(j,j1)
                                             and hydrodef(g,j1,j2) }, 1 ] = yes;

) ;


* Calculating parameters
heatrate(g) $ thermal(g)
   =  sum[ (n,f), thermalstationparameters(g,n,f,'heatrate') ] ;

opercost(g) $ thermal(g)
   =  sum[ (n,f), thermalstationparameters(g,n,f,'operationcost') ] ;

plantcapacity(t,b,g) $ thermal(g)
   = sum[ (n,f), thermalstationparameters(g,n,f,'capacity') ];

plantcapacity(t,b,g) $  hydro(g)
   = sum[ (n,fj,tj), hydrostationparameters(g,n,fj,tj,'capacity')] ;

fixedoutput(t,b,g) = sum[ n, fixedstationparameters(g,n,t,b) ] ;

conversionfactor(g) $ hydro(g)
   = sum[ (n,fj,tj), hydrostationparameters(g,n,fj,tj,'powerfactor')] ;

maxspillflow(g) $ hydro(g)
   = sum[ (n,fj,tj), hydrostationparameters(g,n,fj,tj,'maxspill')] ;

txcapacity(fn,tn)
    = transmissionparameters(fn,tn,'capacity') ;

voll_mw(t,b,n,ls) = demand(t,b,n) * valueoflossload(n,ls,'proportion') ;
voll_nzd(t,b,n,ls) = valueoflossload(n,ls,'cost') ;

minflow(fj,tj) $ hydroarc(fj,tj) = hydroarcparameters(fj,tj,'minflow') ;
maxflow(fj,tj) $ hydroarc(fj,tj) = hydroarcparameters(fj,tj,'maxflow') ;

storagecapacity(j) $ reservoir(j) = reservoirparameters(j,'capacity') ;
initialstorage(j) $ reservoir(j) = reservoirparameters(j,'initial') ;

srmc(t,g) $ thermal(g) =
   sum[ f $ plantfuel(g,f), heatrate(g) * fuelcost(t,f) ]
 + sum[ f $ plantfuel(g,f), heatrate(g) * emissionrate(f) * 1e-6 * co2cost(t) ]
 + opercost(g) ;

reservoirfactor(j)
   = Sum[ hydro(g) $ reservoirplant(j,g), conversionfactor(g)] $ reservoir(j);

startstorage(s,t,j) = initialstorage(j) $ (ord(t) = 1) ;
o_endstorage(s,t,j) = 0;


watersegmentgwh(t,vs)   $ (ord(t) = card(t)) = endwatervalue(vs,'gwh') ;
waternzdpermwh(t,vs)    $ (ord(t) = card(t)) = endwatervalue(vs,'dollarpermwh');

watersegmentgwh(t,vs) $ { (ord(vs) > 1) and waternzdpermwh(t,vs) }
   = watersegmentgwh(t,vs) - watersegmentgwh(t,vs-1);
watersegmentgwh(t,vs) = round(watersegmentgwh(t,vs),2) ;

penaltycost = 500;

validcuts(t,i) = no ;
slopes(t,j,i)  = 0 ;
intercepts(t,i) = 0 ;


loop [s,

   option clear = seq ;
   option clear = startstorage ;
   option clear = o_endstorage ;

   seq(s)  = yes;
   iter = iter + 1;


*  Forward solve ---------------------------------------------------------------
   loop (t,

*     Reset all sets, parameters and variables
      option clear = ti ;
*     Decison variables
      option clear = COST ;
      option clear = GENERATION ;
      option clear = FLOWTHROUGHTURBINE ;
      option clear = FLOWTHROUGHSPILLWAY ;
      option clear = FLOWONHYDROARC ;
      option clear = TRANSMISSIONFLOW ;
      option clear = SHORTAGE ;
      option clear = ENDSTORAGE
      option clear = ENDSTORAGESEGMENTGWH ;
      option clear = FUTURECOST ;
*     Slack variables
      option clear = SPILLWAYVIOLATION ;
      option clear = MAXARCFLOWVIOLATION ;
      option clear = MINARCFLOWVIOLATION ;
*     End reset

      ti(t)  = yes;
      lastinterval = 1 $ [ ord(t) = card(t) ] ;
      startstorage(seq,t,reservoir(j)) $ (ord(t) = 1) = initialstorage(j)  ;
      startstorage(seq,t,reservoir(j)) $ (ord(t) > 1) = o_endstorage(seq,t-1,j);

*     Set upper bound, lower bound or fixed value for decision variables
      GENERATION.fx(seq,ti,b,g) $ fixed(g) = fixedoutput(ti,b,g);
      FLOWONHYDROARC.fx(seq,ti,b,fj,tj) $ (not hydroarc(fj,tj)) = 0 ;
      SHORTAGE.up(seq,ti,b,n,ls) = voll_mw(ti,b,n,ls) ;
      TRANSMISSIONFLOW.up(seq,ti,b,fn,tn) = txcapacity(fn,tn) ;
      ENDSTORAGE.up(seq,ti,j) = storagecapacity(j) ;
      ENDSTORAGESEGMENTGWH.up(seq,ti,vs) = watersegmentgwh(ti,vs);

      option bratio = 1 ;
      HydroThermalModel.reslim = LPTimeLimit ;
      HydroThermalModel.iterlim = LPIterationLimit ;
      solve HydroThermalModel using lp minimizing COST ;
*     Set the model solve status
      ModelSolved = 1 $ { (HydroThermalModel.modelstat = 1)
                      and (HydroThermalModel.solvestat = 1) };

*     Post a progress message to the console and for use by EMI.
      if ((ModelSolved = 0),
         putclose runlog 'The week ' t.tl ' is solved unsuccessfully.'/
         ) ;

      o_systemcost(seq,ti)       = COST.l ;

$ontext
      o_generation(seq,ti,b,g)   = GENERATION.l(seq,ti,b,g) ;
      o_generationcost(seq,ti,b)
         = Sum[ thermal(g), GENERATION(seq,ti,b,g)
                          * srmc(ti,g) * blockhour(ti,b) ];

      o_shortage(seq,ti,b,n)     = Sum[ ls, SHORTAGE.l(seq,ti,b,n,ls) ] ;
      o_shortagecost(seq,ti,b)   = Sum[ (n,ls), SHORTAGE.l(seq,ti,b,n,ls)
                                              * voll_nzd(ti,b,n,ls)
                                              * blockhour(ti,b)] ;

      o_spillwayviolation(seq,ti,b,g) = SPILLWAYVIOLATION.l(seq,ti,b,g) ;
      o_spillwayviolationcost(seq,ti,b)
         = Sum[ g $ hydro(g), SPILLWAYVIOLATION(seq,ti,b,g)
                             * blockhour(ti,b) * penaltycost ] ;

      o_maxarcflowviolation(seq,ti,b,fj,tj)
         = MAXARCFLOWVIOLATION.l(seq,ti,b,fj,tj) ;
      o_minarcflowviolation(seq,ti,b)
         = Sum[ (fj,tj) $ hydroarc(fj,tj), MAXARCFLOWVIOLATION(seq,ti,b,fj,tj)
                                         * blockhour(ti,b) * penaltycost ] ;

      o_minarcflowviolation(seq,ti,b,fj,tj)
         = MINARCFLOWVIOLATION.l(seq,ti,b,fj,tj) ;
      o_minarcflowviolation(seq,ti,b)
         = Sum[ (fj,tj) $ hydroarc(fj,tj), MINARCFLOWVIOLATION(seq,ti,b,fj,tj)
                                         * blockhour(ti,b) * penaltycost ] ;

      o_futurecost(seq,ti) = FURTURECOST.l(sq,ti) ;

      o_endstoragevalue(seq,ti)
         = Sum[ (seq,ti,vs) $ waternzdpermwh(ti,vs)
              , ENDSTORAGESEGMENTGWH.l(seq,ti,vs) * waternzdpermwh(ti,vs) ] ;
$offtext

      o_endstorage(seq,ti,j) = ENDSTORAGE.l(seq,ti,j) ;

   ) ;
*  Forward solve end -----------------------------------------------------------


* Convergence test -------------------------------------------------------------



* Convergence test end ---------------------------------------------------------


*$ontext

*  Backward solve --------------------------------------------------------------
   seq(s1) = yes $ [ord(s1) <= ord(s)] ;

   loop ( (t1,t) $ {revt(t1,t) and (ord(t) > 1)},

*     Reset all sets, parameters and variables
      option clear = ti ;
*     Decison variables
      option clear = COST ;
      option clear = GENERATION ;
      option clear = FLOWTHROUGHTURBINE ;
      option clear = FLOWTHROUGHSPILLWAY ;
      option clear = FLOWONHYDROARC ;
      option clear = TRANSMISSIONFLOW ;
      option clear = SHORTAGE ;
      option clear = ENDSTORAGE
      option clear = ENDSTORAGESEGMENTGWH ;
      option clear = FUTURECOST ;
*     Slack variables
      option clear = SPILLWAYVIOLATION ;
      option clear = MAXARCFLOWVIOLATION ;
      option clear = MINARCFLOWVIOLATION ;
*     End reset

      ti(t)  = yes;
      lastinterval = 1 $ [ ord(t) = card(t) ] ;
      startstorage(seq,t,reservoir(j)) = startstorage(s,t,j);

*     Set upper bound, lower bound or fixed value for decision variables
      GENERATION.fx(seq,ti,b,g) $ fixed(g) = fixedoutput(ti,b,g);
      FLOWONHYDROARC.fx(seq,ti,b,fj,tj) $ (not hydroarc(fj,tj)) = 0 ;
      SHORTAGE.up(seq,ti,b,n,ls) = voll_mw(ti,b,n,ls) ;
      TRANSMISSIONFLOW.up(seq,ti,b,fn,tn) = txcapacity(fn,tn) ;
      ENDSTORAGE.up(seq,ti,j) = storagecapacity(j) ;
      ENDSTORAGESEGMENTGWH.up(seq,ti,vs) = watersegmentgwh(ti,vs);

      option bratio = 1 ;
      HydroThermalModel.reslim = LPTimeLimit ;
      HydroThermalModel.iterlim = LPIterationLimit ;
      solve HydroThermalModel using lp minimizing COST ;
*     Set the model solve status
      ModelSolved = 1 $ { (HydroThermalModel.modelstat = 1)
                      and (HydroThermalModel.solvestat = 1) };
*     Post a progress message to the console and for use by EMI.
      if ((ModelSolved = 0),
         putclose runlog 'The week ' t.tl ' is solved unsuccessfully.'/
         ) ;

      o_systemcost(seq,ti)
         = Sum[ (b,g) $ thermal(g), GENERATION.l(seq,ti,b,g)                    !! Thermal generation cost
                                  * srmc(ti,g) * blockhour(ti,b) ]
         + Sum[ (b,n,ls), SHORTAGE.l(seq,ti,b,n,ls)                             !! Shortage cost
                        * voll_nzd(ti,b,n,ls) * blockhour(ti,b) ]
         + FUTURECOST.l(seq,ti)                                                 !! Expected furure cost based on saved cuts

         + Sum[ (b,g) $ hydro(g), SPILLWAYVIOLATION.l(seq,ti,b,g)               !! Spillway violation cost
                                * blockhour(ti,b) * penaltycost ]
         + Sum[ (b,hydroarc(fj,tj)), MAXARCFLOWVIOLATION.l(seq,ti,b,fj,tj)      !! Max arc flow violation cost
                                   * blockhour(ti,b) * penaltycost ]
         + Sum[ (b,hydroarc(fj,tj)), MINARCFLOWVIOLATION.l(seq,ti,b,fj,tj)      !! Min arc flow violation cost
                                   * blockhour(ti,b) * penaltycost ]
         - Sum[ vs $ waternzdpermwh(ti,vs)                                      !! Storage value at the end of simulation time zone
                   , ENDSTORAGESEGMENTGWH.l(seq,ti,vs) * waternzdpermwh(ti,vs) ]
      ;

      o_slope(reservoir(j))
         = Sum[ (seq,ti)
              , FlowInEqualFlowOutReservoirConstraint.m(seq,ti,j) / card(seq) ] ;

      o_intercept = Sum[ (seq,ti), o_systemcost(seq,ti) / card(seq)]
                  - Sum[ (seq,ti,reservoir(j))
                       , o_slope(j) * startstorage(seq,ti,j) / card(seq) ];


      slopes(t2,j,i)
         $ { (ord(t2)+1 = ord(t)) and reservoir(j) and (ord(i) = iter) }
         = o_slope(j)  ;

      intercepts(t2,i)
         $ { (ord(t2)+1 = ord(t)) and (ord(i) = iter) } = o_intercept ;

      validcuts(t2,i) $ { (ord(t2)+1 = ord(t)) and (ord(i) = iter) } = yes;

   ) ;
*  Backward solve end -----------------------------------------------------------
*$offtext

] ;

execute_unload 'savedoutput.gdx'

;

putclose runlog / 'SM run ended at: ' system.date " " system.time /;
putclose runlog / 'SM run time: ' timeElapsed /;
