$oneolcom

*Primay sets
SETS
   n(*)                                                                         'region or node'
   t(*)                                                                         'time interval of the year - can be day, week, month or quater'
   b(*)                                                                         'demand block'
   j(*)                                                                         'hydro junction'
   s(*)                                                                         'hydro inflow sequences'
   g(*)                                                                         'generation plant'
   f(*)                                                                         'fuel types: coal, diesel, gas, lignite, hydro, wind, geo'

   ls(*)                                                                        'loss load tranches'
   vs(*)                                                                        'Stored energy value segment'

   i(*)                                                                         'Pre-set number of iterations'

;

Alias (n, fn, tn),   (j, j1, j2, fj, tj),   (g, g1, g2)
      (vs, vs1, vs2), (t, t1, t2, t3)
;

SETS
* model sets
   ti(t)                                                                        'current solving time interval'
   seq(s)                                                                       'current applied hydro sequence'
;

*Secondary sets
SETS
   sea(j)                                                                       'junction represent SEA'
   reservoir(j)                                                                 'reservoir or lake that can store water'
   hydroarc(fj,tj)                                                              'definition of hydro arc'
   plantnode(g,n)                                                               'location of generation plant'
   plantfuel(g,f)                                                               'fuel used for plant(g)'
   hydrodef(g,fj,tj)                                                            'hydro generation station definition of inflow from and outflow to'

   fixed(g)                                                                     'generation plants with profiled fixed outputs'
   thermal(g)                                                                   'thermal plants using fuel to generate'
   hydro(g)                                                                     'hydro generation station'

   validcuts(t,i)                                                               'Flag the valid cuts to be applied to calculate expected future cost'

;



PARAMETERS
* Input
   demand(t,b,n)                                                                'average demand of block bl in interval t - MW'
   blockhour(t,b)                                                               'number of hours in block bl - hours'

   heatrate(g)                                                                  'heat rate of fuel(f) applied to generation plant(ge) - GJ/MWh'
   opercost(g)                                                                  'operation and maitemance cost $/MWh'
   fuelcost(t,f)                                                                'cost of fuel in interval t - $/GJ'
   co2cost(t)                                                                   'CO2 cost $/ton'
   emissionrate(f)                                                              'ton CO2/PJ <--> 1(PJ)  = 1e6(GJ)'

   fixedoutput(t,b,g)                                                           'Fixed generation output applied to fixed generation plant - MW'
   plantcapacity(t,b,g)                                                         'Capacity of generation plant(g) during timet) and block(b) - MW'

   conversionfactor(g)                                                          'Specific power conversion factor MW/cumec'
   maxspillflow(g)                                                              'Maximum flow cumec(s) through spillway at a hydro station'

   txcapacity(fn,tn)                                                            'Transmission capacity - MW'
   inflow(s,t,j)                                                                'Infows are defined in cumec(s) for locations (hydro junctions) in the system'

   voll_mw(t,b,n,ls)                                                            'Maximum shortage penalty quantity for loss load tranche ls (MW)'
   voll_nzd(t,b,n,ls)                                                           'Shortage penalty cost applied for loss load tranche ls ($/MWh)'

   minflow(t,b,fj,tj)                                                           'Min flow requirement on flow arc between two hydro junctions'
   maxflow(t,b,fj,tj)                                                           'Max flow requirement on flow arc between two hydro junctions'

   storagecapacity(j)                                                           'Storage capacity (m3) at each reservoir'
   initialstorage(j)                                                            'Initial storage (m3) at each reservoir'

* Calculated input
   srmc(t,g)                                                                    'Pre-calculated short run marginal cost for a thermal plant - $/MWh'
   startstorage(s,t,j)                                                          'Pre-calculated storage level at the start of time interval(t) at reservoir(j)'
   reservoirfactor(j)                                                           'Specific power factor for each reservoir'

   watersegmentgwh(t,vs)                                                        'End storage segments in GWh'
   waternzdpermwh(t,vs)                                                         'Value of end storage segments in $/MWh'

   slopes(t,j,i)                                                                'Slopes of cut i'
   intercept(t,i)                                                               'Intercept of cut i'

;


VARIABLES
   COST                                                                         'Expected system cost'
;


POSITIVE VARIABLES
* Decision variables
   GENERATIONCOST(s,t,b)                                                        'Generation cost by demand block '
   SHORTAGECOST(s,t,b)                                                          'Shortage cost by demand block '
   VIOLATIONCOST(s,t,b)                                                         'Constraint violation cost by demand block'
   FUTURECOST(s,t)                                                              'Expected minimum furture cost and the and of time(t)'
   ENDSTORAGEVALUE(s,t)                                                         'Value of end total storage $'

   TRANSMISSIONFLOW(s,t,b,fn,tn)                                                'Energy flow on transmision branch - MW'
   SHORTAGE(s,t,b,n,ls)                                                         'Loss load allocated to each loss load tranche - MW'
   GENERATION(s,t,b,g)                                                          'Cleared generation @ plant(g) for time(t), block(b) of hydro sequence(s)'

   FLOWTHROUGHTURBINE(s,t,b,g)                                                  'Hydro flow cumec(s) through generation turnbines of hydro plant (g)'
   FLOWTHROUGHSPILLWAY(s,t,b,g)                                                 'Hydro flow cumec(s) through spillway of hydro plant (g)'
   FLOWONHYDROARC(s,t,b,fj,tj)                                                  'Hydro flow release on hydro arc between two hydro junctions - cumec(s)'

   ENDSTORAGE(s,t,j)                                                            'Storage level (m3) at the end of time interval (t) at reservoir (j)'
   ENDSTORAGEGWH(s,t)                                                           'Total storage (GWh) at the end of time interval (t)'
   ENDSTORAGESEGMENTGWH(s,t,vs)                                                 'Segment storage (GWh) at the end of time interval (t)'

* Slack variables
   SURPLUSFIXEDGENERATION(s,t,b,g)                                              'Violation of fixed output - unsued MW'
   SPILLWAYVIOLATION(s,t,b,g)                                                   'Violation of spillway limit - cumec(s) '
   MAXARCFLOWVIOLATION(s,t,b,fj,tj)                                             'Violation of max flow on hydro arc - cumec(s) '
   MINARCFLOWVIOLATION(s,t,b,fj,tj)                                             'Violation of min flow on hydro arc - cumec(s) '
;


EQUATIONS
   ObjectiveFunction                                                            'Objective function'

   ThermalGenerationCostCalculation(s,t,b)                                      'Thermal generation cost calculation'
   ShortageCostCalculation(s,t,b)                                               'Shortage cost calculation'
   ViolationCostCalculation(s,t,b)                                              'Violation cost calculation'
   ExpectedFutureCost(s,t,i)                                                    'Expected minimum future cost calculated based on saved cuts'
   EndStorageValueCalculation(s,t)                                              'Calculate end storage value $'

   EnergyDemandSupplyBalance(s,t,b,n)                                           'Total Energy Supply to a node (region) equal to Demand'

   GenerationCapacityConstraint(s,t,b,g)                                        'Generation output is limited by capacity'
   FixedGenerationConstraint(s,t,b,g)                                           'Generation output is limited by capacity'
   HydroGenerationCumecsToMWConversion(s,t,b,g)                                 'Conversion hydro flow(cumecs) through turbines to generation output (MW)'
   HydroGenerationSpillMaxConstraint(s,t,b,g)                                   'Maximum spilled flow (cumecs)'

   FlowInEqualFlowOutConstraint(s,t,j)                                          'Total Hydro flow in should be equal to hydro flow out off a hydro junction'
   FlowInEqualFlowOutJunctionConstraint(s,t,j)                                  'Total Hydro flow in should be equal to hydro flow out off a hydro junction'
   FlowInEqualFlowOutReservoirConstraint(s,t,j)                                 'Total Hydro flow in should be equal to hydro flow out off a hydro reservoir'

   MaxFlowOnArcConstraint(s,t,b,fj,tj)                                          'Maximum hydro arc flow constraint'
   MinFlowOnArcConstraint(s,t,b,fj,tj)                                          'Minimum hydro arc flow constraint'

   EndStorageTotalGWhCalculation(s,t)                                           'Calculate end storage total GWh'
   EndStorageSegmentGWhCalculation(s,t)                                         'Allocate end storage to segment'

;

* Objective function to minimize total cost
ObjectiveFunction..
  COST
=e=
  Sum[ (seq,ti,b) , GENERATIONCOST(seq,ti,b) ]
+ Sum[ (seq,ti,b) , SHORTAGECOST(seq,ti,b)   ]
+ Sum[ (seq,ti,b) , VIOLATIONCOST(seq,ti,b)  ]
+ Sum[ (seq,ti)   , FUTURECOST(seq,ti)       ]
- Sum[ (seq,ti)   , ENDSTORAGEVALUE(seq,ti)  ]

+ Sum[ (seq,ti,b,g), FLOWTHROUGHSPILLWAY(seq,ti,b,g) * 1e-6 ]
  ;

* Cost/benefit calculation
ThermalGenerationCostCalculation(seq,ti,b)..
  GENERATIONCOST(seq,ti,b)
=e=
  Sum[ thermal(g), GENERATION(seq,ti,b,g) * srmc(ti,g) * blockhour(ti,b)]
  ;

ShortageCostCalculation(seq,ti,b) ..
  SHORTAGECOST(seq,ti,b)
=e=
  Sum[ (n,ls), SHORTAGE(seq,ti,b,n,ls) * voll_nzd(ti,b,n,ls) * blockhour(ti,b)]
  ;

ExpectedFutureCost(seq,ti,i) $ validcuts(ti,i)..
  FUTURECOST(seq,ti)
=g=
  Sum[ reservoir(j), slopes(ti,j,i) * ENDSTORAGE(seq,ti,j) ] + intercept(ti,i)
  ;

ViolationCostCalculation(seq,ti,b) ..
  VIOLATIONCOST(seq,ti,b)
=e=
  Sum[ g       $ fixed(g)        , SURPLUSFIXEDGENERATION(seq,ti,b,g)   * 500 ]
+ Sum[ g       $ hydro(g)        , SPILLWAYVIOLATION(seq,ti,b,g)        * 500 ]
+ Sum[ (fj,tj) $ hydroarc(fj,tj) , MAXARCFLOWVIOLATION(seq,ti,b,fj,tj)  * 500 ]
+ Sum[ (fj,tj) $ hydroarc(fj,tj) , MINARCFLOWVIOLATION(seq,ti,b,fj,tj)  * 500 ]
  ;

EndStorageValueCalculation(seq,ti) $ {Sum[ vs, waternzdpermwh(ti,vs)] > 0}..
  ENDSTORAGEVALUE(seq,ti)
=e=
  Sum[ vs $ waternzdpermwh(ti,vs)
     , ENDSTORAGESEGMENTGWH(seq,ti,vs) * waternzdpermwh(ti,vs) ] ;


* Supply Demand Balance
EnergyDemandSupplyBalance(seq,ti,b,n)..
  Sum[ g    $ plantnode(g,n)  , GENERATION(seq,ti,b,g)            ]
+ Sum[ fn   $ txcapacity(fn,n), TRANSMISSIONFLOW(seq,ti,b,fn,n)   ]
- Sum[ tn   $ txcapacity(n,tn), TRANSMISSIONFLOW(seq,ti,b,n,tn)   ]
+ Sum[ ls                     , SHORTAGE(seq,ti,b,n,ls)           ]
=e=
  demand(ti,b,n)
  ;


* Generation Constraint
GenerationCapacityConstraint(seq,ti,b,g) $ { thermal(g) or hydro(g) }..
  GENERATION(seq,ti,b,g) =l= plantcapacity(ti,b,g)
  ;

FixedGenerationConstraint(seq,ti,b,g) $ fixed(g)..
  GENERATION(seq,ti,b,g)
+ SURPLUSFIXEDGENERATION(seq,ti,b,g)
=e=
  fixedoutput(ti,b,g)
  ;

HydroGenerationCumecsToMWConversion(seq,ti,b,g) $ hydro(g)..
  GENERATION(seq,ti,b,g)
=e=
  FLOWTHROUGHTURBINE(seq,ti,b,g) * conversionfactor(g)
  ;

HydroGenerationSpillMaxConstraint(seq,ti,b,g) $ hydro(g)..
  FLOWTHROUGHSPILLWAY(seq,ti,b,g) - SPILLWAYVIOLATION(seq,ti,b,g)
=l=
  maxspillflow(g)
  ;


* Hydro arc/flow constraints
MaxFlowOnArcConstraint(seq,ti,b,fj,tj)
  $ {hydroarc(fj,tj) and maxflow(ti,b,fj,tj)}..
  FLOWONHYDROARC(seq,ti,b,fj,tj) - MAXARCFLOWVIOLATION(seq,ti,b,fj,tj)          !! Flow on a hysro arc may have maximum limit
=l=
  maxflow(ti,b,fj,tj)
 ;

MinFlowOnArcConstraint(seq,ti,b,fj,tj)
  $ {hydroarc(fj,tj) and minflow(ti,b,fj,tj)}..
  FLOWONHYDROARC(seq,ti,b,fj,tj) + MINARCFLOWVIOLATION(seq,ti,b,fj,tj)          !! Flow on a hysro arc may have mimimum requirement
=g=
  minflow(ti,b,fj,tj)
  ;

FlowInEqualFlowOutJunctionConstraint(seq,ti,j)
  $ {not (reservoir(j) or sea(j))}..
  Sum[ (b,tj) $ hydroarc(j,tj),
       FLOWONHYDROARC(seq,ti,b,j,tj)   * blockhour(ti,b) * 3600 ]               !! Hydro flow out of a junction into a hydro arc
- Sum[ (b,fj) $ hydroarc(fj,j),
       FLOWONHYDROARC(seq,ti,b,fj,j)   * blockhour(ti,b) * 3600 ]               !! Hydro flow into a junction from a hydro arc
+ Sum[ (b,g,tj) $ hydrodef(g,j,tj),
       FLOWTHROUGHTURBINE(seq,ti,b,g)  * blockhour(ti,b) * 3600 ]               !! Hydro flow out of a junction into a hydro station
- Sum[ (b,g,fj) $ hydrodef(g,fj,j),
       FLOWTHROUGHTURBINE(seq,ti,b,g)  * blockhour(ti,b) * 3600 ]               !! Hydro flow into a junction from a hydro station
+ Sum[ (b,g,tj) $ hydrodef(g,j,tj),
       FLOWTHROUGHSPILLWAY(seq,ti,b,g) * blockhour(ti,b) * 3600 ]               !! Hydro flow out of a junction into a hydro station spillway
- Sum[ (b,g,fj) $ hydrodef(g,fj,j),
       FLOWTHROUGHSPILLWAY(seq,ti,b,g) * blockhour(ti,b) * 3600 ]               !! Hydro flow into a junction from a hydro station spillway
=e=
  Sum[ b, inflow(seq,ti,j) * blockhour(ti,b) * 3600 ]
  ;

FlowInEqualFlowOutReservoirConstraint(seq,ti,j) $ reservoir(j)..
  Sum[ (b,tj) $ hydroarc(j,tj),
       FLOWONHYDROARC(seq,ti,b,j,tj)   * blockhour(ti,b) * 3600 ]               !! Hydro flow out of a junction into a hydro arc
- Sum[ (b,fj) $ hydroarc(fj,j),
       FLOWONHYDROARC(seq,ti,b,fj,j)   * blockhour(ti,b) * 3600 ]               !! Hydro flow into a junction from a hydro arc
+ Sum[ (b,g,tj) $ hydrodef(g,j,tj),
       FLOWTHROUGHTURBINE(seq,ti,b,g)  * blockhour(ti,b) * 3600 ]               !! Hydro flow out of a junction into a hydro station
- Sum[ (b,g,fj) $ hydrodef(g,fj,j),
       FLOWTHROUGHTURBINE(seq,ti,b,g)  * blockhour(ti,b) * 3600 ]               !! Hydro flow into a junction from a hydro station
+ Sum[ (b,g,tj) $ hydrodef(g,j,tj),
       FLOWTHROUGHSPILLWAY(seq,ti,b,g) * blockhour(ti,b) * 3600 ]               !! Hydro flow out of a junction into a hydro station spillway
- Sum[ (b,g,fj) $ hydrodef(g,fj,j),
       FLOWTHROUGHSPILLWAY(seq,ti,b,g) * blockhour(ti,b) * 3600 ]               !! Hydro flow into a junction from a hydro station spillway
+  ENDSTORAGE(seq,ti,j)
=e=
  startstorage(seq,ti,j)
+ Sum[ b, inflow(seq,ti,j) * blockhour(ti,b) * 3600 ]
  ;

* Storage constraints
EndStorageTotalGWhCalculation(seq,ti) $ {Sum[ vs, waternzdpermwh(ti,vs)] > 0}..
  ENDSTORAGEGWH(seq,ti)
=e=
  Sum[ reservoir(j), ENDSTORAGE(seq,ti,j) * reservoirfactor(j) / 3.6e6 ];

EndStorageSegmentGWhCalculation(seq,ti) $ {Sum[ vs, waternzdpermwh(ti,vs)] > 0}..
  ENDSTORAGEGWH(seq,ti)
=e=
  Sum[ vs, ENDSTORAGESEGMENTGWH(seq,ti,vs)];


Model HydroThermalModel /
* Objective function
   ObjectiveFunction
* Cost Calculation
   ThermalGenerationCostCalculation
   ShortageCostCalculation
   ExpectedFutureCost
   ViolationCostCalculation
   EndStorageValueCalculation
* Supply Demand
   EnergyDemandSupplyBalance
* Generation Constraint
   GenerationCapacityConstraint
   FixedGenerationConstraint
   HydroGenerationCumecsToMWConversion
   HydroGenerationSpillMaxConstraint
* Hydro arc/flow constraints
   MaxFlowOnArcConstraint
   MinFlowOnArcConstraint
   FlowInEqualFlowOutJunctionConstraint
   FlowInEqualFlowOutReservoirConstraint
* Storage constraint
   EndStorageTotalGWhCalculation
   EndStorageSegmentGWhCalculation
   / ;
