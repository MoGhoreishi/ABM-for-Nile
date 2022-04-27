extensions [ time  matrix csv]
globals[
  policy-patches
  land-patches
  policymaker-num
  farmer-num
  area-ag-tot
  a-rf
  a-rf-obs
  a-rf-list
  a-rf-obs-list
  a-ig-list
  a-ig
  a-ig-obs
  a-ig-obs-list
  a-rf-ET
  RMSE-ig
  data
  data2
  Year
  Month
  cy-ig;crop yield (kg/hectare)/FAO
  cy-rf;(kg/hectare)/the irrigated yield with a coefficient
  cy-rf-ET;(kg/hectare)/FAO
  implemented-c-ig-tot
  implemented-c-ig-tot-list
  cost-total-ig ;;
  crop-price-ig ;;($/kg)crop price,
  cost-total-rf
  crop-price-rf ;;($/kg)crop price,
  a-0-ig
  a-0-rf
  precipitation;; from the data of the summer school
  ET-Maze;;http://www.fao.org/3/s2022e/s2022e02.htm
  Q-HP-ET; streamflow for hydropower downstream in Ethiopia (CMS)
  Q-TB-ET; streamflow for Transboundary flow from Ethiopia to Sudan and Egypt (CMS)
  Q-totTB-ET; total streamflow for Transboundary flow from Ethiopia to Sudan and Egypt (CMS)
  Q-baseTB-ET; monthly averaged natural transboundary flow without GERD (CMS)
  delta; increasing amount on transboundary flow from Ethiopia
  gdc;grwoth in dam capacity (BCM), the Nile basin water resources Atlas report page 175 pdf, 0.55 per year
  dc-ET; future dam capacity in Ethiopia(BCM)
  pdc-ET; potential dam capacity in Ethiopia;180BCM;based on the paper "Screening reservoir systems by considering the efficient trade-offs—informing infrastructure investment decisions on the Blue Nile"
  month-n
  tot-fc-ET ;population * food consumption
  pop-ET ;population
  fc-ET  ;food consumption (Kg/per captia)
  xx
  yy
  ff
]

breed [ farmers farmer ]
breed [ policymakers policymaker]

farmers-own
 [

 Hectares-tot
 W-tot;; the summation of all socio-economic random factors
 risk-aversion
 profit
 future
 socio
 AAA-ig ;; coefficients of profit function
 BBB-ig ;; coefficients of profit function
 CCC-ig ;; coefficients of profit function
 AAA-ig-f ;; coefficients of profit function for future
 BBB-ig-f ;; coefficients of profit function for future
 CCC-ig-f ;; coefficients of profit function for future
 adopted-ig?
 delta-c-ig-socio
 c-ig ;;artificial conservation area for avaluating c
 implemented-c-ig-list ;;the conservation area which is applied
 implemented-c-ig
 implemented-c-ig-tot-farmer ;; the total arear which has been coserved since the begining
 implemented-c-ig-tot-list-farmer
 d-ig
 delta-c-ig-future
 delta-c-ig-profit
 delta-c-ig-cons
 wrd; water resources develpment coefficient
 profit-diff
 profit-diff-future
 profit-ig
 profit-rf-future
 profit-ig-future
 profit-rf
 profit-rf-future
 p-list-ig
 n-list-ig
 p-list-ig-future
 n-list-ig-future
 c-iglist
 d-iglist
 p25ig
 p50ig
 p75ig
 n25ig
 n50ig
 n75ig
 ]

policymakers-own
[
;plocymaker 0, 1, and 2 are eth, sud, and eg
collective-conflict
history
strategies ;; list of strategies
best-strategy ;; index of the current best strategy
prediction   ;; current prediction of conflict in the basin
conflict?
coop; the probability function
coop-list
coop-con; the quantitaive result of the cooperation for an agent
con
memory-size
number-strategies
alpha1 ; coefficient for all ag sector, hp sector, and dc
alpha2 ; coefficient for ps
alpha3 ; coefficient for fdi
alpha4 ; coefficient for prediction
beta1  ; coefficient for ag
beta2  ; coefficient for hp
beta3  ; coefficient for dc
gama  ; coefficient for development in irrigation
past-ag-benefit-coop; it shows the past agricultural benefit which has been resulted by the cooperation
past-ag-benefit-con; it shows the past agricultural benefit which has been resulted by the conflict
past-hp-benefit-coop; it shows the past hydropower benefit which has been resulted by the cooperation
past-hp-benefit-con; it shows the past hydropower benefit which has been resulted by the conflict
fdi; it shows the investments by the foreigner countries in each country
ps; political stability
hp; hydropower generation
pec; potential energy capacity
food-consumption; food consumption (kg)
wc; willing to cooperate
food-gap; food gap
a-r;rainfed area for rainfed (ha/year)
cy-r;crop yield (ton/ha)
fp-r;rainfed food production (kg)
a-ir;irrigated area (ha/year)
cy-ir;crop yield for irrigation (ha/year)
fp-ir;irrigated food production (kg)
fp-tot; total food production (kg)
fc; food consumption (kg per capita)
pop; population
tot-fc; food consumption (kg)
]

to setup
  ca
  ;1983-2016;1962-2017
  ;;year = 1962
  set Year 0
  set Month 0
  file-close-all
  file-open "C:\\Users\\ghore\\OneDrive - University of Saskatchewan\\GIWS Project\\ENSH Model\\AB_Model\\Data_Nile\\Data2_V3.csv"

  set land-patches patches with [pycor < 8 or (pxcor <  8 and pycor >= 8)]
  ask land-patches [ set pcolor 63 ]

  ;; create the 'bar'
  set policy-patches patches with [pxcor > 8 and pycor > 8]
  ask policy-patches [ set pcolor white ]

  ;ask policymakers [  ];random 3]


  set policymaker-num 3
  create-policymakers policymaker-num [

    set conflict? true


    ;; consistent appearance
    ;set color green
    ;set shape "face happy"
    set color red
    set shape "face sad"
    set coop-con 1
    set size 3

    ]


  ask policymaker 0 [
  setxy  11 11
  set conflict? false
  set color green
  set coop-con 1
  set shape "face happy"
  set memory-size 3
  set number-strategies 3
  set alpha1 0.25
  set alpha2 0.25
  set alpha3 0.25
  set alpha4 0.25
  set beta1 0.33
  set beta2 0.33
  set beta3 0.33
  set pec 980;15000
  ]

  ask policymaker 1 [
  setxy  13 14
  set memory-size 3
  set number-strategies 3
  set alpha1 0.25
  set alpha2 0.25
  set alpha3 0.25
  set alpha4 0.25
  set beta1 0.33
  set beta2 0.33
  set beta3 0.33
  set gama 0.001
  set food-consumption   1000000
  set pec 8240
  ]

  ask policymaker 2 [
  setxy  15 11
  set memory-size 3
  set number-strategies 3
  set alpha1 0.25
  set alpha2 0.25
  set alpha3 0.25
  set alpha4 0.25
  set beta1 1
  set beta2 0
  set beta3 0
  set food-consumption   1000000
  set pec 8000
  ]


  ask policymaker 0 [
    set history n-values (memory-size * 2) [2]
    set collective-conflict first history
    set strategies n-values number-strategies [random-strategy]
    set best-strategy first strategies
    update-strategies
  ]
    ask policymaker 1 [
    set history n-values (memory-size * 2) [0]
    set collective-conflict first history
    set strategies n-values number-strategies [random-strategy]
    set best-strategy first strategies
    update-strategies
  ]
    ask policymaker 2 [
    set history n-values (memory-size * 2) [0]
    set collective-conflict first history
    set strategies n-values number-strategies [random-strategy]
    set best-strategy first strategies
    update-strategies
  ]
  ask policymaker 0 [
    set coop 0.6
  ]
  ask policymaker 1 [
    set coop 0.6
  ]
  ask policymaker 2 [
    set coop 0.6
  ]

  ask policymakers [ set coop-list n-values 1 [0.6] ]

  set farmer-num 20
    create-farmers farmer-num [
    ;; adoption property is initialized
    set adopted-ig? false

    ;random-seed 0
    ;; separate the farmers spatially
    move-to-empty-one-of land-patches

    ;; consistent appearance
    set color white
    set shape "person farmer"
    set size 2
    ]
  ask farmers [ set implemented-c-ig 19579.692 / farmer-num] ;5392.046 / farmer-num] ; the change in irrigated area between 1983 and 1982; 1962 and 1961
  ask farmers [ set implemented-c-ig-tot-farmer 167445.014 / farmer-num ] ;74333.84 / farmer-num ] ;

  ;random-seed 100
  ask farmers [ set wrd (random-normal mean-wrd SD-wrd)]
  ;random-seed 100
  ask farmers [ set risk-aversion (random-normal w-risk-aversion SD-w-risk-aversion)]
  ;random-seed 100
  ask farmers [ set future (random-normal w-future SD-w-future)]
  ;random-seed 100
  ask farmers [ set profit (random-normal w-profit SD-w-profit)]
  ;random-seed 100
  ask farmers [ set socio (random-normal w-socio SD-w-socio)]
  ;random-seed 100

  ask farmers [ set c-ig 0 ]
  ask farmers [ set d-ig 0 ]
  ask farmers [ set n-list-ig n-values 1 [-150] ] ; there are 2 resources. I used the first one for Maize; http://www.fao.org/3/w7314e/w7314e0h.htm  ;;;40% difference in income between irrigated and rainfed; https://www.jstor.org/stable/pdf/4410113.pdf?refreqid=excelsior%3A488be8fb5e6150ee830389392f400c27 ;  Poverty and Income Distribution in Rainfed and Irrigated Ecosystems Village Studies in Chhattisga
  ask farmers [ set p-list-ig n-values 1 [150] ] ;
  ask farmers [ set n-list-ig-future n-values 1 [-150] ]
  ask farmers [ set p-list-ig-future n-values 1 [150] ]
  ask farmers [ set implemented-c-ig-list n-values 1 [19579.692 / farmer-num] ];5392.046 / farmer-num] ]
  set implemented-c-ig-tot-list n-values 1 [167445.014];74333.84]
  ask farmers [ set implemented-c-ig-tot-list-farmer n-values 1 [167445.014 / farmer-num] ];74333.84 / farmer-num] ]
  set a-rf-list n-values 1 [6573285.678];3304477]
  set a-ig-obs-list n-values 1 [167445.014];74333.84] ;the historic area of irrigated area
  set a-rf-obs-list n-values 1 [6573285.678];3304477]
  set a-ig-list n-values 1 [167445.014];74333.84] ;the historic area of irrigated area
  set a-0-ig 167445.014;74333.84
  set a-0-rf 6573285.678;3304477
  set ET-Maze 650
  set Q-HP-ET 0
  set Q-TB-ET 0
  set gdc 0.55
  set dc-ET 0
  set pdc-ET 180
  ask farmers [
    create-link-with one-of other farmers
  ]
   reset-ticks
end

to go
  tick
  set Month Month + 1
  ifelse remainder Month 12 = 0
  [set month-n 12]
  [set month-n remainder Month 12 ]

  let remain remainder Month  12

   ;; read csv file line to line -- read monthly data
  set data2 csv:from-row file-read-line
  ;; modifying the reading files
  set data2 word data2 " "

  ask policymaker 0 [
    ;set a-ir sublist  ( read-from-string data2 ) 26 614
    ;set cy-ir  sublist  ( read-from-string data2 ) 971 1559
    set fp-ir 1000 * item 26 ( read-from-string data2 ) ; convert to kg
    ;set fp-ir 1000 * sum(map * a-ir cy-ir) ; convert to kg
    ;set a-r sublist  ( read-from-string data2 ) 1916 2504
    ;set cy-r  sublist  ( read-from-string data2 ) 2861 3449
    ;set fp-r 1000 * sum(map * a-r cy-r) ; convert to kg
    set fp-r 1000 * item 29 ( read-from-string data2 ) ; convert to kg
    set fp-tot fp-r + fp-ir
    set fc item 32 ( read-from-string data2 ) ;per capita
    set pop item 35 ( read-from-string data2 )
    set tot-fc fc * pop

  ]
   ask policymaker 1 [
    ;set a-ir sublist  ( read-from-string data2 ) 614 908
    ;set cy-ir  sublist  ( read-from-string data2 ) 1559 1853
    ;set fp-ir 1000 * sum(map * a-ir cy-ir) ; convert to kg
    set fp-ir 1000 * item 27 ( read-from-string data2 ) ; convert to kg
    ;set a-r sublist  ( read-from-string data2 ) 2504 2798
    ;set cy-r  sublist  ( read-from-string data2 ) 3449 3743
    ;set fp-r 1000 * sum(map * a-r cy-r) ; convert to kg
    set fp-r 1000 * item 30 ( read-from-string data2 ) ; convert to kg
    set fp-tot fp-r + fp-ir
    set fc item 33 ( read-from-string data2 ) ;per capita
    set pop item 36 ( read-from-string data2 )
    set tot-fc fc * pop

  ]
  ask policymaker 2 [
    ;set a-ir sublist  ( read-from-string data2 ) 908 971
    ;set cy-ir  sublist  ( read-from-string data2 ) 1853 1916
    ;set fp-ir 1000 * sum(map * a-ir cy-ir) ; convert to kg
    set fp-ir 1000 * item 28 ( read-from-string data2 ) ; convert to kg
    ;set a-r sublist  ( read-from-string data2 ) 2798 2861
    ;set cy-r  sublist  ( read-from-string data2 ) 3743 3806
    ;set fp-r 1000 * sum(map * a-r cy-r) ; convert to kg
    set fp-r 1000 * item 31 ( read-from-string data2 ) ; convert to kg
    set fp-tot fp-r + fp-ir
    set fc item 34 ( read-from-string data2 ) ;per capita
    set pop item 37 ( read-from-string data2 )
    set tot-fc fc * pop

  ]

  set Q-HP-ET item 18 ( read-from-string data2 )
  set Q-TB-ET item 19 ( read-from-string data2 )
  set Q-baseTB-ET item 20 ( read-from-string data2 )

if month < 408 [
  if remain = 1   [

  ; based on the following study, we can simply calculate the rough value of crop yield in raifed and irrigated for maize; so rainfed/irrigated =~ 0.4;https://iopscience.iop.org/article/10.1088/1748-9326/aac4b1
   ;; read csv file line to line -- read yearly data
  set cy-ig  item 0 ( read-from-string data2 )
  set cy-rf  item 1 ( read-from-string data2)

  set crop-price-ig item 2 ( read-from-string data2 )
  set crop-price-rf item 3 ( read-from-string data2 )

  set cost-total-ig item 4 ( read-from-string data2 )
  set cost-total-rf item 5 ( read-from-string data2 )

  set a-ig-obs item 6 ( read-from-string data2 )
  set a-rf-obs item 7 ( read-from-string data2 )
  set area-ag-tot item 8 ( read-from-string data2 )

  set precipitation item 9 ( read-from-string data2 )
  set cy-rf-ET item 22 ( read-from-string data2 )
  set a-rf-ET item 23 ( read-from-string data2 )
  set fc-ET item 24 ( read-from-string data2 )
  set pop-ET item 25 ( read-from-string data2 )

  set tot-fc-ET pop-ET * fc-ET

  ask policymaker 0 [

        set ps item 11 ( read-from-string data2 )
        set fdi item 14 ( read-from-string data2 )
        set hp item 17 ( read-from-string data2 )

  ]

  ask policymaker 1 [
        set conflict? true
        set ps item 12 ( read-from-string data2 )
        set fdi item 15 ( read-from-string data2 )
        set hp item 38 ( read-from-string data2 )

  ]

  ask policymaker 2 [
        set conflict? true
        set ps item 13 ( read-from-string data2 )
        set fdi item 16 ( read-from-string data2 )

  ]
    ]
  ]
;  if Month = 1   [
  ;  Farmer-Decision-Making
 ; ]

  if month < 408 [
  if remain = 0   [
      Policymaker-Decision-Making
    ]
  ]


  set Q-totTB-ET Q-HP-ET + delta * Q-baseTB-ET + Q-TB-ET


 ; if month < 408 [
 ; if remain = 0   [
  ;    Policymaker-Decision-Making-Sud
     ; Farmer-Decision-Making
  ;  ]
  ;]

 ; if month > 12 [
 ; if month < 408 [
 ;; if remain = 11   [


  ;;      set a-rf area-ag-tot - implemented-c-ig-tot
  ;      set a-ig implemented-c-ig-tot


  ;      set a-ig  (1 - (precipitation / ET-Maze)) * a-ig

   ;     set a-rf-list insert-item ( length a-rf-list ) a-rf-list a-rf
    ;    set a-ig-list insert-item ( length a-ig-list ) a-ig-list a-ig

     ;   set a-rf-obs-list insert-item ( length a-rf-obs-list ) a-rf-obs-list a-rf-obs
      ;  set a-ig-obs-list insert-item ( length a-ig-obs-list ) a-ig-obs-list a-ig-obs

       ; ask policymaker 1 [ set food-gap  food-consumption - (a-ig * cy-ig  + a-rf * cy-rf )]
       ; set mean-wrd [gama] of policymaker 1 * ([food-gap] of policymaker 1 / [food-consumption] of policymaker 1)
    ;]
;]
;]

;;  set   RMSE-ig compute-RMSE a-ig-list a-ig-obs-list

  if Month = 408 [  stop  ]
end

to Farmer-Decision-Making

  ;;year = 1962 to 2017

  ask farmers [ set Hectares-tot area-ag-tot / farmer-num ]
  ask farmers [
    set adopted-ig? false
    set color white
    set c-ig 0
    set d-ig 0
    set implemented-c-ig 0
  ]

;;-----------------------------------------------------------------------------agriculture sector (rainfed to irrigated area)-------------------------------------------------------
  ;;formulation for delta-c-tech-profit
  ask farmers [ set profit-ig     ( ( crop-price-ig  * cy-ig  ) - cost-total-ig )   ]
  ask farmers [ set profit-rf   ( ( crop-price-rf  * cy-rf  ) - cost-total-rf ) ]
  ask farmers [ set profit-diff   profit-ig - profit-rf  ]
  ;;calculating the historical positive and negative profit-diff-ig-list
  ask farmers [
    ifelse  ( profit-diff >= 0 ) [ set p-list-ig insert-item ( length p-list-ig ) p-list-ig profit-diff ] [ set n-list-ig insert-item ( length n-list-ig ) n-list-ig profit-diff ]
  ]
  ;; calculating the quartiles
  ask farmers [ set p25ig lower-quartile p-list-ig ]
  ask farmers [ set p50ig median p-list-ig ]
  ask farmers [ set p75ig upper-quartile p-list-ig ]

  ask farmers [ set n25ig lower-quartile n-list-ig ]
  ask farmers [ set n50ig median n-list-ig ]
  ask farmers [ set n75ig upper-quartile n-list-ig ]
  ;; equation solver
  if Year > 1 [
  ask farmers [ if  ( profit-diff >= 0 )
  [
  let AA ( list  (list (p75ig ^ 2) p75ig 1) (list (p50ig ^ 2) p50ig 1)  (list (p25ig ^ 2) p25ig 1) )
  let A matrix:from-row-list AA
  let B matrix:from-row-list [[1][0.5][0]]
  ;;print matrix:solve A B
  let m1 matrix:to-column-list matrix:solve A B
  set AAA-ig item 0 ( item 0  m1 )
  set BBB-ig item 1 ( item 0  m1 )
  set CCC-ig item 2 ( item 0  m1 )
  ]

  ;; I trunt the following codes off as switching to irrigation should not give you losse

  ;[
  ;let AA ( list  (list (n75ig ^ 2) n75ig 1) (list (n50ig ^ 2) n50ig 1)  (list (n25ig ^ 2) n25ig 1) )
  ;let A matrix:from-row-list AA
  ;let B matrix:from-row-list [[0][0.5][1]]
  ;;print matrix:solve A B
  ;let m1 matrix:to-column-list matrix:solve A B
  ;set AAA-ig item 0 ( item 0  m1 )
  ;set BBB-ig item 1 ( item 0  m1 )
  ;set CCC-ig item 2 ( item 0  m1 )
  ;]
  ]
  ]

  ask farmers [ if profit-diff >= 0 and profit-diff <= p25ig
    [set AAA-ig 0
     set BBB-ig 0
     set CCC-ig 0
    ]
  ]

   ;; I trunt the following codes off as switching to irrigation should not give you losse
  ;ask farmers [ if profit-diff < 0 and profit-diff > n75ig
   ; [set AAA-ig 0
    ; set BBB-ig 0
     ;set CCC-ig 0
    ;]
  ;]

  ask farmers [ set delta-c-ig-profit   ( AAA-ig * ( profit-diff ^ 2 ) + BBB-ig * ( profit-diff ) + CCC-ig )  * wrd * ( Hectares-tot - implemented-c-ig-tot-farmer )  ]
  ;;formulation for delta-c-ig-future, the difference with the previous formulation is the crop price for the future.
  ;;this variable is randomly selected.
  ask farmers [ set profit-rf-future  (random-float 1) * ( ( crop-price-rf  * cy-rf  ) - cost-total-rf )] ;; ( crop-price-Specialtycrops * ( 1 - ( random-float 0.4 - 0.25 ) ) * cy-Specialtycrops  ) - cost-total-Specialtycrops ]
  ;random-seed 100
  ask farmers [ set profit-ig-future  (random-float 1) *( ( crop-price-ig  * cy-ig  ) - cost-total-ig )   ] ;(random-float 1) *(( (random-normal 496 1)  * cy-ig ) - (random-normal 694 1))
  ask farmers [ set profit-diff-future profit-ig-future - profit-rf-future ]
  ;;calculating the historical positive and negative profit-diff-list for future
  ask farmers [
    ifelse  ( profit-diff-future >= 0 ) [ set p-list-ig-future insert-item ( length p-list-ig-future ) p-list-ig-future profit-diff-future ] [ set n-list-ig-future insert-item ( length n-list-ig-future ) n-list-ig-future profit-diff-future ]
  ]
  ;; calculating the quartiles
  ask farmers [ set p25ig lower-quartile p-list-ig-future ]
  ask farmers [ set p50ig median p-list-ig-future ]
  ask farmers [ set p75ig upper-quartile p-list-ig-future ]

  ask farmers [ set n25ig lower-quartile n-list-ig-future ]
  ask farmers [ set n50ig median n-list-ig-future ]
  ask farmers [ set n75ig upper-quartile n-list-ig-future ]
  ;; equation solver
  if Year > 1 [
  ask farmers [ if  ( profit-diff-future >= 0 )
  [
  let AAf ( list  (list (p75ig ^ 2) p75ig 1) (list (p50ig ^ 2) p50ig 1)  (list (p25ig ^ 2) p25ig 1) )
  let Af matrix:from-row-list AAf
  let Bff matrix:from-row-list [[1][0.5][0]]
  ;;print matrix:solve A B
  let m1 matrix:to-column-list matrix:solve Af Bff
  set AAA-ig-f item 0 ( item 0  m1 )
  set BBB-ig-f item 1 ( item 0  m1 )
  set CCC-ig-f item 2 ( item 0  m1 )
  ]

      ;; I trunt the following codes off as switching to irrigation should not give you losse
 ; [
 ; let AAf ( list  (list (n75ig ^ 2) n75ig 1) (list (n50ig ^ 2) n50ig 1)  (list (n25ig ^ 2) n25ig 1) )
 ; let Af matrix:from-row-list AAf
  ;let Bff matrix:from-row-list [[0][0.5][1]]
  ;;print matrix:solve A B
  ;let m1 matrix:to-column-list matrix:solve Af Bff
  ;set AAA-ig-f item 0 ( item 0  m1 )
  ;set BBB-ig-f item 1 ( item 0  m1 )
  ;set CCC-ig-f item 2 ( item 0  m1 )
  ;]
  ]
  ]

  ask farmers [ if profit-diff >= 0 and profit-diff <= p25ig
    [set AAA-ig-f 0
     set BBB-ig-f 0
     set CCC-ig-f 0
    ]
  ]
;; I trunt the following codes off as switching to irrigation should not give you losse
;  ask farmers [ if profit-diff < 0 and profit-diff > n75ig
;    [set AAA-ig-f 0
 ;    set BBB-ig-f 0
 ;    set CCC-ig-f 0
 ;   ]
 ; ]

  ask farmers [ set delta-c-ig-future    ( AAA-ig-f * ( profit-diff-future ^ 2 ) + BBB-ig-f * ( profit-diff-future ) + CCC-ig-f ) * wrd * ( Hectares-tot - implemented-c-ig-tot-farmer ) ]
  ;;formulation for delta-c-ig-cons, maxchange is 1 based on the reference
  ask farmers [ set delta-c-ig-cons  wrd * ( Hectares-tot) ]
  ;;the potential conservation
  ask farmers [ set W-tot risk-aversion + future + profit + socio ]
  ask farmers [ set  d-ig round ( ( (risk-aversion / W-tot) * item ( Year - 1 ) implemented-c-ig-list + (future / W-tot) * ( item ( Year - 1 ) implemented-c-ig-list + delta-c-ig-future ) + (profit / W-tot) * ( item ( Year - 1 ) implemented-c-ig-list + delta-c-ig-profit ) +  (socio / W-tot) * delta-c-ig-socio ))]
  ;;who conssiders conservation beneficial?

  ask farmers with [ d-ig > 0 ] [
    set adopted-ig? true
    set color yellow
    set implemented-c-ig ifelse-value   ( Hectares-tot - implemented-c-ig-tot-farmer - d-ig > 0 )
      [ d-ig ]
      [ 0  ]
    ]


if Year > 1 [
    ifelse item ( Year - 1 ) implemented-c-ig-tot-list - item ( Year - 2 ) implemented-c-ig-tot-list = 0 [ ask farmers [set delta-c-ig-socio 0 ] ]
    [
    ask farmers [
     let c-socio sum [ item ( Year - 1 ) implemented-c-ig-list ] of in-link-neighbors
      set delta-c-ig-socio ( c-socio / (  item ( Year - 1 ) implemented-c-ig-tot-list - item ( Year - 2 ) implemented-c-ig-tot-list  ) ) * wrd *  Hectares-tot;( item ( Year - 1 ) implemented-c-ig-tot-list ) ) * item ( Year - 1 ) implemented-c-ig-list ] ]
    ]
    ]

  ]


 ; if Year > 1 [
  ;ask farmers ;with [ adopted-ig?]
   ; [ let c-socio  item ( Year - 1 ) implemented-c-tech-list
   ; ask link-neighbors [ set color yellow
    ;                     set adopted-tech? true
     ;   set delta-c-tech-socio ( c-socio / (  item ( Year - 1 ) implemented-c-tech-tot-list - item ( Year - 2 ) implemented-c-tech-tot-list  ) ) * Hectares-technology-tot]];( item ( Year - 1 ) implemented-c-tech-tot-list ) ) * item ( Year - 1 ) implemented-c-ig-list ] ]
 ; ]

 ;;make the list of c-ig, d-ig and implemented-c-ig for calling them in each time step
  ask farmers [ set implemented-c-ig-list insert-item ( length implemented-c-ig-list ) implemented-c-ig-list implemented-c-ig ]
  ask farmers [ set implemented-c-ig-tot-farmer (sum implemented-c-ig-list + ((a-0-ig) / farmer-num  )) ]
  ask farmers [ set implemented-c-ig-tot-list-farmer insert-item ( length implemented-c-ig-tot-list-farmer ) implemented-c-ig-tot-list-farmer implemented-c-ig-tot-farmer ]
  set implemented-c-ig-tot sum [  implemented-c-ig-tot-farmer ] of farmers
  set implemented-c-ig-tot-list insert-item ( length implemented-c-ig-tot-list ) implemented-c-ig-tot-list implemented-c-ig-tot

  if Year = 34 [ stop ]
end
;;-----------------------------------------------------------------------------policymaker agent-------------------------------------------------------
to Policymaker-Decision-Making


  set Year Year + 1
  ask policymakers  [
    set prediction predict-conflict best-strategy sublist history 0 memory-size
    set conflict? true
    set color red
    set coop-con 1

  ]

  ask policymaker 0 [
    set prediction predict-conflict best-strategy sublist history 0 memory-size
    set conflict? false
    set color green
    set coop-con 1

  ]

  ask policymaker 0 [
     ; agricultural and hydropower have additivie effects, while other factors have multiplicative effects
     ; Weighted Geometric Mean
     set coop ( ( beta1 *  ( fp-tot  / tot-fc)   + beta2 * (    hp / pec  )    +  beta3 * (dc-ET / pdc-ET) ) ^ alpha1)  *  ((1 - ps) ^ alpha2) * ((1 - fdi) ^ alpha3) * ((1 - prediction) ^ alpha4)
    ; set wc (exp (5 * coop)) / (exp (5 * coop) + exp (5 * (1 - coop)) )
  ]

  ask policymaker 1 [
     ; agricultural and hydropower have additivie effects, while other factors have multiplicative effects
     ; Weighted Geometric Mean
     set coop   ( (beta1 *   ( tot-fc / fp-tot  )   + beta2 * ( pec / hp )    +  beta3 * (dc-ET / pdc-ET) ) ^ alpha1)  *  ((1 - ps) ^ alpha2) * ((1 - fdi) ^ alpha3) * ((1 - prediction) ^ alpha4)
    ; set wc (exp (5 * coop)) / (exp (5 * coop) + exp (5 * (1 - coop)) )
  ]

  ask policymaker 2 [
     ; agricultural and hydropower have additivie effects, while other factors have multiplicative effects
     ; Weighted Geometric Mean
     set coop ( ( beta1 *  ( tot-fc / fp-tot  )    ) ^ alpha1)  *  ((1 - ps) ^ alpha2) * ((1 - fdi) ^ alpha3) * ((1 - prediction) ^ alpha4)
    ; set wc (exp (5 * coop)) / (exp (5 * coop) + exp (5 * (1 - coop)) )
  ]


  ask policymakers [
     ifelse random-float 1.0 < wc
     [
     set conflict? false
     set coop-con 1
     set color green
     set shape "face happy"
    ]
     [
     set conflict? true
     set coop-con 0
     set color Red
     set shape "face sad"
    ]
  ]

  ask policymakers [
    ;set collective-conflict count other policymakers with [ shape = "face sad" ]
    set collective-conflict ( 2 - sum [coop] of other policymakers ) / 2

  ]

  ask policymaker 0 [
    ;ifelse (color  = green) and (collective-conflict <= 1)
    ifelse coop > 0.37
    [set delta  0.1
     set dc-ET dc-ET + gdc
    ]
    [set delta  0   ]
  ]

  ask policymakers [ set coop-list insert-item ( length coop-list ) coop-list coop ]
  ;; update the collective-conflict history
  ask policymakers  [set history fput collective-conflict but-last history]
  ;; have the agents decide what the new best strategy is
  ask policymakers  [ update-strategies ]


end

to Policymaker-Decision-Making-Sud


 ; ask policymaker 1 [
 ;   set prediction predict-conflict best-strategy sublist history 0 memory-size
 ;   set conflict? true
 ; ]


 ; ask policymaker 1 [
 ;    ; agricultural and hydropower have additivie effects, while other factors have multiplicative effects
 ;    ; Weighted Geometric Mean
 ;    set coop ( ( beta1 * ( 1 - min list 1 ( (a-ig * cy-ig  + a-rf * cy-rf ) / food-consumption ) )    + beta2 * (1 - min list 1 ( hp / pec ) )    +  beta3 * (1 - dc-ET / pdc-ET) ) ^ alpha1)  *  ((1 - ps) ^ alpha2) * ((1 - fdi) ^ alpha3) * ((1 - prediction) ^ alpha4)
 ;    set wc (exp (10 * coop)) / (exp (10 * coop) + exp (10 * (1 - coop)) )
 ; ]
 ; ask policymaker 0 [
 ;    ifelse random-float 1.0 < wc
 ;    [
 ;    set conflict? false
 ;    set coop-con 1
 ;    set color green
 ;    set shape "face happy"
 ;   ]
 ;    [
 ;    set conflict? true
 ;    set coop-con 0
 ;    set color Red
 ;    set shape "face sad"
 ;   ]
 ; ]

 ; ask policymaker 0 [
 ;   set collective-conflict count other policymakers with [ shape = "face sad" ]
 ; ]

 ; ask policymaker 0 [
 ;   ifelse (color  = green) and (collective-conflict <= 1)
 ;   [set delta  0.1
 ;    set dc-ET dc-ET + gdc
 ;   ]
 ;   [set delta  0   ]
 ; ]

  ;; update the collective-conflict history
 ; ask policymaker 0 [set history fput collective-conflict but-last history]
  ;; have the agents decide what the new best strategy is
 ; ask policymaker 0 [ update-strategies ]


end


;;-------------------------------------------------------------------quartiles----------------------------------------------------------
to-report upper-quartile [ xs ]
  let med median xs
  let upper filter [ x -> x > med ] xs
  report ifelse-value (empty? upper) [ med ] [ median upper ]
end

to-report lower-quartile [ xs ]
  let med median xs
  let lower filter [ x -> x < med ] xs
  report ifelse-value (empty? lower) [ med ] [ median lower ]
end

;;-------------------------------------------------------------------MSE----------------------------------------------------------

to-report compute-RMSE [ series1 series2 ]
  let l1 length series1
  let l2 length series2
  let l min (list l1 l2)

  let minlist []
  let maxlist []

  if (l = l1) [ set minlist series1 set maxlist series2 ]
  if (l = l2) [ set minlist series2 set maxlist series1 ]

  set maxlist sublist maxlist 0 l

  let sqd-errors (map [ [?1 ?2] -> (abs ?1 - ?2) ^ 2 ] maxlist minlist)
  let mse sqrt((sum sqd-errors) / l)

  report mse
end

;; determines which strategy would have predicted the best results had it been used this round.
;; the best strategy is the one that has the sum of smallest difference between the
;; current and the predicted conflict for each of the preceding
;; years (going back MEMORY-SIZE years)

to update-strategies
  let best-score memory-size * policymaker-num
  foreach strategies [ the-strategy ->
    let score 0
    let years 1
    repeat memory-size [
      set prediction predict-conflict the-strategy sublist history years (years + memory-size)
      ;; increment the score by the difference between this year's conflict and your prediction for this year
      set score score + abs (item (years - 1) history - prediction)
      set years years + 1
    ]
    if (score <= best-score) [
      set best-score score
      set best-strategy the-strategy
    ]
  ]
end

to move-to-empty-one-of [locations]  ;; turtle procedure
  move-to one-of locations
  while [any? other turtles-here] [
    move-to one-of locations
  ]
end
;; This reports an agent's prediction of the current conflict
;; using a particular strategy and portion of the conflict history.
;; More specifically, the strategy is then described by the formula
;; p(t) = x(t - 1) * a(t - 1) + x(t - 2) * a(t -2) +..
;;      ... + x(t - MEMORY-SIZE) * a(t - MEMORY-SIZE) + c * 3,
;; where p(t) is the prediction at time t, x(t) is the conflict at time t,
;; a(t) is the weight for time t, c is a constant, and MEMORY-SIZE is an external parameter.
to-report predict-conflict [strategy subhistory]
  ;; the first element of the strategy is the constant, c, in the prediction formula.
  ;; one can think of it as the the agent's prediction of the river basins's conflicts
  ;; in the absence of any other data
  ;; then we multiply each year in the history by its respective weight
  report ( ( (policymaker-num - 1) * first strategy + sum (map [ [weight years] -> years * weight ] butfirst strategy subhistory))) / (policymaker-num )

end
;; this reports a random strategy. a strategy is just a set of weights from 0 to 1.0 which
;; determines how much emphasis is put on each previous time period when making
;; an attendance prediction for the next time period
to-report random-strategy
  let list-strategy n-values (memory-size + 1) [random-float 1.0]
  report map [ x -> x / sum(list-strategy) ] list-strategy
end
@#$#@#$#@
GRAPHICS-WINDOW
414
29
882
498
-1
-1
13.94
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
Ticks
30.0

BUTTON
29
35
92
68
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
110
35
173
68
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
160
192
193
w-risk-aversion
w-risk-aversion
0
1
0.3
0.05
1
NIL
HORIZONTAL

SLIDER
20
207
192
240
w-future
w-future
0
1
0.65
0.05
1
NIL
HORIZONTAL

SLIDER
19
252
191
285
w-profit
w-profit
0
1
0.0
0.05
1
NIL
HORIZONTAL

BUTTON
30
81
93
114
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
299
192
332
w-socio
w-socio
0
1
0.3
0.05
1
NIL
HORIZONTAL

MONITOR
248
55
298
100
Month
month-n
17
1
11

MONITOR
297
55
354
100
Year
1983 + Year
17
1
11

SLIDER
20
344
192
377
mean-wrd
mean-wrd
0
100
0.95
0.01
1
NIL
HORIZONTAL

SLIDER
208
345
380
378
SD-wrd
SD-wrd
0
100
0.77
0.01
1
NIL
HORIZONTAL

SLIDER
206
161
380
194
SD-w-risk-aversion
SD-w-risk-aversion
0
100
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
207
208
379
241
SD-w-future
SD-w-future
0
100
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
207
252
379
285
SD-w-profit
SD-w-profit
0
100
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
208
299
380
332
SD-w-socio
SD-w-socio
0
100
0.1
0.01
1
NIL
HORIZONTAL

PLOT
900
30
1370
187
Willingness to cooperate_Ethiopia 
Time (Year)
Value (-)
1983.0
2016.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ask policymaker 0 [plotxy  Year + 1983 coop];wc];coop-con]\n;plotxy  Year + 1983  (a-rf-ET * cy-rf-ET ) / tot-fc-ET "

PLOT
900
530
1371
690
Cooperation-Conflict Dynamics
Time (Year)
Value (-)
1983.0
2016.0
0.0
1.0
true
false
"" ""
PENS
"Sudan" 1.0 0 -16777216 true "" "plotxy  Year + 1983   (1 / 3) * ( [coop] of policymaker 0 + [coop] of policymaker 1 + [coop] of policymaker 2) ;[((coop-con - (-1)) * (7 - (-7)) / (1 - (-1))) + (-7) ] of policymakers"

TEXTBOX
80
135
330
161
Decision-making Factors for Farmers
15
0.0
1

TEXTBOX
70
395
385
431
Decision-making Factors for Policymakers
15
0.0
1

PLOT
900
200
1370
355
Willingness to cooperate_Sudan
Time (Year)
Value (-)
1983.0
2016.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ask policymaker 1 [plotxy  Year + 1983 coop];wc];coop-con]"

PLOT
900
370
1370
520
Willingness to cooperate_Egypt
Time (Year)
Value (-)
1983.0
2016.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ask policymaker 2 [plotxy  Year + 1983 coop];wc];coop-con]"

PLOT
1385
435
1850
600
Egypt
year
memory
1983.0
2016.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ask policymaker 2 [plotxy  Year + 1983 1 - prediction];wc];coop-con]"

PLOT
1385
260
1850
430
Sudan
year
memory
1983.0
2016.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ask policymaker 1 [plotxy  Year + 1983 1 - prediction];wc];coop-con]"

PLOT
1385
80
1850
255
Ethiopia
year
memory
1983.0
2016.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ask policymaker 0 [plotxy  Year + 1983 1 - prediction];wc];coop-con]"

@#$#@#$#@
## Note for the contributer:

- Modify the price and cost for functions of future gross margins
- Note that the model should generate 56-element timeseries, but not it is 57

## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
1.0
    org.nlogo.sdm.gui.AggregateDrawing 7
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 168 102 30 30
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 310 159 30 30
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 1035 122 30 30
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 888 84 30 30
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 296 516 30 30
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 1009 528 30 30
        org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 895 456 30 30
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>;ask policymaker 0 [show coop]</metric>
    <enumeratedValueSet variable="SD-w-profit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SD-wrd">
      <value value="0.77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-profit">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-future">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SD-w-socio">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-strategies">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SD-w-risk-aversion">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-socio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w-risk-aversion">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-wrd">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SD-w-future">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
