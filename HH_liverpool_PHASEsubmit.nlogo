extensions [table csv rnd]

globals [
  N1 N2 N3 N4      ;; neighbourhoods (now only used for plots)
  FVStores         ;; Table with fruit and veg stores + their price and availability
  price-of-stores  ;; table with monetary price per individual meal in FV stores of different category
  low-rank         ;; "low income household ranking strategy" - table with shopping preferences
  high-rank        ;; "high income household ranking strategy"
;  ff-low-rank
;  ff-high-rank
]

patches-own [locale]

breed [ households household ]
breed [ shops shop ]

;; note: future iteration will import these data
households-own [
  ;; counters ================
  meals-skipped       ;; overall total
  meals-skipped-this-week
  ff-meals            ;; total
  fv-meals
  ;; ==================

  ;; location ===============
  FV-access    ; Table of FV outlets accessible, later populated with ranking and priorities
  FF-access    ; Table of FF outlets accessible
  neighborhood
  ff-density   ; density (categorical: low, medium, high) of fast food restaurants in agent's neighbourhood.
               ; Used to determine TEMPTATION

  Num-HH         ; number of people in HH
  class          ; 0 = low income; 1 = not low income
  travel-mode    ; 0 = no car; 1 = has car
  budget         ; monetary budget refilled once a week, differs by class
  FV-stock       ; Fruit and veg meals in the larder

  ;; Unused =======================
  ;Avg-daily-servings-FV
  ;AH-meal
  ;OH-meal
]

shops-own [
  shop-type             ;;  0 = Fast food;  1 = Fruit and veg
  category              ;;  FV Stores category:  1 = discount; 2 = corner; 3 = medium; 4 = large
  availability          ;;  Essentially the size of the shop
  price                 ;;  price (categorical) of FV store 0 = low; 1 = medium; 2 = high
  monetary-price        ;;  price ("monetary") of FV store

  visits                ;;  footfall
  customer-orig         ;;  list holding the neighbourhood of origin of customers. Used to determine where a new store opens
  earnings              ;;  sum of money spent by all customers. Reset every year
  location
]

;; ===============================================================
;; ====== Model config ===========================================

;; We define a number of lookup tables with prices and preferences by store and household type
;; this makes it easier to alter the model

to define-FV-stores
  ; monetary price per individual meal in FV stores of different price category
  ; FV-store price cat:Low   Mid   High
  set price-of-stores  [1    1.5    2]

  ;; this table defines price and availability of the 4 FV store categories
  ;; it is used in function create-shop below.
  set FVStores table:make
                  ;cat | price | availability
  table:put FVStores 1 [ 0       1 ] ;; discount store
  table:put FVStores 2 [ 2       0 ] ;; corner shop
  table:put FVStores 3 [ 2       1 ] ;; medium store
  table:put FVStores 4 [ 1       2 ] ;; large store
end


;; This table defines the way households of different types pick the FV store
;; when they go shopping. Low income households always prefer low price over
;; availability. Non-low income households prefer higher availability
;; In this version we use "availability" which is essentially the size of a store,
;; as a factor. Before we were using distance. Distance is now factored in via access to car
to define-preferences
  set low-rank table:make
  set high-rank table:make

  ;; ==================================

  ;;   SHOP RANKING SYSTEM - preference among accessible shops
  ;;   by price and availability per household income.

  ;; ==================================

  ;; Low income housholds
                               ;; PRICE
          ; availability  low  mid  high
  table:put low-rank 2  [  1    4    7  ]
  table:put low-rank 1  [  2    5    8  ]
  table:put low-rank 0  [  3    6    9  ]

  ;; High income housholds         price
          ; availability  low  mid  high
  table:put high-rank 2 [  1    2    3  ]
  table:put high-rank 1 [  4    5    6  ]
  table:put high-rank 0 [  7    8    9  ]
end

;;; Possibly needed to make ppl decide between fast food or a meal at home.
;;; based on fast food density and price of FV. Currently unused...

;to define-food-source-rank
;  set ff-low-rank table:make
;  set ff-high-rank table:make
;
;  ;; ==================================
;
;  ;;   Food Source Decision Rank
;
;  ;; ==================================
;
;  ;; Low income housholds
;                               ;; PRICE AH Meal
;                ; FF dens       ;low ;mid ;high
;  table:put ff-low-rank "low"  [  1    4    7  ]
;  table:put ff-low-rank "mid"  [  2    5    8  ]
;  table:put ff-low-rank "high" [  3    6    9  ]
;
;  ;; STILL TO SORT - MED INCOME HH
;
;  ;; High income housholds
;                             ;; PRICE AH Meal
;                ; FF dens        ;low ;mid ;high
;  table:put ff-high-rank "low"  [  7    3    1  ]
;  table:put ff-high-rank "mid"  [  8    4    2  ]
;  table:put ff-high-rank "high" [  9    6    9  ]
;end

;; ======================================================

;;             Model setup

;; ======================================================

to setup
  ca
  define-preferences
  setup-patches

  define-FV-stores
  import-food-scenario scenario

  create-hh
  reset-ticks
end

to import-food-scenario [num]
  foreach csv:from-file (word "scenario_" num ".csv") [ neigh ->
    let loc item 0 neigh
    if is-number? loc [
      foreach (range 1 5) [cat ->  ;; cat = FV store category: 1 = discount; 2 = corner; 3 = medium; 4 = large
        output-show (word "Creating " item cat neigh " stores of category " cat " in area " item 0 neigh)
        ask n-of item cat neigh patches with [locale = loc] [
          create-shop cat
        ]
      ]
      ;; These are the FF
      output-show (word "Creating " item 5 neigh " fastfood stores in area " item 0 neigh)
      ask n-of item 5 neigh patches with [locale = loc] [create-shop 0]
    ]
  ]
end

to setup-patches
  ask patches with [pycor < 0 and pxcor < 0][
    set locale 1
    set pcolor violet
  ]

  ;ask patches with [ pycor < 0 and pxcor < 0 ] [ set pcolor violet ]
  ;; low
  ask patches with [pycor > 0 and pxcor > 0][
    set locale 2
    set pcolor sky
  ]
  ;; low
  ask patches with [pycor < 0 and pxcor > 0 ][
    set locale 3
    set pcolor yellow
  ]

  ;; mixed
  ask patches with [pycor > 0 and pxcor < 0 ][
    set locale 4
    set pcolor orange + 1 + 1
  ]
  set N1 patches with [locale = 1]
  set N2 patches with [locale = 2]
  set N3 patches with [locale = 3]
  set N4 patches with [locale = 4]
end

to create-hh
  ;; create our households
  ;;; set a population number based on a toggle on the GUI
  ask patches [if pcolor != black [sprout-households random 2]]

  ;;create high-income neighborhood
  ask households [
    ifelse pcolor = violet [
      set class ifelse-value random-float 1 > 0.2 [1][0]  ;; here we have 80% class 1 (high-income HH) and 20% class 0 (low-income HH)
      get-paid
      ; set AH-meal random-float 1
      set travel-mode 1 ;everyone has a car or access to a car
                        ; set interval-FV-shops 7 + random 14
      set Num-HH 1 + random 4
      ; set an array/list? of the stores in the neighborhood as accessible.
      ;; function of distance and HH travel mode
      ;;
      ;set shop-access [ shop ] of shops with pcolor = violet
      ;; set shop-access
    ] [
      ;;create low-income neighborhood
      ifelse pcolor = sky or pcolor = yellow [
      set class ifelse-value random-float 1 > 0.8 [1][0] ;; here we have 20% class 1 (high-income HH) and 80% class 0 (low-income HH)
      get-paid
      ; set AH-meal 0
      set travel-mode 0 ; no one has a car
                        ; set interval-FV-shops 1 + random 8
      set Num-HH 1 + random 4
      ][

        ;;create a mixed neighborhood
        if pcolor = 27 [
          ; set AH-meal random-float 1 * 0.5
          set class ifelse-value random-float 1 >= 0.5 [1][0]   ;; here we have 50% class 1 (high-income HH) and 50% class 0 (low-income HH)
          set travel-mode random 2 ; mixed
                                   ;  set interval-FV-shops 1 + random 12
          set Num-HH 1 + random 4
          ; set Avg-daily-servings-FV Num-HH * random-float 3.4 ;;how to make this realistic, we know avg intake...and we want to randomize a bit! Good place to practice.
        ]
      ]
    ]
    set neighborhood [locale] of patch-here
    set color ifelse-value class = 1 [green][white]
    assign-shops
  ]
  ; compute-density-tertiles
end


to create-shop [cat]
  ;; cat: 0 = FF; 1 = discount; 2 = corner; 3 = medium; 4 = large
  sprout-shops 1 [
    set customer-orig []
    set location [locale] of patch-here
    set category cat
    ifelse cat > 0 [
      set shop-type 1 ;; shop-type 1 is a FV store
      set price item 0 table:get FVStores cat  ;; we get the price category from the table created in define-FV-stores
      set availability item 1 table:get FVStores cat  ;; we get availability from the table in define-FV-stores
      adjust-shape-and-colour
      set-monetary-price
    ][
      ;; this is a Fast food store. Price is fixed and always the same.
      ;; We set it to the equivalent of a FV meal purchased in the low price store
      set monetary-price random-normal item 0 price-of-stores 0.1
      set shop-type 0 ;; shop-type 0 is a FF store
      set shape "house"
      set color black
    ]
  ]
end

to go
  if ticks > 1 and ticks mod 365 = 0 [
    ;; every year stores decide to change price, stay the same or close
    if stores-adjust = 1 [ask households [assign-shops]]
    ask shops [
      set earnings 0
      set visits 0
      set customer-orig []
    ]
  ]

  ;; Weekly, households set their budgets and we reset meals skipped in a week
  if ticks mod 7 = 0 [
    ask households [
      get-paid
      set meals-skipped-this-week 0
    ]
  ]

  ;; FUNCTION 2: Decide to shop
  ;; Daily (per tick) if we don't have enough to feed everybody in the hh for one day
  ;; we go and buy groceries as long as we have budget to shop.
  ;; Future iteration to include other constrains in this. i.e. we might have money but not time/will to go buy FV.
  ask households [
    if FV-Stock < (4 * Num-HH) and budget > 0 [purchase-from choose-store "FV"]
  ]

  ;; We eat 3 times a day and every time we choose either out of home or in home meals
  ;; We could only eat 2x per day or also consider that certain meals are more or less likely eaten out.
  repeat 3 [ask households [choose-food-source]]

  tick  ;; one tick is one day - in principle
end

to get-paid
  ;; We assume that less wealthy HH receive a (normally distributed) income sufficient to buy a meal a day
  ;; from the low price FV store for the whole household. Wealthier HH receive income
  ;; sufficient to buy a meal a day from the medium ranked FV shop
  set budget budget + ifelse-value class = 0
  [random-normal ( ((1 + Num-HH) * 24) * (item 0 price-of-stores)) 1.2]
  [random-normal ( ((1 + Num-HH) * 24) * (item 1 price-of-stores)) 1.2]
end

to set-monetary-price
  ;; we translate the categorical price attribute into an actual sum that the shop charges
  ;; 0 = low price 1 = mid price 2 = high price
  ifelse price = 0 [set monetary-price FVPriceInflation * (random-normal item 0 price-of-stores 0.1)][
    ifelse price = 1 [set monetary-price FVPriceInflation * (random-normal item 1 price-of-stores 0.1)]
      [set monetary-price FVPriceInflation * (random-normal item 2 price-of-stores 0.1)]
  ]
end

;; FUNCTION 1 Identify accessible food outlets for shopping
to assign-shops
  set FV-access table:make
  set FF-access table:make
  let maxdist 5
  if travel-mode = 1 [set maxdist 10] ; no car: max distance 5; with car: max distance 10

  ;; First we work out how many FF we have around us...
  let ff-near shops in-radius maxdist with [shop-type = 0]
  if not any? ff-near [set ff-near min-n-of 1 shops with [shop-type = 0][distance myself]]

  ;; then we enter them in a table with distance
  ask ff-near [
    let me who
    let dist distance myself
    ask myself [
      table:put FF-access me dist
    ]
  ]

  set FV-access table:make
  set ff-density count ff-near / count patches in-radius maxdist

  ;; Then we do the same with FV shops
  let close-shops shops in-radius maxdist with [shop-type = 1]
  ifelse count close-shops = 0
  ;; If the hh does not have FV shops in proximity we give them the closest (with a low rank: rank 6!)
  [table:put FV-access [who] of min-one-of shops with [shop-type = 1] [distance myself] 6 ]
  [
    let thishop nobody
    ask close-shops [
      set thishop self
      let avail availability
      ask myself [
        let table low-rank
        if class = 1 [set table high-rank]
        table:put FV-access [who] of thishop (item [price] of thishop table:get table avail)
      ]
    ]
  ]
end

to compute-density-tertiles
  let maxdens max [ff-density] of households
  ask households [
    ifelse ff-density < maxdens * 1 / 3 [set ff-density "low"][
      ifelse ff-density >= maxdens * 1 / 3 and ff-density < maxdens * 2 / 3 [set ff-density "mid"][set ff-density "high"]
    ]
  ]
end


;; FUNCTION 3: Choosing a FV Outlet or a FF Outlet
to-report choose-store [tp]
  ifelse tp = "FV" [
  ;; using RND extension, reports one store with probability of being selected proportional to its "weight"
  ;; The weight is the inverse of the store's rank. Function that favors lower score but element of randomness.
  report shop rnd:weighted-one-of-list table:keys FV-access [ [s] -> (10 - table:get FV-access s) ]
  ]
  [  set ff-meals ff-meals + 1
    ; choose a random accessible FF outlet - we do not weight anything here
    report shop one-of table:keys FF-access
  ]
end

;; FUNCTION 4: Acquire FV from outlet
to purchase-from [store]
  let tp [shop-type] of store
  let myorig neighborhood
  let cost [monetary-price] of store ; price of store
  let how-much-we-can-afford budget / cost ; we can afford a certain amount of servings in this store

  ; how much the HH wants to buy: enough for three times a day for 10 days if we're shopping for FV, one meal each member if we're in a FF
  let how-much-we-want ifelse-value tp = 0 [num-hh][(num-hh * 3 * 10)]

  ;; we only buy what we can afford
  if how-much-we-can-afford < how-much-we-want [set how-much-we-want how-much-we-can-afford]

  ; spending based on price of store and amount purchased
  let spent cost * how-much-we-want
  set budget budget - spent ; update budget

  if tp = 1 [set FV-Stock FV-Stock + how-much-we-want] ;add to your FV stock

  ask store [
    set visits visits + 1 ; add visits to store
    set earnings earnings + spent ; add amount spent to store
    set customer-orig fput myorig customer-orig ; the shop keeps track of where its customers are from
  ]
end


;; FUNCTION 5: Choose a food source for each meal
to choose-food-source
  ;; If we have FV at home we consume it, but we can decide to go to
  ;; FF outlet anyway based on density of FF
  ifelse FV-stock >= num-HH [
    ;; decision: go to FF or consume FV stock
    ;; f(ff-density, price-of-fv-meal / price-of-ff-meal)

    ;; This is an absurdly silly and nonsensical way of making the ff-density
    ;; count in the decision of where to eat. Needs a proper function
    ;; eventually, this will not be random and we can weight this based on household survey
    let ff-weight 5
    ; if class = 0 [set ff-weight 12]
    let prob-ff ff-density * ff-weight
    ;; here, if the probability of FF consumption is more than 1,
    ifelse random-float 1 < prob-ff
          [purchase-from choose-store "FF"]
          [eat-FV]
  ]
  ;; if FV stock is low (less than the number of people in household)
  ;;  and we have some money we go out
  [ifelse budget > 0
    [purchase-from choose-store "FF"]
    ;; if the budget is too low or zero we skip a meal
    [set meals-skipped meals-skipped + 1
    set meals-skipped-this-week meals-skipped-this-week + 1
    ]
  ]
end

;; FUNCTION 5.1 Consumption of a FV meal
to eat-FV ; this happens after choosing a food source
; if eating an at-home-meal, reduce stock of FV
  set fv-meals fv-meals + 1
  set FV-stock FV-stock - num-hh
end


;; STORES DECIDE TO CLOSE/OPEN/Change Price
;; Iterate over all store types, categories and prices.
;; Stores compare their earnings against the median of all other stores with the same characteristics to determine
;; whether they need to lower their price, close, or, if they are doing well, sprout another identical store.
;; The function returns 1 if any change has happened, to signal that agents need to recompute their accessible stores.
to-report stores-adjust
  let changes 0
  foreach range 2 [stype ->
    foreach range 3 [sprice ->
      foreach range 5 [cat ->
        let these shops with [shop-type = stype and price = sprice and category = cat]
        if any? these [
          let median-earnings median [earnings] of these
          let closure-threshold median-earnings * thres-closing
          let opening-threshold median-earnings * thres-opening
          ; show (word "closing threshold of shop type " stype " is: " closure-threshold)
          ; show (word "opening threshold of shop type " stype " is: " opening-threshold)
          ask these with [earnings <= closure-threshold][
            set changes 1
            ifelse price > 0 [
              let newprice price - 1
              output-print (word "Year " (ticks / 365) " - shop " who " (type " shop-type "; price " price "; category " category "; earnings: " round earnings "; area: " location ") is lowering its price to " newprice)
              set price newprice
              set-monetary-price
              adjust-shape-and-colour
            ][
              output-print (word "Year " (ticks / 365) " - shop " who " (type " shop-type "; price " price "; category " category "; earnings: " round earnings "; area: " location ") is closing")
              die
            ]
          ]
          ask these with [earnings >= opening-threshold][
            let orig who
            let money earnings
            set changes 1
            let where one-of modes customer-orig
            hatch 1 [
              set visits 0
              set earnings 0
              set customer-orig []
              move-to one-of patches with [locale = where]
              set location [locale] of patch-here
              output-print
              (word "Year " (ticks / 365) " - shop " orig " (type " shop-type "; price " price "; category " category "; earnings: " round money "; area: " location ") makes money. New shop: " who " in area " where)
            ]
          ]
        ]
      ]
    ]
  ]
  report changes
end

to adjust-shape-and-colour
  ;; shops are represented as houses. The colour represents the price,
  ;; the size represents availability: larger stores are... larger stores.
  set shape "house"
  ;; 0 = low price (grey); 1 = mid price (red); 2 = high price (green)
  ifelse price = 0 [set color grey][ifelse price = 1 [set color red][set color green]]
  set size availability + 1
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
444
445
-1
-1
12.91
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
ticks
30.0

BUTTON
11
632
77
665
setup
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
80
633
147
666
NIL
go\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
450
10
823
260
Median meals at home per day
NIL
NIL
0.0
10.0
1.0
3.0
true
true
"" ""
PENS
"Low Income" 1.0 0 -16777216 true "" "plot median [fv-meals] of households with [class = 0] / ticks"
"Not Low Income" 1.0 0 -2674135 true "" "plot median [fv-meals] of households with [class = 1] / ticks"
"N1" 1.0 0 -8630108 true "" "plot median [fv-meals] of households-on N1 / ticks"
"N2" 1.0 0 -13791810 true "" "plot median [fv-meals] of households-on N2 / ticks"
"N3" 1.0 0 -1184463 true "" "plot median [fv-meals] of households-on N3 / ticks"
"N4" 1.0 0 -612749 true "" "plot median [fv-meals] of households-on N4 / ticks"

PLOT
450
262
823
497
Median grocery stock by income
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Low income" 1.0 0 -16777216 true "" "plot median [FV-Stock] of households with [class = 0 ]"
"Not low income" 1.0 0 -2674135 true "" "plot median [FV-Stock] of households with [class = 1 ]"

PLOT
11
454
445
626
Households who skipped at least one meal this week
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Low income" 1.0 0 -16777216 true "" "plot count households with [class = 0 and meals-skipped-this-week > 0] / count households with [class = 0]"
"Not low income" 1.0 0 -2674135 true "" "plot count households with [class = 1 and meals-skipped-this-week > 0] / count households with [class = 1]"

MONITOR
348
567
436
612
meals skipped
sum [meals-skipped] of households
1
1
11

SLIDER
9
668
227
701
thres-closing
thres-closing
0
1
0.6
0.01
1
* median
HORIZONTAL

SLIDER
228
668
446
701
thres-opening
thres-opening
1
3
1.6
0.01
1
* median
HORIZONTAL

OUTPUT
450
502
1156
719
13

SLIDER
147
633
295
666
FVPriceInflation
FVPriceInflation
0.1
5
1.5
0.01
1
NIL
HORIZONTAL

PLOT
826
327
1077
497
proportion of FV stores
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"N1" 1.0 0 -8630108 true "" "plot count (shops-on N1) with [shop-type = 1] / count (shops-on N1) "
"N2" 1.0 0 -13791810 true "" "plot count (shops-on N2) with [shop-type = 1] / count (shops-on N2) "
"N3" 1.0 0 -1184463 true "" "plot count (shops-on N3) with [shop-type = 1] / count (shops-on N3) "
"N4" 1.0 0 -612749 true "" "plot count (shops-on N4) with [shop-type = 1] / count (shops-on N4)"

PLOT
1078
327
1331
498
Median price - FV Stores
NIL
NIL
0.0
10.0
0.0
2.5
true
true
"" ""
PENS
"median" 1.0 0 -16777216 true "" "plot median [monetary-price] of shops with [shop-type = 1]"
"Low" 1.0 0 -7500403 true "" "plot median [monetary-price] of shops with [shop-type = 1 and price = 0]"
"Medium" 1.0 0 -2674135 true "" "plot median [monetary-price] of shops with [shop-type = 1 and price = 1]"
"High" 1.0 0 -10899396 true "" "plot median [monetary-price] of shops with [shop-type = 1 and price = 2]"

SWITCH
827
12
970
45
food-pantries
food-pantries
1
1
-1000

CHOOSER
828
48
920
93
scenario
scenario
1 2 3 4
1

PLOT
826
145
1081
325
Meals breakdown (low income)
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Home" 1.0 0 -16777216 true "" "plot mean [fv-meals] of households with [class = 0] / ticks / 3"
"Out" 1.0 0 -7500403 true "" "plot mean [ff-meals] of households with [class = 0] / ticks / 3"
"SKIP" 1.0 0 -2674135 true "" "plot mean [meals-skipped] of households with [class = 0] / ticks / 3"

PLOT
1082
146
1338
326
Meals breakdown (non low income)
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Home" 1.0 0 -16777216 true "" "plot median [fv-meals] of households with [class = 1] / ticks / 3"
"Out" 1.0 0 -7500403 true "" "plot median [ff-meals] of households with [class = 1] / ticks / 3"
"SKIP" 1.0 0 -2674135 true "" "plot median [meals-skipped] of households with [class = 1] / ticks / 3"

@#$#@#$#@
TODO: 
income of low income = 14 servings a week from the low price store 1 serving per tick from low price.


DISTRIBUTION: income low / non low.
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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
0
@#$#@#$#@
