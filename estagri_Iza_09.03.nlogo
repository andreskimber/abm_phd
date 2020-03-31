;; Estonian Late Stone Age - Early Iron Age (2900 BC - 450 AD) agriculture model
;; Author: Andres Kimber
;; version:
;; date:
;; Netlogo version: 6.1.1
;; data necessary to run it:

; Currently farms are not so much settlements, but more like farming plots, that need to be moved. for now at least keep as is.
; Should add maybe a breed of agents that are settlements that try to have fields near them and move, when no more are available

extensions [
  csv
  profiler
]
breed [
  farms farm
]
globals [  ;; IR: I like the declarations of data type you did for bool, you should do it for all globals, use: bool, int, float, string, list, turtleset, patchset etc
  new-farms-number          ; integer. number of farms to be created in growth-marry procedure
  population                ; comments
  population2               ; comments
  population3               ; comments...
  population3-remainder
  available-patches         ; patches that are (1) suitable for agriculture, (2) not occupied and (3) not in fallow
  fallowing-patches         ; list of patches that are in fallow
  year                      ;
  open-reveals              ; proportion of open land from REVEALS model
  open-reveals-value        ; open-reveals reported as list and this transforms it into a single value
  forest-reveals
  forest-reveals-value
  agrarian-reveals
  agrarian-reveals-value
  no-room-death
  all-harvested-patches     ; list of number of harvested patches at the end of farms life. for calculating mean number of patches used by an agent during its lifetimes
  mean-required-patches
  nofood-death              ; number of agents who have died because of no food
  age-death                 ; number of agents who have died because of old age
  age-at-death              ; age of the agent when it dies
  pop                       ; list. number of farms during current and previous tick
  pop-growth-rate           ; pop growth rate during current tick
  pop-growth-rate-list      ; list of pop growth rates of all ticks
  pop-density               ; population density of people per square kilometer (per/km2) during current tick
  pop-density-list          ; list of pop density of all ticks
  fertile-farms             ; farms that have enough food and correct age for sending out and offspring to make new farm. not a good name. their are no giving birth to children, but marrying them off
  usable-patches
  suitable-patches          ; patches with good enough yield-multiplier to food a farm
]

patches-own [
  occupied                  ; Boolean. records if there are any turtles on the patch
  availability              ; Boolean. TRUE if occupied and fallow FALSE
  occupation-frequency      ; how many times has the resource of patch been harvested on
  years-of-fallowing        ; years how long the patch has been in fallow
  in-fallow                 ; is the patch in fallow at the moment
  original-color            ; the original color of a patch
  land-cover-type           ; defines land use type (forest, open land, field) for each patch.
  yield-multiplier          ; how many seeds you get from sowing one seed
  original-yield-multiplier ; the initial yield multiplier before farming
  ;sown-seed                ; amount of seed (kg) that has been sown on the patch
  soil-suitability          ; suitability of the soil for agriculture. Based on soil map and relevant references in general_notes.md
  year-of-harvest           ; number years the patch has been harvested in a row
  required-fallow           ; number of years required for a patch to recover its initial fertility. will grow by one year with every fallow period. just testing (21.01.2020)
  drag                      ; comments...
]

farms-own [
  storage                   ; integer. amount of grain currently owned by a farm; should remove old storage (like in anasazi model)?
  max-storage               ; integer. maximum amount of grain a farm can have stored. is it particularly necessary?
  nofood                    ; integer. how many ticks have they been without food
  age                       ; integer. age of a farm
  moves                     ; number of times the farm has changed location
  harvested-patches         ; a list of all patches where the farm has harvested. farm will prefer to harvest patches it has harvested before
  ;best-patches
  people                    ; integer. number of people in a farm.
  grain-need                ; farms' yearly need of grain in kg. caloric need + next years seed
  seed                      ; grain in kg needed for sowing. part of storage
  sown-seed                 ; amount of seed that was sown
  crop                      ; amount of grain (kg) that the farm harvested last time
  offspring                 ; how many farms has this farm produced
  reachable-patches         ; patches in their "walking" distance
  my-usable-patches         ; patches that are reachable and available
  distance-to-patches       ;; comments...
  nearby
  dist
  target-patch
]


to setup
  ca
  reset-ticks
  file-close-all
  ifelse soils? [ make-soils ] [ make-plain ] ; dependent on the switch, patches have either 4 different type of soils or are homogenous
  create-farms initial-number-farms [ ; teha eraldi funktsiooniks
    set shape "house"
    set size 1
    set storage 0
    set age 15 + random 15
    setxy random max-pxcor + 1 random max-pycor + 1
    set offspring 0
    set harvested-patches []
    set people people-per-farm ; one-of (range 4 10) ; based on historical and archaeological data there were 4-9 people in a household/settlement
  ]
  ask farms [
    calculate-grain-need
    set storage grain-need
   ; record-occupation ; not necessary ; AK: necessary for sort-available-patches, but could be replaced there.
    set age-at-death [] ; replaced check-death
  ]
  setup-available-patches
  visualise
  reset-ticks
  set year -2900
  report-reveals-data
  set all-harvested-patches []
  set pop []
  set pop-growth-rate-list []
  set pop-density-list []
  set pop-density ( count farms * people-per-farm ) /  (( count patches * 3 ) / 100 ) ; people per km2. patch size is 3 ha.
  ; pop-density = ( number of farms * number of people in a farm ) / (( number of patches * 3 = number of hectares because one patch is 3 ha )  / 100 (because there are 100 h in 1 km2) )
  set pop-density-list lput pop-density pop-density-list
  set fallowing-patches []
  set fertile-farms []
end

to go
  if not any? turtles OR year = 450 [ ; OR no-room-death > 5 ; OR [yield-multiplier] of last available-patches != 6
    stop
  ]
;  if year = -1600 [ set pop-limit pop-limit * 3 ] ; just testing (26.01.2020) to see if rapid changes in population affect land cover, and result in validation data
;  if year = -1300 [ set pop-limit pop-limit * 2 ]
;  if year = -1100 [ set pop-limit pop-limit * 2 ]
  ask turtles [
    check-death
    calculate-grain-need
    ;move-farm
    move-farm2
    harvest-farm
    eat
    record-occupation ; check if still necessary function (18.02.20)
  ]
  fallow
  sort-available-patches ; used here for sorting the list.
  visualise
  pop-growth
  calculate-pop-density
  increase-calories-from-grain
  report-reveals-data
  set year year + 1
 ; ask patch 45 12 [print yield-multiplier] ;; Testing
  tick
end

;;; Farm procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to harvest-farm
  ifelse storage > seed [
    set storage storage - seed
    set sown-seed seed
  ][
    set sown-seed storage ; if there is not enough seed then will sow as much as there is
    set storage 0
  ]
  ; not add randomness at the moment (16.12). Find parameter values that result in  outcome for model
  let crop-simple sown-seed * [ yield-multiplier ] of patch-here ; initial calculation of crop
  ; actual crop will be a random value from a normal distribution with a mean of "crop" and standard deviation of 25% of "crop". This is due to weather variability. From Baum 2016, table 3. Weather based variability could be same for all agents.
  ;set crop round ( random-normal crop-simple ( 0.25 * crop-simple ) )
  set storage storage + crop-simple
  if storage > max-storage [ set storage max-storage ]
  if member? patch-here harvested-patches = FALSE [
    set harvested-patches lput patch-here harvested-patches ; for farming memory
  ]
  ;set harvested-patches remove-duplicates harvested-patches ; could combined with adding new item. or not add if current patch is already present in list.
  ask patch-here [
    set land-cover-type "agrarian"
    set year-of-harvest year-of-harvest + 1
    define-soil-fertility
  ]
end

to eat
  let food grain-need - seed
  ifelse storage >= grain-need - seed [
    set storage storage - ( grain-need - seed ) ; if farm has enough storage it will eat it yearly caloric need and wont eat the next years' seed
  ][
    ifelse storage >= grain-need [
      set storage storage - grain-need ; if farms does not have enough storage it will also eat its next years' seed
    ][
      set storage 0 ; if farms' storage is lower than grain-need it will eat everythin it has
    ]
  ]
end
to eat2
  ; I'm trying to figure it out here
  ifelse storage > grain-need
  [set storage storage - grain-need
    if storage < seed [set seed seed - storage]
  ]

  [set storage 0]
end

to setup-available-patches
  set available-patches []
   ask patches with [soil-suitability != "Not suitable"] [ ; adding only patches that have suitability for agriculture
      if occupied = FALSE AND in-fallow = FALSE  [ ; AND soil-suitability != "Not suitable" ; think through the soil suitability thing
       set available-patches lput self available-patches
     ]
   ]
    set available-patches sort-by [ [ a b ] -> [ yield-multiplier ] of a < [ yield-multiplier ] of b ] available-patches
end

to sort-available-patches ; should change to sort-available-patches
  ; at setup, create an empty list called available-patches and add into it all patches that are suitable for agriculture and not in fallow or occupied.
  ; Then sort the list in an increasing order based on value of multiplier of patches.
  ; at every tick sort the patches again.
   ; sorts patches in an increasing order based on value of multiplier
    ; list should be shortened for model speed. Could be done by selecting random 20% values from the list.
    set available-patches sort-by [ [ a b ] -> [ yield-multiplier ] of a < [ yield-multiplier ] of b ] available-patches ; needed here for move-farm2 procedure, in case there are no suitable patches in search radius
end

to select-target-patch
  set my-usable-patches [] ; empties the list so that every farm can make their own
  set my-usable-patches filter [ x -> [ distance myself ] of x < 5 AND [ yield-multiplier ] of x * [ seed ] of self > [ grain-need ] of self ] available-patches
  ; from available-patches filters the ones in a predefined radius and the ones which can produce enough crop to satisfy the grain-need
  ; this takes long time, because searches from all available-patches which is a long list. Should optimise!
  ifelse length my-usable-patches != 0 [
    set my-usable-patches sort-by [ [ a b ] -> [ distance myself ] of a < [ distance myself ] of b ] my-usable-patches ; this way farms will prefer the closest patch. Could test both ways.
;    set my-usable-patches sort-by [ [ a b ] -> [ yield-multiplier ] of a > [ yield-multiplier ] of b ] my-usable-patches ; this way farms will prefer the best patch
    set target-patch first my-usable-patches
    ;foreach my-usable-patches [x -> ask x [ set pcolor red ] ]
  ][
    ; what to do when there are no suitable patches in search radius?
    ; At the moment moves to best available patch, no matter the distance. But that is actually a great reward, with no downside. Not sure that is right here.
    ifelse length available-patches != 0 [
      ;sort-available-patches ; used here for sorting the the list. no need to do it on go procedure, because this is the only instance its used. Cannot be used here. Makes every farm compile the list at tick 0
      set target-patch last available-patches
    ][
      ; if no patch available then farm will die
      set no-room-death no-room-death + 1
      ;set all-harvested-patches lput length [ harvested-patches ] of self all-harvested-patches
      ; not using it here because here it does not show normal number of patches needed but a smaller number, because of overpopulation
      set age-at-death lput [ age ] of self age-at-death
      if population-growth = "marry" AND member? self fertile-farms [
        set fertile-farms remove self fertile-farms
      ]
      die
    ]
  ]
end

to move-farm2
  if [ yield-multiplier ] of patch-here * seed < grain-need  [ ; AND [ yield-multiplier ] of last available-patches * seed > grain-need
                                                               ; AND ... part could check, if there are even any suitable patches available before going forward
    ask patch-here [ ; setting the current patch up for fallowing (before the farms leaves or dies)
       set land-cover-type "open"
       set in-fallow TRUE ; probably not necessary since this was the basis of creating available-patches list at setup, but not needed for updating it,
                          ; since farms and the fallow procedure put in and take out the patches from the list themselves
       set occupied FALSE
       set fallowing-patches lput self fallowing-patches
     ]
    select-target-patch
    move-to target-patch
    set available-patches remove patch-here available-patches
;    ask patch-here [ ; might not be neccessary with the use of available-patches list
;       set occupied TRUE ; confirming occupation of the new patch
;     ]
    set moves moves + 1
  ]
end

to move-farm
  if ticks = 0 [ set fallowing-patches [] ]
  if [ yield-multiplier ] of patch-here * seed < grain-need [

   ; should compare best-patches agent-set with harvested-patches list and prefer the ones previously harvested. could not get it working. filter? function did not work. maybe should make empty patches and global list instead,
   ; so could compare a list to a list not to an agentset.
   ;ifelse member? harvested-patches best-patches = TRUE [  ][  ]
   ask patch-here [ ; setting the current patch up for fallowing (before the farms leaves or dies)
       set land-cover-type "open"
       set in-fallow TRUE
       set occupied FALSE
       set fallowing-patches lput self fallowing-patches
     ]
   ifelse length available-patches != 0 AND [ yield-multiplier ] of last available-patches * seed > grain-need [
      ;let p last available-patches ;[ distance myself ] ; chooses a closest empty patch with most grain
     ;ifelse p != nobody [
      ;if [ yield-multiplier ] of p
      move-to last available-patches
      set available-patches remove patch-here available-patches
      ask patch-here [ ; kas nüüd kui kasutusel available-patches list on seda osa enam vaja?
        set occupied TRUE ; confirming occupation of the new patch
      ]
      set moves moves + 1 ; just testing, to see how many times farms move. seem to move on every tick
    ][
      set no-room-death no-room-death + 1
      ;set all-harvested-patches lput length [ harvested-patches ] of self all-harvested-patches ; võtsin siit praegu välja sest need ei kujuta nn tavaolukorda vaid juba ülepopulatsiooni tõttu suremist
      set age-at-death lput [ age ] of self age-at-death
      if population-growth = "marry" AND member? self fertile-farms [
        set fertile-farms remove self fertile-farms
      ]
      die
    ]
  ]
end


to calculate-grain-need ; farms' yearly need of grain in kg
 ; calories needed divided by number of calories in one kg
 ; ( number of people in farm (slider) * daily caloric need * days in a year ) * percentage of calories from grain (slider) / 100 (because the slider is in % not 0,...) / kcal in kg of grain (mean of barley, wheat and oats) +
 ; + ( seed for next years' sowing  * calories-from-grain / 100 )
  ; seed amount (550 kg) derived from Tarvel 1972, 47-48. look at notes file. alele poole vähem. 351 võetud ETRA lk 339. vt märkmeid. arvestatud, et üks patch on 3 ha. ehk 117 * 3 = 351. kuna 25% läks maksudeks siis võiks olla hoopis 90*1.3*3*0.75 = 264
  ; otsida kust see 25% maksudeks on võetud
  set seed 264 * calories-from-grain / 100
  ; 2000*365/100/3500 = 2.085
  ; set grain-need round( people * calories-from-grain * 2.085 + seed
  set grain-need round( people * 2000 * 365 * calories-from-grain / 100 / 3500 + seed )
  set max-storage grain-need * 3
end

to record-occupation
  ; farm will record its presence on a patch by updating occupancy frequency by 1
  ask patch-here [
    set occupied TRUE
    set occupation-frequency occupation-frequency + 1
  ]
end

;;; Population growth ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to pop-growth
  count-all-harvested-patches
  if population-growth = "stable" [ growth-stable ]
  if population-growth = "fission" [ growth-fission ]
  if population-growth = "marry" [ growth-marry ]
end

to growth-fission
; if a farm has twice the food of the consumption rate then it will birth another farm. the storage will be divided between them
  ask farms [
    if storage >= grain-need * 2 AND age > 30 AND age < 60 AND offspring <= 10 AND count turtles <= pop-limit [ ;  ; AND length available-patches > mean-required-patches * count farms
      ; added limit to available patches in order to reproduce.  AND length available-patches > 100
      if random 100 > 60 [ ; this is basically a probability of children reaching adulthood (16 years)
       set storage storage - grain-need
       hatch 1 [
         new-farm-settings
         ]
       set offspring offspring + 1
      ]
    ]
  ]
end

to count-all-harvested-patches
  if ticks = 0 [ set all-harvested-patches [] ] ; move that to setup
  ifelse length all-harvested-patches != 0 [ ; this if procedure could be replaced by just "mean all-harvested-patches * count farms" in the next if. Add value 1 to list at setup to avoid division by zero
      set mean-required-patches mean all-harvested-patches
    ][
      set mean-required-patches 1 ; added abritrary value of 1 so there would not be "division by zero" error in the beginning of simulation where no farm has died yet
    ]
end

to growth-marry
  list-fertile-farms
  ; compared to fission, the advantage is that a single farms does not need to have twice its needs.
  ifelse length fertile-farms / 2 != floor ( length fertile-farms / 2 ) [ ; checking if the length is an even or an uneven number
   set new-farms-number length fertile-farms / 2
  ][
   set new-farms-number length fertile-farms / 2 + one-of [ -0.5 0.5 ] ; if length of fertile farms is uneven, then will create farms equal to if the length was even or add one more to that
  ]
  if ( length available-patches - ( mean-required-patches * count farms ) ) / mean-required-patches > length fertile-farms / 2 [
  ; this will only let new farms born, if there is enough room for them, based on how many farms are there at the moment and how much land does one farm need on average.
  ; this eliminates huge peaks in population, but also assumes quite good (impossible?) global knowledge of the farms.
  ; In order for this to work properly, should add an option to have less new farms than half of fertile farms. This way, if there is not room for fertile-farms/2 new farms,
  ; then at least those could still born for whom there is space. if there is space for 4 but fertile-farms/2 = 6, then the 4 could still be born. At the moment in this case no new farms are bo
   create-farms new-farms-number [
    new-farm-settings
    ]
   foreach fertile-farms [ x -> ask x [
     set storage storage - 0.5 * grain-need
     set offspring offspring + 1
    ]
   ]
  ]
end

to list-fertile-farms
  ask farms [
    if storage >= grain-need * 1.5 AND age > 30 AND age < 60 AND offspring <= 10 AND not member? self fertile-farms [
     set fertile-farms lput self fertile-farms
    ]
;      [
;     if member? self fertile-farms [
;       set fertile-farms remove self fertile-farms
;      ]
;    ]
;    set fertile-farms remove-duplicates fertile-farms
  ]
end

to growth-stable
  ; growth is constant. no "families" are created, because farms do not hatch, but are created
  ; currently based on number of farms, not people
  ; the population will grow by 0.1% every tick by creating relevant number of new farms (population2). the larger the decimal point of population2 the better chance of creating another farm (population3)
  set population count farms
  set population2 0.002 * population ; 0.01 is too low, because farms will die before new ones will be born
  set population3 precision ( population2 - floor population2 ) 2
  ifelse ( random-float 100 ) < ( 100 * population3 )[
    set new-farms-number floor population2 + 1
  ][
    set new-farms-number floor population2
  ]
  create-farms new-farms-number [
    new-farm-settings
  ]
end

to new-farm-settings
  ; did it this way so adjusting the settings for different pop growth types would be easier
  set shape "house"
  set size 1
  set age 16
  set nofood 0
  set moves 0
  set offspring 0
  set harvested-patches []
  set people people-per-farm ; one-of (range 4 10)
  calculate-grain-need
  set storage grain-need ; at the moment (12.02) working only when all farms have same grain-need, i.e. same number of people
  set crop 0
  set reachable-patches [] ; bad names. need to change
  set usable-patches []
  setxy random max-pxcor + 1 random max-pycor + 1 ; causes some new farms to go the unsuitable area for first year.
  ; could make it so that each new farm would settle near its parents
end

to check-death ;; look into dying age/4
  ; farm will die if it has not had any food for two consecutive ticks
  ; when farm reaches age 30 it will have 5% chance of dying during every next tick
   ifelse storage = 0 [ set nofood nofood + 1 ] [ set nofood 0 ]
   set age age + 1
   if nofood >= 2 OR age >= 25 AND random 100 < age / 4 [ ; combined both dying opportunities (starving and aging) into one if statement (19.01.20)
     ask patch-here [
        set land-cover-type "open"
        set in-fallow TRUE
        set occupied FALSE
        set fallowing-patches lput self fallowing-patches
       ]
     ifelse nofood = 2 [ ; for recording if farm died because it had no food or because of old age
       set nofood-death nofood-death + 1
     ][
       set age-death age-death + 1
     ]
     set all-harvested-patches lput length [ harvested-patches ] of self all-harvested-patches ; testing (23.01.2020) to remove sharp peaks from population growth and decline. added to reproduce procedure
     set age-at-death lput [ age ] of self age-at-death
     if population-growth = "marry" AND member? self fertile-farms [
      set fertile-farms remove self fertile-farms
     ]
     die
   ]
end

to calculate-pop-density
  set pop lput count farms pop
  if length pop > 2 [ set pop remove-item 0 pop ] ; no need for the list to be longer
  if length pop = 2 [
    set pop-growth-rate precision (( item 1 pop - item 0 pop ) / item 0 pop ) 4 ; calculating pop density by subtracting previous year pop from current year pop and dividing by previous year pop
    set pop-growth-rate-list lput pop-growth-rate pop-growth-rate-list
    if length pop-growth-rate-list > 100 [ ; looking at pop growth rate of last 100 years
      set pop-growth-rate-list remove-item 0 pop-growth-rate-list
    ]
  ]
  set pop-density ( count farms * people-per-farm ) / (( count patches * 3 ) / 100 )
  set pop-density-list lput pop-density pop-density-list
  if length pop-density-list > 100 [ ; looking at pop density of last 100 years
    set pop-density-list remove-item 0 pop-density-list
  ]
end


;;; Patch procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to fallow ; grain will start to re-grow if (1) the patch has not been used for at least one tick, (2) and has been occupied at least once, (3) and has current growth-rate lower than global-max-grain at that soil
; currently all the soils deplete the same way. should add different depletion characteristics.
; For example, (1) something that has lot of grain but depletes quickly and replenishes slowly and (2) something that has less grain, but depletes more slowly and replenishes faster
; depletion should be exponential. especially on thin rendzina soils.
  foreach fallowing-patches [ x -> ask x [
    if year-of-harvest = 0 [ ; added the if procedure so that the years-of-fallowing counter would start one year after the last harvest and not on the year of last harvest.
      ; basically it will just skip adding years-of-fallowing for the first year
      set years-of-fallowing years-of-fallowing + 1
    ]
    if years-of-fallowing = 1 [ set land-cover-type "open" ] ;;;
    set year-of-harvest 0
    if years-of-fallowing >= precision required-fallow 1 [ ; fallow-period-length
      set in-fallow FALSE
      set land-cover-type "forest" ; forest should grow earlier than just magically appear at the end of fallow. maybe should
      set years-of-fallowing 0
      ; soil depletion at end of each fallow cycle
      ; set original-yield-multiplier original-yield-multiplier - 0.005 * original-yield-multiplier ; needs to changed, based on data (CENTURY?), so the soils could deteriorate
      ; set yield-multiplier original-yield-multiplier ; siin jäi poolele 11.01.20. original-yield-multiplier ei ole väga hea nimi enam. vbl pigem max-yield-multiplier
      soil-depletion
      if soil-suitability = "Good" [ ; testing elongating rendzina soil fallow periods.
       set required-fallow precision ( required-fallow + 0.5 ) 1 ; the required fallow for rendzina soils get 0.5 years longer with every fallow period
       ]
      set fallowing-patches remove self fallowing-patches
      set available-patches lput self available-patches
    ]
   ]
  ]
end

to soil-depletion ; no need for original-yield-multiplier
  if soil-suitability = "Good" [
    set original-yield-multiplier original-yield-multiplier - 0.005 * original-yield-multiplier ; needs to changed, based on data (CENTURY?), so the soils could deteriorate
    set yield-multiplier original-yield-multiplier
  ]
  if soil-suitability = "Moderate" [
    set original-yield-multiplier original-yield-multiplier - 0.001 * original-yield-multiplier
    set yield-multiplier original-yield-multiplier
  ]
  if soil-suitability = "Poor" [
    set original-yield-multiplier original-yield-multiplier - 0.001 * original-yield-multiplier
    set yield-multiplier original-yield-multiplier
  ]
end

to make-soils ; area sizes based on soil map
  ask patches [ ; too basic way to do it. could be dependent on number of soils slider
    if pycor <= 0.14 * max-pycor                              [ set pcolor 24 ;[204 76 2]
                                                          set soil-suitability "Good"

    ]
    if pycor >= 0.14 * max-pycor AND pycor <= 0.32 * max-pycor [set pcolor 26 ; [ 254 153 41 ]

                                                          set soil-suitability "Moderate"
    ]
    if pycor >= 0.32 * max-pycor AND pycor <= 0.52 * max-pycor [ set pcolor 27 ; [ 254 217 142 ]
                                                          set soil-suitability "Poor"
    ]
    if pycor >= 0.52 * max-pycor                               [ set pcolor 29 ; [ 255 255 212 ]
                                                          set soil-suitability "Not suitable"
   ]
   set years-of-fallowing 0
   set in-fallow FALSE
   set occupied FALSE
   set original-color pcolor
   set required-fallow fallow-period-length
   define-land-cover-type
   define-yield-multiplier
  ; define-soil-fertility
  ]

end

to make-plain
  ; create a homogenous area where all patches have grain and growth rate equal to the global max grain
   ask patches [
    set pcolor green
    set years-of-fallowing 0
    set in-fallow FALSE
    set occupied FALSE
    set original-color pcolor
    set required-fallow fallow-period-length
    set soil-suitability "Moderate"
    define-land-cover-type
    define-yield-multiplier
;    define-soil-fertility
  ]

end

;;; Land cover and REVEALS model data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to define-land-cover-type
  ; based on REVEALS model data form lake Maardu
  ; At 2900 BC 82% of patches are forest and the remaining are open landscape
  ifelse random 100 < 82 [
   set land-cover-type "forest"
  ][
   set land-cover-type "open"
  ]
end

to report-reveals-data ; avoid opening files with every tick
  ; opens files in the model folder and reads one line each tick. Data in the files represents each landcover class from 2900 BC to 450 AD and is sorted accordingly.
  file-open "open_reveals.csv"
  set open-reveals csv:from-row file-read-line
  set open-reveals-value item 0 open-reveals
  file-open "forest_reveals.csv"
  set forest-reveals csv:from-row file-read-line
  set forest-reveals-value item 0 forest-reveals
  file-open "agrarian_reveals.csv"
  set agrarian-reveals csv:from-row file-read-line
  set agrarian-reveals-value item 0 agrarian-reveals
end


;;; Soils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to define-yield-multiplier
  set yield-multiplier 10
  if soil-suitability = "Good" [ set yield-multiplier yield-multiplier * 1.5 ] ; testida kuidagi kui palju parem peab see pinnas olema, et luua reaalseid mustreid vms
  if soil-suitability = "Moderate" [ set yield-multiplier yield-multiplier * 1 ]
  if soil-suitability = "Poor" [ set yield-multiplier yield-multiplier * 0.5 ] ; alla kahe seemne peeti väga kehvaks mullaks juba. vt ETRA
  if soil-suitability = "Not suitable" [ set yield-multiplier 0.1 ]
  set original-yield-multiplier yield-multiplier
end

to define-soil-fertility ; parandada need väärtused ;parandada is not a word, no way
  ; on second year the yield drops to about 30% of the first year. if the plot has been cultivated on already for two previous years, then the yield is about 10 times less (here 9) than on first year.
  ; from Ehrmann 2014 fig 4 ja Rösch 2017, 7
  if year-of-harvest = 0 [ set yield-multiplier yield-multiplier ]
  if year-of-harvest = 1 [ set yield-multiplier precision ( yield-multiplier / 3 ) 3]
  if year-of-harvest = 2 [ set yield-multiplier precision ( yield-multiplier / 3 ) 3]
  if year-of-harvest = 3 [ set yield-multiplier precision ( yield-multiplier / 2 ) 3]
end


;;; Visualisation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to visualise
  if visualisation != "no-visualisation" [
   if visualisation = "land-cover" [ visualise-land-cover ]
   if visualisation = "occupation" [ visualise-occupation ]
   if visualisation = "fallow" [ ask patches [ visualise-fallow ] ]
  ]
end

to visualise-land-cover
  ask patches [
    if land-cover-type = "forest" [ set pcolor green ]
    if land-cover-type = "agrarian" [ set pcolor black ]
    if land-cover-type = "open" [ set pcolor orange ]
  ]
end

to visualise-occupation ; repair!!!
  ; patches will be colored from
  if max [ occupation-frequency ] of patches != 0 [ ; did not work (setup produced error) as part of unified visualise procedure with out the if function. probably cannot scale-color from 0 to 0 at setup (tick 0).
    let max-color max [occupation-frequency] of patches
    ask patches [ set pcolor scale-color red occupation-frequency 0 max-color ]
    ask farms [ set hidden? true]
  ]
end

to visualise-fallow
   ; if patch is left to fallow the color will turn dark and become more light every tick. if the fallow period is over, the patch will be colored the original color
    ifelse years-of-fallowing = 0 [
      set pcolor original-color ; original-color
    ][
      set pcolor green ;scale-color original-color years-of-fallowing 0 original-color ;
    ]
end




;;;; Miscellaneous ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to make-movie
 setup
   repeat 300 [
     if count farms > 1 [
     go
     export-interface (word "C:/phd/thesis/netlogo_model/gif/tick" (1000 + ticks) ".png")
   ]
  ]
end

to increase-calories-from-grain
  ifelse calories-from-grain < max-calories-from-grain [ ; reliance on agriculture grows gradually
    set calories-from-grain precision ( calories-from-grain + 0.01 ) 2 ;
  ][
    set calories-from-grain max-calories-from-grain
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
189
10
791
433
-1
-1
9.0
1
10
1
1
1
0
0
0
1
0
65
0
45
0
0
1
years
30.0

SLIDER
6
111
178
144
initial-number-farms
initial-number-farms
0
100
20.0
1
1
NIL
HORIZONTAL

BUTTON
17
16
90
49
Setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
100
53
168
86
Go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

SWITCH
6
302
96
335
soils?
soils?
0
1
-1000

BUTTON
99
15
170
48
Go once
go
NIL
1
T
OBSERVER
NIL
O
NIL
NIL
1

PLOT
0
485
200
635
Population
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? [ plotxy ticks count turtles ]"

PLOT
624
484
824
634
Mean storage
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? [ if ticks > 0 [ plotxy ticks mean [ storage ] of turtles ] ]"

PLOT
1099
166
1299
316
Storage distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if plots-on? [ \nset-histogram-num-bars 10\nset-plot-x-range 0 (max [max-storage] of farms + 1)\nset-plot-pen-interval (max [storage] of farms + 1) / 10\nhistogram [storage] of farms\n]"

SLIDER
6
189
178
222
fallow-period-length
fallow-period-length
1
200
25.0
1
1
years
HORIZONTAL

SLIDER
6
149
178
182
people-per-farm
people-per-farm
2
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
6
226
177
259
calories-from-grain
calories-from-grain
0
100
100.0
1
1
%
HORIZONTAL

PLOT
415
485
615
635
Land use proportions
NIL
NIL
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Forest" 1.0 0 -15575016 true "" "if plots-on? [ plotxy ticks count patches with [ land-cover-type = \"forest\" ]  / count patches * 100 ]"
"Open" 1.0 0 -955883 true "" "if plots-on? [ plotxy ticks count patches with [ land-cover-type = \"open\" ] / count patches * 100 ]"
"Agrarian" 1.0 0 -16777216 true "" "if plots-on? [ plotxy ticks count patches with [ land-cover-type = \"agrarian\" ] / count patches * 100 ]"

CHOOSER
4
341
177
386
visualisation
visualisation
"land-cover" "occupation" "fallow" "no-visualisation"
2

PLOT
830
484
1030
634
Farm age distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if plots-on? [ \nset-histogram-num-bars 10 \nset-plot-x-range 0 70\n;set-plot-pen-interval ( max [ age ] of farms + 1 / 10 )\nhistogram [ age ] of farms\n]"

MONITOR
1
648
58
693
Year
year
0
1
11

PLOT
828
166
1093
314
Open land
NIL
NIL
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"REVEALS" 1.0 0 -13345367 true "" "if plots-on? [ plot open-reveals-value ]"
"ABM" 1.0 0 -955883 true "" "if plots-on? [ plotxy ticks count patches with [ land-cover-type = \"open\" ] / count patches * 100 ]"

PLOT
828
10
1093
160
Forest
NIL
NIL
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"REVEALS" 1.0 0 -13345367 true "" "if plots-on? [ plot forest-reveals-value ]"
"ABM" 1.0 0 -10899396 true "" "if plots-on? [ plotxy ticks count patches with [ land-cover-type = \"forest\" ]  / count patches * 100]"

PLOT
829
320
1091
470
Agrarian land
NIL
NIL
0.0
10.0
0.0
25.0
true
true
"" ""
PENS
"REVEALS" 1.0 0 -14070903 true "" "if plots-on? [ plot agrarian-reveals-value ]"
"ABM" 1.0 0 -16777216 true "" "if plots-on? [ plotxy ticks count patches with [ land-cover-type = \"agrarian\" ] / count patches * 100]"

BUTTON
18
54
89
87
Profiler
setup ;; set up the model\nprofiler:start ;; start profiling\nrepeat 10 [ go ] ;; run something you want to measure\nprofiler:stop ;; stop profiling\nprint profiler:report ;; view the results\nprofiler:reset ;; clear the data
NIL
1
T
OBSERVER
NIL
P
NIL
NIL
1

SWITCH
96
302
186
335
plots-on?
plots-on?
0
1
-1000

PLOT
1101
320
1301
470
Yield multiplier mean
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Good soil" 1.0 0 -12087248 true "" "if plots-on? and ticks > 0 [ \n  plot mean [ yield-multiplier ] of patches with [ soil-suitability = \"Good\" ] ]"
"Moderate soil" 1.0 0 -1184463 true "" "if plots-on? and ticks > 0 [ plot mean [ yield-multiplier ] of patches with [ soil-suitability = \"Moderate\" ]]"
"Poor soil" 1.0 0 -6459832 true "" "if plots-on? and ticks > 0 [ plot mean [ yield-multiplier ] of patches with [ soil-suitability = \"Poor\" ]]"

SLIDER
6
262
177
295
max-calories-from-grain
max-calories-from-grain
10
100
100.0
1
1
%
HORIZONTAL

MONITOR
499
745
595
790
death no-room
no-room-death
0
1
11

PLOT
208
485
408
635
Available patches
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? [ if ticks > 0 [ plotxy ticks (length available-patches) ] ]\n"

MONITOR
225
649
322
694
Available patches
length available-patches
0
1
11

PLOT
1041
484
1241
634
Mean of harvested patches
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? [ if ticks > 0 AND length all-harvested-patches != 0 [ plotxy ticks mean-required-patches ] ]"

MONITOR
164
648
214
693
Farms
count turtles
17
1
11

MONITOR
499
649
598
694
death due hunger
nofood-death
17
1
11

MONITOR
500
697
594
742
death due age
age-death
17
1
11

PLOT
1099
10
1299
160
Crop distribution
NIL
NIL
0.0
6000.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if plots-on? [ \n  set-histogram-num-bars 10 \n  ;set-plot-x-range 0 50\n  histogram [ crop ] of farms with [ moves > 0 ]\n  ]"

MONITOR
325
649
407
694
Mean harv p
mean-required-patches
1
1
11

MONITOR
412
649
494
694
MHP * farms
mean-required-patches * count farms
1
1
11

SLIDER
5
437
172
470
pop-limit
pop-limit
1
9000
7567.0
1
1
NIL
HORIZONTAL

PLOT
1246
484
1446
634
Age at death
NIL
NIL
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if plots-on? and ticks > 0 [ \nset-histogram-num-bars 20\n;set-plot-x-range 0 (max [max-storage] of farms + 1)\n;set-plot-pen-interval (max [storage] of farms + 1) / 10\nhistogram age-at-death\n]"

MONITOR
623
650
747
695
Yield-multi of last ap
[yield-multiplier] of last available-patches
4
1
11

MONITOR
623
698
713
743
seed * last ap
precision ( mean [ seed ] of farms * [yield-multiplier] of last available-patches ) 3
17
1
11

PLOT
1308
320
1508
470
Yield-multiplier of last ap
NIL
NIL
0.0
10.0
2.0
8.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? and ticks > 0 [ \n  plot [ yield-multiplier ] of last available-patches ]"

MONITOR
623
746
784
791
Best patch satisfies grain-need
mean [ seed ] of farms * [yield-multiplier] of last available-patches >= mean [ grain-need ] of farms
17
1
11

MONITOR
1512
166
1603
211
Pop growth rate
pop-growth-rate
4
1
11

PLOT
1306
166
1506
316
Population growth rate
NIL
NIL
0.0
10.0
-0.5
0.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? and ticks != 0 and length pop > 1 [\nplot ( item 1 pop - item 0 pop ) / item 0 pop\n]"

PLOT
1305
10
1505
160
Mean pop growth rate
NIL
NIL
0.0
10.0
-0.05
0.05
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? and ticks > 0  and length pop-growth-rate-list != 0 [\nplot mean pop-growth-rate-list\n]"

MONITOR
1514
10
1694
55
Mean pop growth rate (100 y)
mean pop-growth-rate-list
4
1
11

MONITOR
830
695
972
740
pop density (persons/km2)
pop-density
3
1
11

MONITOR
831
645
971
690
Mean pop density  (100 y)
mean pop-density-list
3
1
11

PLOT
1041
639
1241
789
Mean pop density
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? and ticks > 0 and length pop-density-list > 1 [\nplot mean pop-density-list\n]"

PLOT
1246
639
1446
789
Pop density
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if plots-on? and ticks > 0 [\nplot pop-density\n]"

BUTTON
4
766
83
799
Make gif
make-movie
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
52
706
129
751
Fertile farms
length fertile-farms
2
1
11

MONITOR
133
706
235
751
Farms with 1 child
count farms with [ offspring = 1 ]
17
1
11

MONITOR
239
706
343
751
Farms with 2 child
count farms with [ offspring = 2 ]
17
1
11

PLOT
1455
483
1655
633
Farms with offsprings
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
"1" 1.0 0 -5298144 true "" "if plots-on? [ plot count farms with [ offspring = 1 ] ]"
"2" 1.0 0 -14070903 true "" "if plots-on? [ plot count farms with [ offspring = 2 ] ]"

CHOOSER
4
389
176
434
population-growth
population-growth
"stable" "fission" "marry"
2

MONITOR
347
706
486
751
Room for number of farms
( length available-patches - ( mean-required-patches * count farms ) ) / mean-required-patches
1
1
11

MONITOR
63
648
160
693
Study area in km2
count patches * 3 / 100
1
1
11

@#$#@#$#@
## FILL IN THE INFO ANDRES

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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="fallow-length-effect" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>no-room-death &gt;= 10</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="number-farms">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plots-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisation">
      <value value="&quot;no visualisation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pop-growth-stable?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soils?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="people-per-farm">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-calories-from-grain">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fallow-period-length" first="5" step="5" last="100"/>
    <enumeratedValueSet variable="calories-from-grain">
      <value value="75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="no_space_all_dead" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>not any? turtles</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="number-farms">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plots-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualisation">
      <value value="&quot;no visualisation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pop-growth-stable?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soils?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="people-per-farm">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-calories-from-grain">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fallow-period-length" first="10" step="10" last="100"/>
    <enumeratedValueSet variable="calories-from-grain">
      <value value="75"/>
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
0
@#$#@#$#@
