extensions[csv matrix]

globals[
  number-of-months     ; Tracks the number of months in simulation
  number-of-years      ; Tracks the number of years in simulation
  growth-logistic-A    ; Skeleton for logisitic function for the change in infra rate
  growth-logistic-B    ; Skeleton for logisitic function for the change in infra rate
  infra-rates          ; Infrastructure growth rates that have been captured from data file
  growth-rates         ; Sales growth rates that have been captured from data file
  growth-prob          ; Calculated annual probability of rate of growth of charging infra
  probabilties         ; List to capture all the calculated charging infra probabilities
  sales-rate           ; Calculated annual rate of sales growth of EVs
  sales-probabilties   ; List to capture all the calculated sales growth rates
  corr-matrix          ; List to capture correlation calculation
  corr-sales-infra     ; Corrrelation coefficient capture for looking at how the sales varies based upon infrastructure
  correlations         ; List to capture all the corrrelation coefficients
  ;ch-stations          ;Moved to slider
  passed-months
  P1                   ; infra logisitic check
  P2                   ; infra logisitic check
  srate                ; Average of sales growth rate based on the data
  ssd                  ; Standard Deviation of sales growth rate based on the data
]


to setup
  ca
  reset-ticks

  ;Using the data file to capture sales and charging infrastructure data
  let data csv:from-file "Amar_Agrawal_alldata_16_11_2022.csv"
  file-close

  ;Setting up the normal distribution variables for Sales Growth rate
  set growth-rates item 3 data
  set growth-rates remove-item 0 growth-rates
  set srate mean growth-rates
  set ssd standard-deviation growth-rates

  ;Setting up the linear regression for infrastructure growth rate
  set infra-rates item 5 data
  set infra-rates remove-item 0 infra-rates
  set P1 max infra-rates
  set P2 0.01 + random min infra-rates

  ;Initializing the different parameters
  set number-of-months 60
  set passed-months 0
  set probabilties []
  set number-of-years 0
  set sales-rate 0
  set sales-probabilties []
  set correlations []

  setup-growth-logistic

  ;Initializing the available charging stations
  ask n-of ch-stations patches[set pcolor green]

end


;the go procedure
to go
  tick

  while [number-of-months > ticks]
    [

      calculate-annual
      update-growth-prob
      update-sales-rate

      let count1 count patches with [pcolor = green]
      set-current-plot "Charging Stations"
      plot count1
    ]

  ;Print out lists at the end to check the values of output
  ;show corr-sales-infra
  ;show probabilties
  ;show sales-probabilties
  show correlations
  ;show the current average of the correlations with valid values for the model run of 60 ticks (5 years)
  let avg item (2) correlations + item (3) correlations
  show avg / 2

end

;subprocedure to run the model updates annually
to calculate-annual
  set passed-months passed-months + 1  ;passed-months calucalation to update the model every year
  if passed-months mod 12 = 0
    [
     increase-stations
    ]
  tick
end


;subprocedure to handle the major calculation of the model that is the correlation while plotting them
to increase-stations

  ;Populating all the lists with existing values
  set probabilties lput growth-prob probabilties
  set sales-probabilties lput sales-rate sales-probabilties
  set correlations lput corr-sales-infra correlations

  set number-of-years number-of-years + 1

  ;Increasing the number of charging infrastructure based on their current number
  let countch count patches with [pcolor = green]
  let newcount countch + ( countch * growth-prob )
  set newcount newcount - countch
  ;show newcount
  ask n-of newcount patches [set pcolor green]

  ;Plotting the infra growth rate in realtime
  set-current-plot "Infra Growth"
  let value 0
  let terms length probabilties
      ifelse terms < 1
      [set value growth-prob ]
      [set value item ( terms - 1 ) probabilties]
  plot value

  ;Plotting the sales growth rate in realtime
  set-current-plot "Sales Growth"
  let value1 0
  let terms2 length sales-probabilties
      ifelse terms2 < 1
      [set value1 growth-prob ]
      [set value1 item ( terms2 - 1 ) sales-probabilties]
  plot value1

  ;Calculate the correlation only when there are more than 1 value in each of the lists
  if terms > 1 and terms2 > 1
  [
    set corr-matrix matrix:regress matrix:from-column-list (list sales-probabilties probabilties)
    set corr-sales-infra  item (0) corr-matrix
    set corr-sales-infra  item (0) corr-sales-infra  ;populating the correlation coefficient by iterating through result of correlation operation
  ]

  ;Plotting the sales growth rate in realtime
  set-current-plot "Corelations"
  let value3 0
  let terms3 length correlations
      ifelse terms3 < 1
      [set value3 corr-sales-infra ]
      [set value3 item ( terms3 - 1 ) correlations]
  plot value3

end


to setup-growth-logistic
  ;set P1 0.25 ;Static calculation for the data submitted and also the best parameter value
  let X1 18 ;1.5 years into the model the rate is going to be very high
  ;let X1 ( number-of-months / 12 ) * 0.2 ; 0.2 runtime into the model the rate is going to be very high

  ;set P2 0.1  ;Static calculation for the data submitted and also the best parameter value
  let X2 42 ;3.5 years into the model the rate is going to gradually go down
  ;let X2 ( number-of-months / 12 ) * 0.8 ; 0.8 runtime into the model the rate is going to going to gradually go down

  let repro-D ln (P1 / (1 - P1))
  let repro-C ln (P2 / (1 - P2))

  set growth-logistic-B (repro-D - repro-C) / (X1 - X2)
  set growth-logistic-A repro-D - (growth-logistic-B * X1)

end


;subprocedure to calculate the update infrastructure growth rate using logistic function
to update-growth-prob

  ; This procedure updates the probability of reproducing (global var. growth-prob),
  ; a function of the total charging infrastructure available before annual growth starts
  let total-infra count patches with [pcolor = green]

  ; Use the logistic variables initialized during setup
  let logistic-Z exp (growth-logistic-A + (growth-logistic-B * total-infra))

  set growth-prob (logistic-Z / (1 + logistic-Z))

end


;subprocedure to calculate the sales growth rate using normal distribution
to update-sales-rate
  ;set sales-rate random-normal 0.11 0.02  ;Static calculation for the data submitted
  set sales-rate random-normal srate ssd
end
@#$#@#$#@
GRAPHICS-WINDOW
242
11
791
561
-1
-1
16.4
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
80
99
143
132
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
80
155
143
188
NIL
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
21
224
221
374
Charging Stations
days
Charging Infra
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
29
32
201
65
ch-stations
ch-stations
1
35
1.0
1
1
NIL
HORIZONTAL

PLOT
818
32
1018
182
Sales Growth
number-of-year
sales-rate
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
819
208
1019
358
Infra Growth
number-of-year
infra-growth-rate
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
819
380
1019
530
Corelations
number-of-year
corelations
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

@#$#@#$#@
The agent-based model is presented following the ODD (Overview, Design concepts, and Details) protocol (Grimm et al., 2006, Grimm et al., 2010).

# Model ODD Description
People usually say that there would be a significant increase in sales of electric vehicles only if there were more charging options and less to worry about the cars stopping midway. This model tries to examine the same if there exists a very strong relationship between them or not. The model makes use of data available for the state of California and tries and predicts the sales patterns and charging infrastructure growth while also keeping the track of the interrelationship between them. Following are the sources from which the data required has been collected and other references which have set the base for different patterns used and assumptions. 
Sources:
https://afdc.energy.gov/vehicle-registration?utf8=✓&year=2021					https://afdc.energy.gov/files/docs/historical-station-counts.xlsx				https://afdc.energy.gov/data/10366
https://afdc.energy.gov/stations/states
https://www.autosinnovate.org/about/advocacy/Recommended%20Attributes%20for%20EV%20Charging%20Stations%2009DEC2021.pdf
https://www.autosinnovate.org/posts/papers-reports/Get%20Connected%202022%20Q1%20Electric%20Vehicle%20Report.pdf
https://www.autolist.com/news-and-analysis/2021-survey-electric-vehicles

## 1. Purpose and Patterns 

This model has been developed to simulate the importance of the availability of charging infrastructure for sales growth of electric cars in the US state of California. Initially, the cost of the vehicle and its range was also considered to be included in the scope of the model but were chosen to be omitted as the market price of cars was vague for different capacities with ambiguous after-sales benefits. Additionally, the lack of proper data on the actual requirement of range to be driven by average customers resulted in the exclusion of electric vehicles' range from this model.

The dataset used in this model consists of the rate of sales growth of the different types of electric vehicles and the rate of charging infrastructure available in the US state of California and the corresponding year (2016 - 2021) to which the data belongs to.

By looking at the sales data trends it was assumed that the sales followed a stochastic approach and was therefore implemented using a normal distribution.
The charging infrastructure does seem to have an increasing trend from the data but it has been assumed to fall after some time as the number of infrastructures one can create becomes scarce and also more and more difficult as time progresses. Therefore, the charging infrastructure growth rate has been modelled using a logistic regression.

## 2. Entities, State Variables and Scales

The different agents in this model are described below:
Charging Station: Charging stations are the apparatus used for charging an electric vehicle and are represented as the green patches in the environment. Instead of having the growth rate of the charging stations as a state variable, it has been dealt with differently as a different variable. The rate of growth of the charging infrastructure has been recorded as 'probabilities' which is a list containing all the predicted growth probabilities ('growth-prob' annually) of the charging infrastructure.

The rate of predicted annual sales growth('growth-rates') has also been dealt with as a list namely 'sales-probabilities' which does not relate to the patches or any other entity. 

'Correlations' is the final output list which calculates the correlations between the above two lists. 

Environment: The geographical state of California will be treated as the environment with multiple charging stations as patches and output intended as plots on Interface.

Each tick in the model represents a month passed and the sales and infrastructure growth rates are predicted at the end of 12 months.

## 3. Process overview and scheduling

The model will first import the dataset and then initialize values for sales growth median and standard deviation to calculate the normal distribution of the sales growth. Then, it will set the minimum and maximum charging infrastructure growth rates for the logistic function. After that the model starts with a initial number of charging infrastructure.

As the model begins execution it first checks if a year has passed in the simulation which is equivalent to execution of 12 ticks. If a year has passed then it calls the increase stations module which in turn takes then takes the existing sales growth rate and the charging infra growth rate and calculates the correlation. The first passes of the correlation are initialized as null since the data available is not enough to calculate a proper correlation. After that the necessary output are also updated on the 3 graphs showing the different individual trends. After execution of the sub model, it then calculates the new charging infrastructure annual growth rate and predicts the new sales growth rate for the upcoming simulation.  

## 4. Design Concepts

Basic Principles
The model plans to ascertain the relationship between the electric vehicles' sales with the charging apparatus by looking at the past trends of data and then trying and simulate the same for different values of each other. Looking at the past sales data, it was assumed that the sales pattern was stochastic and henceforth a new value is stochastically calculated as 'growth-rates' and stored into 'sales-probabilities'. The charging apparatus rate was also modelled as a decreasing logistic function throughout the years and each year value is stored in the 'probabilities' list.

Emergence
The sales growth's normal distribution is assumed to follow the past data trend, and is imposed with the mean and the standard deviation of the past data. Also, the maximum and minimum charging infrastructure growth rates are imposed based on the input data.
After the ascertainment of the correlation between them, an emergence will then be realized and stored in 'corr-sales-infra' each year which will illustrate how much does sales get affected by the charging gear. 

This model's final output is going to be the probability of how much sales grow for electronic vehicles based on the change in charging infrastructure. The final output is not just the sum of the aforementioned values but is a list of correlation values which are updated every year and all the output correlation values will be stored in 'correlations' list. The unit of probability will be different from the one used for measuring the sales figures and the number of charging stations. The inputs may hint at the outcome of the model as well and therefore the final output will not be a completely emergent result.

Adaptation
The number of charging infrastructure patches adhere to their growth probability. In the sub model 'update-growth-prob', the number of charging infrastructures is checked every year and the 'growth-prob' is adapted accordingly and new stations are added or if the probability is low then the number stays the same.

Observation Techniques
The model might be observed using multiple different techniques such as the "view" tab where the different graphical elements of the model are updated and their interaction with each other can be examined. Different plots like the "Sales Growth", "Infra Growth", and "Corelation" on the interface tabs have been used to depict the values of the different variables as the model runs. The model also includes behaviour space experiments to further observe and analyse the model's different runs and values.

Prediction
The model will predict the sales growth and charging infrastructure explicitly on the base of the input data each year. As the sales data tends to follow a random trend, the model uses a stochastic approach whereas the charging infrastructure growth rate ideally will decline and henceforth has been predicted as a decreasing logistic function.

Interaction
The two variables sales rate and charging infrastructure are entirely interdependent and are derived from the dataset and therefore will work as individual metrics to help the model make a better decision. 

Stochasticity
A normal distribution has been used for handling the various sales growth parameters namely, 'srate' and 'ssd' which the model ascertains based on the input from data files. Another factor which is the lower rate of growth of the charging infrastructure sales denoted by 'P2' of EV data has used a stochastic approach lower than the minimum of all the growth rates recorded. The model is still being developed and concepts of probability might still need refining in future. 

Observation
The model's output shows that there is not a significant relationship between sales growth and the rate of charging infrastructure growth as the final correlation values are significantly lower and below 0.15 in majority of runs. One of the reasons for this might be the stochastic approach used for the sales growth prediction. Another important factor is the exclusion of the existing personal or workplace charging stations due to which the number of charging infrastructures currently being recorded is not as realistic.

## 5. Initialization

The very first thing that the model would require is the data file without which the sub models would not function. Then, a value can be chosen from the slider for the starting number of charging infrastructure in the world. Additionally, when the data file is recognized, the charging growth rates' maximum and minimum values are initialized as well as the normal distribution for the sales growth is set up.

## 6. Input Data 

The input data file contains the sales numbers for the different years with the annual growth rate calculated as standardized values. Then, the subsequent rows contain the number of charging infrastructure available across the years along with their standardized annual growth rates.

## 7. Submodels 

increase-stations: This sub model takes in the current growth rate of the charging infrastructure and then updates the patches corresponding to the same number. It also updates all the output plots as the model progresses.

update-growth-prob: This sub model is responsible for updating the charging infrastructure growth rate annually in the model. It does so with the help of logistic functions that are set up in subsequent sub model setup-growth-logistic.

update-sales-rate: This sub model is responsible for updating the sales growth rate which takes the mean and standard deviation of all the sales rates input from the dataset.

calculate-annual: This sub model keeps track of the months being passed and runs the necessary sub models as a year passes
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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="CheckInfraSensitive" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>corr-sales-infra</metric>
    <steppedValueSet variable="P1" first="0.2" step="0.05" last="0.9"/>
    <steppedValueSet variable="P2" first="0.1" step="0.05" last="0.8"/>
  </experiment>
  <experiment name="CheckGrowthSensitive" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>corr-sales-infra</metric>
    <steppedValueSet variable="srate" first="5" step="5" last="50"/>
    <steppedValueSet variable="ssd" first="1" step="0.2" last="20"/>
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
