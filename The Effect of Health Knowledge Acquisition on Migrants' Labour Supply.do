use "D:\BaiduNetdiskDownload\2015\a卷(STATA).dta"
drop if Q203X==.
gen workhour= Q203X
gen overwork60=1 if workhour>=60
replace overwork60=0 if workhour<60

recode Q303A Q303B Q303C Q303D Q303E Q303F Q303G Q303H Q303I(2=0)
gen h_dummy=1 if Q303A==1
replace h_dummy=1 if Q303B==1
replace h_dummy=1 if Q303C==1
replace h_dummy=1 if Q303D==1
replace h_dummy=1 if Q303E==1
replace h_dummy=1 if Q303F==1
replace h_dummy=1 if Q303G==1
replace h_dummy=1 if Q303H==1
replace h_dummy=1 if Q303I==1
replace h_dummy=0 if h_dummy==.

gen male=1 if q101b1==1
replace male=0 if q101b1==2

gen age=2015- q101c1y
drop if age>64

gen weekwage=Q210/4
gen wage=weekwage/ workhour
drop if wage==.
replace wage=log(wage+1)
*winsor2 wage,cut(1,99) replace

gen han=1 if q101d1==1
replace han=0 if han==.

gen edu=0 if q101e1==1
replace edu=6 if q101e1==2
replace edu=9 if q101e1==3
replace edu=12 if q101e1==4
replace edu=15 if q101e1==5
replace edu=16 if q101e1==6
replace edu=19 if q101e1==7

gen hukou=1 if q101f1==1
replace hukou=1 if q101f1==3
replace hukou=0 if q101f1==2
replace hukou=0 if q101f1==4

gen marriage=1 if q101g1==2
replace marriage=1 if q101g1==3
replace marriage=0 if q101g1==1
replace marriage=0 if q101g1==4
replace marriage=0 if q101g1==5

gen Occupation= Q206
gen Industry= Q207
encode F2 ,gen(Region)

///Table 1. Migrants’ Acquisition of Health Knowledge in the Past Year
tab Q303A
tab Q303B
tab Q303C
tab Q303D
tab Q303E
tab Q303F
tab Q303G
tab Q303H
tab Q303I

///Table 2. Descriptive Statistics of Variables
logout,save(Descriptive Statistics of Variables)word replace:tabstat workhour overwork60 male age han edu hukou marriage wage liv,s(N min max mean sd p25 p50 p75 ) by( h_dummy)f(%12.3f) c(s)

///Table 3. Effects of Health Knowledge Acquisition on Working Hours and Overwork Possibility of Migrants
*OLS+Logit
reghdfe workhour h_dummy male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

*1:1 nearest neighbour matching within a calliper (radius 0.01)
psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) cal(0.01)
pstest male age han edu hukou marriage wage liv,both graph

bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3) 
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

*1:1 nearest neighbour matching
psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) 
pstest male age han edu hukou marriage wage liv,both graph
bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3) 
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

*1:10 nearest neighbour matching within a calliper (radius 0.05)
psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(10) cal(0.05)
pstest male age han edu hukou marriage wage liv,both graph
bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3) 
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

///Heterogeneity Analysis

//Individual Heterogeneity

///Table 4. Effects of Health Knowledge Acquisition on Working Hours and Overwork Possibility of Migrants with Different Wage Rates

xtile wage1= wage, nq(3)

reghdfe workhour h_dummy male age han edu hukou marriage wage liv if wage1==1,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
reghdfe workhour h_dummy male age han edu hukou marriage wage liv if wage1==2,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour h_dummy male age han edu hukou marriage wage liv if wage1==3,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==1,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==2,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==3,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) cal(0.01)
pstest male age han edu hukou marriage wage liv,both graph

bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if wage1==1 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if wage1==2 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if wage1==3 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==1 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==2 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==3 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

///Table 5. Effects of Health Knowledge Acquisition on Working Hours and Overwork Possibility of Migrants with Different Education Levels

gen edu1=1 if edu<=9
replace edu1=2 if edu==12
replace edu1=3 if edu>12

reghdfe workhour h_dummy male age han edu hukou marriage wage liv if edu1==1,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
reghdfe workhour h_dummy male age han edu hukou marriage wage liv if edu1==2,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour h_dummy male age han edu hukou marriage wage liv if edu1==3,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==1,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==2,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==3,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) cal(0.01)
pstest male age han edu hukou marriage wage liv,both graph

bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if edu1==1 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if edu1==2 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if edu1==3 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==1 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==2 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==3 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

///Table 6. Effects of Health Knowledge Acquisition on Working Hours and Overwork Possibility of Migrants with Different Hukou

reghdfe workhour h_dummy male age han edu hukou marriage wage liv if hukou==1,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
reghdfe workhour h_dummy male age han edu hukou marriage wage liv if hukou==0,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==1,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==0,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) cal(0.01)
pstest male age han edu hukou marriage wage liv,both graph

bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if hukou==1 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): reghdfe workhour h_dummy male age han edu hukou marriage wage liv if hukou==0 & _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==1 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 h_dummy male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==0 & _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

///Table 7. Effects of Wage Rate, Education Level and Household Registration on the Long-term Residence Intention of Migrants

gen live=1 if Q211==1
replace live=0 if live==.

gen l_wage=1 if wage1==1
replace l_wage=0 if l_wage==.
gen l_edu=1 if edu1==1
replace l_edu=0 if l_edu==.

logit live l_wage male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(live) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit live l_edu male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(live) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit live hukou male age han edu marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(live) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit live l_wage l_edu hukou male age han edu marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(live) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) cal(0.01)
pstest male age han edu hukou marriage wage liv,both graph

bootstrap , reps(1000): logit live l_wage male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(live) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit live l_edu male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(live) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit live hukou male age han edu marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(live) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

//Health Knowledge Heterogeneity 

///Table 8. Effects of Different Health Knowledge Acquisition on the Working Hours of Migrants

reghdfe workhour Q303A male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303B male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303C male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303D male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303E male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303F male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303G male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303H male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
reghdfe workhour Q303I male age han edu hukou marriage wage liv,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) cal(0.01)
pstest male age han edu hukou marriage wage liv,both graph

bootstrap , reps(1000): reghdfe workhour Q303A male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303B male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303C male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303D male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303E male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303F male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303G male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303H male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
bootstrap , reps(1000): reghdfe workhour Q303I male age han edu hukou marriage wage liv if _weight!=.,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

///Table 9. Effects of Different Health Knowledge Acquisition on the Overwork Possibility of Migrants

logit overwork60 Q303A male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303B male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303C male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303D male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303E male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303F male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303G male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303H male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
logit overwork60 Q303I male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

psmatch2 h_dummy male age han edu hukou marriage wage liv,ate ties logit common n(1) cal(0.01)
pstest male age han edu hukou marriage wage liv,both graph
 
bootstrap , reps(1000): logit overwork60 Q303A male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303B male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303C male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303D male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303E male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303F male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303G male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303H male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
bootstrap , reps(1000): logit overwork60 Q303I male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if _weight!=.,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

///Robustness Checks
gen h_total=Q303A +Q303D +Q303G +Q303I
bys F3 : egen total= total( h_dummy )
bys F3 : egen count= total( 1 )
gen h_mean=(total-h_dummy)/(count-1)

///Table 10. Robustness Checks: Effects of Health Knowledge Acquisition on the Working Hours and Overwork Possibility of Migrants

reghdfe workhour h_total male age han edu hukou marriage wage liv ,absorb(Occupation Industry Region) vce(cluster F3)
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,first
outreg2 using result.doc,append  tstat bdec(3) tdec(2) ctitle(workhour)addtext(province FE, YES, YEAR FE, YES)
logit overwork60 h_total male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region ,r
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,first
outreg2 using result.doc,append  tstat bdec(3) tdec(2) ctitle(overwork60)addtext(province FE, YES, YEAR FE, YES)
reg h_total h_mean male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region,vce(cluster F3)
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(h_total) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 

///Table 11. Robustness Checks: Individual Heterogeneity Analysis

ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==1,first
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==2,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==3,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==1,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==2,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if wage1==3,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==1,first
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==2,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==3,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==1,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==2,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if edu1==3,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(overwork60) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==1,first
outreg2 using result.doc,replace tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
ivreg2 workhour ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==0,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==1,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES) 
ivreg2 overwork60 ( h_total=h_mean) male age han edu hukou marriage wage liv i.Occupation i.Industry i.Region if hukou==0,first
outreg2 using result.doc,append tstat bdec(3) tdec(2) ctitle(workhour) addtext(Occupation FE, YES, Industry FE, YES, Region FE, YES)

keep ID newID pro F1 F2 F3 F6 F7 F8 q101id1 q101a1 q101b1 q101c1y q101c1m q101d1 q101e1 q101f1 q101g1 q101h1 q101i1 q101j1 q101k1y q101k1m q101l1 Q203X Q206 Q207 Q210 Q211 Q303A Q303B Q303C Q303D Q303E Q303F Q303G Q303H Q303I liv workhour overwork60 h_dummy male age han edu hukou marriage Occupation Industry Region weekwage wage wage1 edu1 live l_wage l_edu h_total total count h_mean
