** Time
gen int year = yofd(date)
gen int month = month(date)
gen int day = day(date)
gen long number = 10000*year + 100*month + day

recode number (20160509/20160623 = 0 "Pre-results")(0160624/20160716 = 1 "Post-results"), gen(referendum)
recode number (20160509/20160622 = 0 "Pre-results")(20160623=1 "Referendum")(0160624/20160716 = 2 "Post-results"), gen(referendum2)

tab referendum*
bysort referendum: tab c10
 

* Distance to referendum (negative; zero; )
gen time_zero = date-mdy(06,24,2016)


** Missing
mvdecode q102 q104 q301 q307ni q608 q501_* q501ni_* q404_*  q411_*, mv(77 = .r \ 88=.d)
mvdecode q607ni, mv(6/77=.r)

** Control variables
* Religious community and religiosity
recode q103ni2 (2=1)(3=0)(nonmiss=.), g(community)
replace community = 2 if community ==. 
lab val community community
lab define community 0 "Protestant"  1 "Catholic" 2 "None/other" , modify

g religiosity = 8-q104
lab var religiosity "Religiosity"

rename q101 age
rename q607ni education
tab q608, gen(employment_)

* Politics
g polinterest = 5-q301
lab var polinterest "How interested in politics"

g polpref = q307ni 
lab var polpref "Preferred political party"

recode q307ni (11 =1) (.d=1)(else=0), g(nonvote)
lab var nonvote "Would not vote/DK"

* Exposure & PTSD
recode q401 (1=1) (else=0), gen(reside)

foreach var of varlist q404_* q404g_1 {
recode `var' (1=1)(0=0)(else=0), gen(d`var')
}

egen exposure = anymatch(dq*), values(1) 
lab var exposure "Victim during Troubles"

g ptsd = (q411_1 + q411_2 + q411_3 + q411_4 + q411_5 + q411_6)/6
lab var ptsd "PTSD scale"


* Rural
recode q704 (1/2 =1)(3/5=0), gen(rural)


** Dependent variables: causes of the Troubles: 
foreach var of varlist q501_* q501ni_*  {
g i`var' = 6-`var'
}
drop iq501_7

rename iq501_1 cause_1
rename iq501_2 cause_2
rename iq501_4 cause_3
rename iq501_6 cause_4
rename iq501ni_3 cause_5
rename iq501ni_4 cause_6
rename iq501ni_1 cause_7
rename iq501ni_2 cause_8

* Number of missing values on causes
foreach var of varlist q501_* q501ni_*  {
g x`var' = 99 if `var' ==.d | `var' ==.r
}

egen causes_missing = anycount(xq501_1 xq501_2 xq501_4 xq501_6 xq501ni_1 xq501ni_2 xq501ni_3 xq501ni_4), values(99) 
tab causes_missing


** Future preferences post-Brexit
recode q603ni (1/2=1) (else=0), gen(remain)
recode q603ni (3=1) (else=0), gen(independence)
recode q603ni (4=1) (else=0),gen(unification)
recode q603ni (77=1) (88 = 1)(5=1)(else=0),gen(q603ni_missing)


** Deleting redundant variables
keep date male referendum* time_zero c10 q102 q104 q301 q307ni q608 q501_* q501ni_* q404_*  q411_* q607ni community religiosity age education employment_* q608 polinterest polpref nonvote reside dq* exposure ptsd directvictim witness indirectvictim victim_miss directvictim_miss witness_miss directvictim witness indirectvictim indirectvictim_miss rural iq* cause_* xq* causes_missing q603ni remain independence unification q603ni_missing q611

save brexit_short, replace