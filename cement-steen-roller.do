****************************************************************************************
* This do-file will generate the estimates, figures and tables in the AER
* article, except for the welfare analysis that is undertaken using Excel.
* All data and results necessary for the welfare analysis are however 
* included in the dataset "cement-steen-roller.dta", and in the demand results
* estimated in this do-file.
* Steen Röller 2006
****************************************************************************************
clear

set matsize 800
set more off

* dataset
* remember to include path:

use "      cement-steen-roller.dta", clear

tsset year

* variable description
label var  year                 				"year of observation"
label var  exp                 	 			"Norwegian Export in tons"
label var  pexp                 				"Norwegian ExportPrice in NOK per ton (nominal)"
label var  pm	                				"PriceMaterials NOK per ton (nominal) "
label var  pe	                				"PriceFuel&Electricity NOK per ton  (nominal)"
label var  way	                				"Wage NOK per year (nomina)l"
label var  mc1	                				"Short run Marginal cost measure (NOK per ton)"
*                                      				"Materials+Fuel&Electricity+all wage (nominal) "
label var  cpi                  				"Norwegian CPI index (1985=100)"
label var  pn	               				"Price Norway (nominal)"
label var  qn 	               				"Norwegian Consumption (Norwegian trade and industry statistics)"
label var  byan                 				"Norwegian Building activity index (real)"
label var rinterestrate         			"Real Interest rate"
label var rwage                 			"Real wage per produced ton"
label var y                     				"Total Norwegian production (Norwegian production numbers)"
label var predictedmc       	  		"Predicted Marginal Cost from theoretical model "
label var exporteurope          			"European exports"
label var  productioncemburea   		"Norwegian production CEMBUREAU numbers"
label var consumptioncembureau  	"Norwegian consumption CEMBUREAU numbers"
desc

* remember to include path:
save "      cement-steen-roller2.dta", replace


*deflate nominal variables
gen pmr=pm/cpi*100
gen per=pe/cpi*100
gen wayr=way/cpi*100
gen pexpr=pexp/cpi*100
gen pnr=pn/cpi*100
gen shortrunAC=mc1/cpi*100


*summary statistics, Table 1 
summarize pexpr pnr byan qn y exp pmr per wayr  if year>1954 & year<1983


* Demand estimation, Table 2 results

* Shazam 7 was used in the original demand estimation that treats lags differntly
* from STATA. In the Shazam routine the first observation (1955) in the 
* generation and tests of elasticities was removed. 
* In order to replicate the elasticities in Table 2 the means for "qn" and "pnr" 
* therefore have to be generated for the period 1956 to 1982.

* The elasticities where also 1955 is included in the means for  "qn" and "pnr" 
* only differ marginally from the elasticities presented in the paper. 
* The short run demand elasticity in this case is now -0.462 as opposed 
* to -0.455 in the paper, and the long run elasticity is -1.49 as opposed 
* to -1.47 in the paper. 
* The short run elasticity is still significant at a 5% level, 
* whereas the long run elasticity is still non-significant. 

egen mqn=mean(qn) if year>1955 & year<1983
egen mpnr=mean(pnr) if year>1955 & year<1983

* here we impose the period 1955 to 1982 as sample for the demand estimation
drop if year<1955
drop if year>1982

* here we make shure that the means generated above also are defined for 1955
replace mqn=1415858
replace mpnr=524.0771 

* the demand model estimation and elasticity calculation/tests (Table 2 results)
ivreg pnr (qn=wayr pmr per pexpr) byan l.pnr l.qn l.byan

*short run elasticity, Table 2
nlcom ((1/_b[qn])*(mpnr/mqn))

*long run elasticity, Table 2
nlcom (((1-_b[l.pnr]) / (_b[qn]+_b[l.qn])) *(mpnr/mqn))

*adjustment speed, Table 2
nlcom (1-_b[l.pnr])

*autocorrelation tests
predict residual, resid

*Q1, Table 2
wntestq residual, lags(1)

*Q4, Table 2
wntestq residual, lags(4)


*The figures in the paper are also shown here. Note however that Excel 
* was used to generate all our figures in the paper and therefore the 
* appearance of the STATA generated figures will differ from 
* the figures displayed in the paper. In particular in those cases 
* where we allow for two different axes with different scale in the 
* Excel generated figures. 
* To regenerate the figures we need the full dataset:

* remember to include path:
use    cement-steen-roller.dta, clear

tsset year
*deflate variables again
gen pmr=pm/cpi*100
gen per=pe/cpi*100
gen wayr=way/cpi*100
gen pexpr=pexp/cpi*100
gen pnr=pn/cpi*100
gen shortrunAC=mc1/cpi*100


*Figure 1 - production and domestic consumption in Norway

* This can be drawn from two data sources. Either using 
* CEMBUREAU data, or the Norwegian production data. 
* The Figures thatare used in Figure 1 is based on the 
* CEMBUREAU data and looks like: 
twoway (tsline   productioncemburea)(tsline consumptioncembureau) if year>1954 & year<1969
* the Norwegian numbers provides of course a very similar picture 
* since the Norwegian production and consumption data only differs 
* in scale and are correlated with the CEMBUREAU numbers with 0.99 and 0.096
* correspondingly
gen y1000tonnes=y/1000
gen qn1000tonnes=qn/1000
twoway (tsline  y1000tonnes qn1000tonnes) if year>1954 & year<1969

*Figure 2 - European and Norwegian exports
twoway (tsline  exporteurope)(tsline exp, yaxis(2)) if year>1954

*Figure 4 consistency of predicted marginal cost
twoway (tsline predictedmc  pexpr pnr) if year>1954 & year<1969

*Figure 5 factor prices and  predicted MC
twoway (tsline predictedmc)(tsline rwage per pmr,yaxis(2) ) if year>1954 & year<1969

*Figure 6 shortrun AC and predicted MC
twoway (tsline predictedmc  shortrunAC) if year>1954 & year<1969

clear

* Now we load the saved dataset with variable labels
* remember to include path:
use    cement-steen-roller2.dta, clear

tsset year
