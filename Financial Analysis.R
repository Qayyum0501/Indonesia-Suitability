calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}

npv=calc_NPV(annual_revenue = 10670187665,lifetime_yrs = 25,CAPEX = 33011550000)
npv
ifelse(npv>0, "Support","object" )

Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.08, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 103594055000
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 
