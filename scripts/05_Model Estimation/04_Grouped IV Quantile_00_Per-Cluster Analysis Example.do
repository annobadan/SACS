
********************************************************************************
* Initial Data Setup *
********************************************************************************
use http://www.stata-press.com/data/mlmus3/hsb, clear
keep schoolid mathach sector ses
describe

/* The variables of interest are:
sector (wj) = cluster-level indicator for whether school is Catholic
ses (xij) = unit-level covariate that indexes students' socioeconomic status
mathach (yij) = outcome, students' performance on a math test
schoolid = cluster (school) identifier */

*Define cluster identifier
xtset schoolid

*Generate cross-level interaction term between ses and sector (xij*wj)
generate sesXsector = ses*sector


********************************************************************************
* Random Effects (REML) *
********************************************************************************
/* Estimation is done in one step using covariates that are unit-level,
cluster-level, and interactions between the two. Note: ses is the covariate
with a random slope */

mixed mathach ses sector sesXsector || schoolid: ses, ///
covariance(unstructured) reml


********************************************************************************
* Augmented Fixed Effects (FE+) *
********************************************************************************

***STEP 1 - FE
**Estimate coefficients of unit-level covariates using standard fixed effects
xtreg mathach ses sesXsector, fe

****STEP 2 - Regress quasi-residuals on cluster-level covariate
**Generate "newy" as residuals from the first stage regression
generate ynew = mathach - _b[ses]*ses - _b[sesXsector]*sesXsector

**Regress the residuals on the cluster-level covariate, with cluster-robust SEs
regress ynew sector, vce(cluster schoolid)


********************************************************************************
* Per-Cluster Regression (PC) *
********************************************************************************

***Step 1
**Not needed because there are no unit-level covariates that do not have
**random slopes (R3=0)

***Step 2
**For each cluster, regress outcome on unit-level covariate using OLS, saving
**estimates of the intercepts (a1) and coefficients (a2) in statsby_HSB.dta
statsby a1=_b[_cons] a2=_b[ses] , by(schoolid) saving(statsby_HSB, replace): ///
regress mathach ses

**Merge estimates into dataset (after sorting data according to schoolid)
sort schoolid
merge m:1 schoolid using statsby_HSB

***Step 3
/* Part a: Regress intercept estimates (a1) on cluster-level covariate,
using 1 observation per cluster - OLS with robust SEs */

**Create indicator for 1 observation per cluster (it doesn't matter which one)
egen pickone = tag(schoolid)

**OLS for 1 observation per cluster, with robust SEs
regress a1 sector if pickone==1, vce(robust)

*=>estimated intercept (_cons) = estimated intercept of model (gamma0)
*=>estimated coefficient of sector = estimate coefficient of sector (gamma1)


/* Part b: Regress coefficient estimates (a2) on the cluster-level covariates,
using 1 observation per cluster - OLS with robust SEs */
regress a2 sector if pickone==1, vce(robust)

*=>estimated intercept (_cons) = estimated coefficient of ses (beta1)
*=>estimated coefficient of sector = estimated interaction parameter (beta2)


