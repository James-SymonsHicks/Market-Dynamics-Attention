clear
use "FTSE_PricingData_Full_Clean_Py.dta", replace /// Using the cleaned dataset (Refinitiv data)

** INDICATORS:
sort id date
gen month = mofd(date)
label var month "Month Indicator" /// Month-level indicator

gen date_num = string(date)
destring date_num, replace
gen date7 = date_num - 20820
gen weekind = floor(date7 /7) + 1
label var weekind "Indicator for Week No." /// Week-level indicator
drop date_num date7

*SPREAD DATA:
gen spread = askprice - bidprice
label var spread "Bid-Ask Spread"
drop if spread < 0
gen midpoint = (askprice + bidprice)/2
label var midpoint "Bid-Ask Midpoint"

*RELATIVE SPREADS
gen rel_spread = spread / midpoint 
label var rel_spread "Spread / M.P"
gen per_relspread = rel_spread * 100
label var per_relspread "Relative Spread in %"

*RETURN DATA:
sort id date
by id: gen ret1d = (close[_n]/close[_n-1])-1
label var ret1d "1-Day Return"
by id: gen ret5d = (close[_n]/close[_n-5])-1
label var ret5d "5-Day Return"

*MEAN RETURNS BY STOCK
by id: egen meanret1d = mean(ret1d)
label var meanret1d "Average 1-Day Return By Stock"
by id: egen meanret5d = mean(ret5d)
label var meanret5d "Average 5-Day Return By Stock"

*AMIHUD ILLIQUIDITY
gen illiq = abs(ret1d)/turnover

*VOLATILITY MEASURES:
*HIGH-LOW RANGE:
gen high_low = highprice - lowprice
label var high_low "High_t - Low_t"
gen logh_logl = log(highprice)-log(lowprice)
label var logh_logl "Log(high)-Log(low)"


*WEEKLY STOCK VOLATILITY
gen retdiffsq = (ret1d - meanret1d)^2
label var retdiffsq "(R_t - R\bar)^2"
bysort id weekind: gen count2 = _N
label var count2 "Number of observations in week"

/// Computing the weekly variance (volatility measure 1) as described in the methodology section.
sort id weekind
by id weekind: egen sum_retdiffsqwk = sum(retdiffsq)
label var sum_retdiffsqwk "Weekly Sum((R_t - R\bar)^2)"
by id: gen vol2_1w = 1/((count2)-1) * sum_retdiffsqwk if count2 > 1
label var vol2_1w "Weekly Variance"

gen vol_1w = (vol2_1w)^(1/2) /// Weekly standard deviation of returns.
label var vol_1w "Weekly Std Dev"

*AVERAGE MARKET CAP:
sort id date
by id: egen avg_mc = mean(marketcap)
label var avg_mc "Average Market Cap of Stock over Sample"

gen logilliq = log(illiq)
gen logvolume = log(volume)
gen logturnover = log(turnover)
gen logmarketcap = log(marketcap)

/// Averaging variables over weeks.
sort id weekind
by id weekind: egen logvolume1w = mean(logvolume) 		/// Weekly average trading volume
by id weekind: egen logmarketcap1w = mean(logmarketcap) /// Weekly average market cap
by id weekind: egen logturnover1w = mean(logturnover) 	/// Weekly average turnover
by id weekind: egen spread1w = mean(spread) 			/// Weekly average spread
by id weekind: egen relspread1w = mean(rel_spread) 		/// Weekly average relative spread
by id weekind: egen ret1d_1w = mean(ret1d) 				/// Weekly average daily returns
by id weekind: egen ret5d_1w = mean(ret5d) 				/// Weekly average 5-day returns
by id weekind: egen highlow1w = mean(high_low) 			/// Weekly average daily high-low prices
by id weekind: egen loghl_1w = mean(logh_logl) 			/// Weekly average log high-low prices
by id weekind: egen log_illiq_1w = mean(logilliq) 		/// Weekly average log illiquidity (Amihud) measure 

/// Generating dummies for market cap sizes.
gen mc25 =0
gen mc50 =0
gen mc75 =0
replace mc25 =1 if avg_mc < 6.10e+09
replace mc50 =1 if inrange(avg_mc, 6.10e+09, 2.13e+10)
replace mc75 =1 if avg_mc > 2.13e+10

sort id date
duplicates drop symbol weekind, force

merge m:m symbol weekind using "Google_Trends-Stock_Symbols_Clean.dta" /// Merging with Google Trends data.

/// Cleaning following data merging.
drop if _merge==1
drop if _merge==2
drop _merge
xtset id weekind

log using "ET_All_Models_Output", replace
*Overall models
** ----------------------------- MODEL 1 -----------------------------
xtreg interest_index vol_1w logturnover1w spread1w logmarketcap1w

newey interest_index L(1/2).interest_index L(0/3).vol_1w logturnover1w spread1w logmarketcap1w, lag(5) force

/// 2-stage fixed-effects HAC regression:
xtreg interest_index L(1/2).interest_index L(0/3).vol_1w logturnover1w spread1w logmarketcap1w, fe
predict resid1, u
newey interest_index L(1/2).interest_index L(0/3).vol_1w logturnover1w spread1w logmarketcap1w resid1, lag(5) force


** ----------------------------- MODEL 2 -----------------------------
xtreg interest_index loghl_1w logturnover1w spread1w logmarketcap1w

newey interest_index L(1/2).interest_index L(0/2).loghl_1w logturnover1w spread1w logmarketcap1w, lag(5) force

/// 2-stage fixed-effects HAC regression:
xtreg interest_index L(1/2).interest_index L(0/2).loghl_1w logturnover1w spread1w logmarketcap1w, fe
predict resid2, u
newey interest_index L(1/2).interest_index L(0/2).loghl_1w logturnover1w spread1w logmarketcap1w resid2, lag(5) force


** ----------------------------- MODEL 3 -----------------------------
xtreg interest_index relspread1w logturnover1w logmarketcap1w 

newey interest_index L(1/2).interest_index L(0/3).relspread1w logturnover1w logmarketcap1w, lag(5) force

/// 2-stage fixed-effects HAC regression:
xtreg interest_index L(1/2).interest_index L(0/3).relspread1w logturnover1w logmarketcap1w, fe
predict resid3, u
newey interest_index L(1/2).interest_index L(0/3).relspread1w logturnover1w logmarketcap1w resid3, lag(5) force


** ----------------------------- MODEL 4 -----------------------------
xtreg interest_index log_illiq_1w logturnover1w logmarketcap1w

newey interest_index L(1/2).interest_index L(0/4).log_illiq_1w logturnover1w logmarketcap1w, lag(5) force

/// 2-stage fixed-effects HAC regression:
xtreg interest_index L(1/2).interest_index L(0/4).log_illiq_1w logturnover1w logmarketcap1w, fe
predict resid4, u
newey interest_index L(1/2).interest_index L(0/4).log_illiq_1w logturnover1w logmarketcap1w resid4, lag(5) force


** ----------------------------- MODEL 5 -----------------------------
xtreg interest_index ret1d_1w logturnover1w spread1w logmarketcap1w

newey interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w, lag(5) force

/// 2-stage fixed-effects HAC regression:
xtreg interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w, fe
predict resid5, u
newey interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w resid5, lag(5) force


** ----------------------------- MODEL 6 -----------------------------
xtreg interest_index ret5d_1w logturnover1w spread1w logmarketcap1w

newey interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w, lag(5) force

/// 2-stage fixed-effects HAC regression:
xtreg interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w, fe
predict resid6,u
newey interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w resid6, lag(5) force


** ----------------------------- MODEL 7 -----------------------------
xtreg interest_index logvolume1w logturnover1w spread1w logmarketcap1w

newey interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w, lag(5) force

/// 2-stage fixed-effects HAC regression:
xtreg interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w, fe
predict resid7, u
newey interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w resid7, lag(5) force




*Models by market capitalisation
/// Note that all models in this section use the 2-stage fixed-effects HAC regression. I.e., regressing using first-stage fixed effects regression residuals.
** ----------------------------- MODEL 1 -----------------------------
xtreg interest_index L(1/2).interest_index L(0/3).vol_1w logmarketcap1w spread1w logturnover1w if mc25==1, fe
predict resid1_mc25, u
newey interest_index L(1/2).interest_index L(0/3).vol_1w logmarketcap1w spread1w logturnover1w resid1_mc25 if mc25==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/3).vol_1w logmarketcap1w spread1w logturnover1w if mc50==1, fe
predict resid1_mc50, u
newey interest_index L(1/2).interest_index L(0/3).vol_1w logmarketcap1w spread1w logturnover1w resid1_mc50 if mc50==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/3).vol_1w logmarketcap1w spread1w logturnover1w if mc75==1, fe
predict resid1_mc75, u
newey interest_index L(1/2).interest_index L(0/3).vol_1w logmarketcap1w spread1w logturnover1w resid1_mc75 if mc75==1, lag(5) force


** ----------------------------- MODEL 2 -----------------------------
xtreg interest_index L(1/2).interest_index L(0/2).loghl_1w logmarketcap1w spread1w logturnover1w if mc25==1, fe
predict resid2_mc25, u
newey interest_index L(1/2).interest_index L(0/2).loghl_1w logmarketcap1w spread1w logturnover1w resid2_mc25 if mc25==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/2).loghl_1w logmarketcap1w spread1w logturnover1w if mc50==1, fe 
predict resid2_mc50, u
newey interest_index L(1/2).interest_index L(0/2).loghl_1w logmarketcap1w spread1w logturnover1w resid2_mc50 if mc50==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/2).loghl_1w logmarketcap1w spread1w logturnover1w if mc75==1, fe
predict resid2_mc75, u
newey interest_index L(1/2).interest_index L(0/2).loghl_1w logmarketcap1w spread1w logturnover1w resid2_mc75 if mc75==1, lag(5) force


** ----------------------------- MODEL 3 -----------------------------
xtreg interest_index L(1/2).interest_index L(0/3).relspread1w logmarketcap1w logturnover1w if mc25==1, fe
predict resid3_mc25, u
newey interest_index L(1/2).interest_index L(0/3).relspread1w logmarketcap1w logturnover1w resid3_mc25 if mc25==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/3).relspread1w logmarketcap1w logturnover1w if mc50==1, fe
predict resid3_mc50, u
newey interest_index L(1/2).interest_index L(0/3).relspread1w logmarketcap1w logturnover1w resid3_mc50 if mc50==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/3).relspread1w logmarketcap1w logturnover1w if mc75==1, fe
predict resid3_mc75, u
newey interest_index L(1/2).interest_index L(0/3).relspread1w logmarketcap1w logturnover1w resid3_mc75 if mc75==1, lag(5) force


** ----------------------------- MODEL 4 -----------------------------
xtreg interest_index L(1/2).interest_index L(0/4).log_illiq_1w logmarketcap1w logturnover1w if mc25==1, fe
predict resid4_mc25, u
newey interest_index L(1/2).interest_index L(0/4).log_illiq_1w logmarketcap1w logturnover1w resid4_mc25 if mc25==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/4).log_illiq_1w logmarketcap1w logturnover1w if mc50==1, fe
predict resid4_mc50, u
newey interest_index L(1/2).interest_index L(0/4).log_illiq_1w logmarketcap1w logturnover1w resid4_mc50 if mc50==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/4).log_illiq_1w logmarketcap1w logturnover1w if mc75==1, fe
predict resid4_mc75, u
newey interest_index L(1/2).interest_index L(0/4).log_illiq_1w logmarketcap1w logturnover1w resid4_mc75 if mc75==1, lag(5) force


** ----------------------------- MODEL 5 -----------------------------
xtreg interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w if mc25==1, fe
predict resid5_mc25, u
newey interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w resid5_mc25 if mc25==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w if mc50==1, fe
predict resid5_mc50, u
newey interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w resid5_mc50 if mc50==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w if mc75==1, fe
predict resid5_mc75, u
newey interest_index L(1/2).interest_index L(0/2).ret1d_1w logturnover1w spread1w logmarketcap1w resid5_mc75 if mc75==1, lag(5) force


** ----------------------------- MODEL 6 -----------------------------
xtreg interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w if mc25==1, fe
predict resid6_mc25, u
newey interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w resid6_mc25 if mc25==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w if mc50==1, fe
predict resid6_mc50, u
newey interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w resid6_mc50 if mc50==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w if mc75==1, fe
predict resid6_mc75, u
newey interest_index L(1/2).interest_index L(0/4).ret5d_1w logturnover1w spread1w logmarketcap1w resid6_mc75 if mc75==1, lag(5) force


** ----------------------------- MODEL 7 -----------------------------
xtreg interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w if mc25==1, fe
predict resid7_mc25, u
newey interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w resid7_mc25 if mc25==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w if mc50==1, fe
predict resid7_mc50, u
newey interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w resid7_mc50 if mc50==1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w if mc75==1, fe
predict resid7_mc75, u
newey interest_index L(1/2).interest_index L(0/3).logvolume1w logturnover1w spread1w logmarketcap1w resid7_mc75 if mc75==1, lag(5) force


**** COMBINED MODELS ****
xtreg interest_index L(1/2).interest_index L(0/3).vol_1w L(0/3).spread1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w, fe
predict comb_resid1, u
newey interest_index L(1/2).interest_index L(0/3).vol_1w L(0/3).spread1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w comb_resid1, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/3).vol_1w L(0/4).log_illiq_1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w, fe
predict comb_resid2, u
newey interest_index L(1/2).interest_index L(0/3).vol_1w L(0/4).log_illiq_1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w comb_resid2, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/2).loghl_1w L(0/3).spread1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w, fe
predict comb_resid3, u
newey interest_index L(1/2).interest_index L(0/2).loghl_1w L(0/3).spread1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w comb_resid3, lag(5) force


xtreg interest_index L(1/2).interest_index L(0/2).loghl_1w L(0/4).log_illiq_1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w, fe
predict comb_resid4, u
newey interest_index L(1/2).interest_index L(0/2).loghl_1w L(0/4).log_illiq_1w L(0/2).ret1d_1w L(0/4).ret5d_1w L(0/3).logvolume1w logturnover1w logmarketcap1w comb_resid4, lag(5) force



** ROBUSTNESS CHECKS
gen noisy_x_vol = noisy * vol_1w
xtreg interest_index L(1/2).interest_index L(0/3).vol_1w L(0/3).noisy_x_vol logmarketcap1w spread1w logturnover1w, fe
predict resid1_rb, u
newey interest_index L(1/2).interest_index L(0/3).vol_1w L(0/3).noisy_x_vol logmarketcap1w spread1w logturnover1w resid1_rb, lag(5) force

test noisy_x_vol L.noisy_x_vol L2.noisy_x_vol L3.noisy_x_vol

gen noisy_x_logrange = noisy * loghl_1w
xtreg interest_index L(1/2).interest_index L(0/2).loghl_1w L(0/2).noisy_x_logrange logmarketcap1w spread1w logturnover1w, fe
predict resid2_rb, u
newey interest_index L(1/2).interest_index L(0/2).loghl_1w L(0/2).noisy_x_logrange logmarketcap1w spread1w logturnover1w resid2_rb, lag(5) force

test noisy_x_logrange L.noisy_x_logrange L2.noisy_x_logrange

gen noisy_x_relspread = noisy * relspread1w
xtreg interest_index L(1/2).interest_index L(0/3).relspread1w L(0/3).noisy_x_relspread logmarketcap1w logturnover1w, fe
predict resid3_rb, u
newey interest_index L(1/2).interest_index L(0/3).relspread1w L(0/3).noisy_x_relspread logmarketcap1w logturnover1w resid3_rb, lag(5) force

test noisy_x_relspread L.noisy_x_relspread L2.noisy_x_relspread L3.noisy_x_relspread

gen noisy_x_illiq = noisy * log_illiq_1w
xtreg interest_index L(1/2).interest_index L(0/4).log_illiq_1w L(0/4).noisy_x_illiq logmarketcap1w logturnover1w, fe 
predict resid4_rb, u
newey interest_index L(1/2).interest_index L(0/4).log_illiq_1w L(0/4).noisy_x_illiq logmarketcap1w logturnover1w resid4_rb, lag(5) force

test noisy_x_illiq L.noisy_x_illiq L2.noisy_x_illiq L3.noisy_x_illiq L4.noisy_x_illiq

gen noisy_x_ret1d = noisy * ret1d_1w
xtreg interest_index L(1/2).interest_index L(0/2).ret1d_1w L(0/2).noisy_x_ret1d logmarketcap1w spread1w logturnover1w, fe
predict resid5_rb, u
newey interest_index L(1/2).interest_index L(0/2).ret1d_1w L(0/2).noisy_x_ret1d logmarketcap1w spread1w logturnover1w resid5_rb, lag(5) force

test noisy_x_ret1d L.noisy_x_ret1d L2.noisy_x_ret1d

gen noisy_x_ret5d = noisy * ret5d_1w
xtreg interest_index L(1/2).interest_index L(0/4).ret5d_1w L(0/4).noisy_x_ret5d logmarketcap1w spread1w logturnover1w, fe
predict resid6_rb, u
newey interest_index L(1/2).interest_index L(0/4).ret5d_1w L(0/4).noisy_x_ret5d logmarketcap1w spread1w logturnover1w resid6_rb, lag(5) force

test noisy_x_ret5d L.noisy_x_ret5d L2.noisy_x_ret5d L3.noisy_x_ret5d L4.noisy_x_ret5d

gen noisy_x_logvolume = noisy * logvolume1w
xtreg interest_index L(1/2).interest_index L(0/3).logvolume1w L(0/3).noisy_x_logvolume logmarketcap1w spread1w logturnover1w, fe
predict resid7_rb, u
newey interest_index L(1/2).interest_index L(0/3).logvolume1w L(0/3).noisy_x_logvolume logmarketcap1w spread1w logturnover1w resid7_rb, lag(5) force

test noisy_x_logvolume L.noisy_x_logvolume L2.noisy_x_logvolume L3.noisy_x_logvolume

log close
translate "ET_All_Models_Output.smcl" "ET_All_Models_Output.pdf", replace
