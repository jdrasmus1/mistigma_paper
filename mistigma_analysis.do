
clear
set more off

capture log close
log using "stigmaexp_05282019.log", replace

use "mistigma_data.dta"

* DATA CLEANING
	* generate new variables
		* binary married variable
		generate married=mstatus==1

		* variable aggregated across mental illness categories
		generate misassign_aggr=.
		replace misassign_aggr=1 if misassign==1
		replace misassign_aggr=2 if misassign==2 | misassign==5 | misassign==8
		replace misassign_aggr=3 if misassign==3 | misassign==6 | misassign==9
		replace misassign_aggr=4 if misassign==4 | misassign==7 | misassign==10
		label define misassign_aggr 1 "1:control" 2 "2:MI" 3 "3:MI+resp" 4 "4:MI+relapse"
		label values misassign_aggr misassign_aggr

		* binary variables for each personal belief outcome
		foreach x in punished shame {
			generate `x'_bin = `x' == 1
			replace `x'_bin =. if missing(`x')
		}
		* reverse code marry personal belief outcome
		generate marry_bin = marry != 1
		replace marry_bin =. if missing(marry)

		* reverse code other_marry perceived norms outcome
		rename other_marry other_marry_orig
		generate other_marry = 5 - other_marry_orig
		replace other_marry = other_marry_orig if missing(other_marry_orig)
		label define other_marry 1 "1:at least 90%" 2 "2:between 50% and 90%" ///
				3 "3:between 10% and 50%" 4 "4:less than 10%" .d "do not know" .k "refuses"
		label values other_marry other_marry

		* binary variables for each perceived norms outcome
		foreach x in marry punished shame {
			generate other_`x'_bin = other_`x' == 3 | other_`x' == 4
			replace other_`x'_bin =. if missing(other_`x')
		}

	* generate dummy variables to enable omodel/gologit2
		tab misassign, gen(misassign)
		tab misassign_aggr, gen(misassign_aggr)
		tab vid, gen(vid)

* ANALYSIS FOR MAIN SECTION OF PAPER
	* summary statistics (Table 1)
		tabstat age, stat(n mean sd)
		tab female, missing
		tab married, missing
		tab agecat, missing
		tab edcat, missing
		tab vid, missing

	* unadjusted findings (Table 2 & 3)
		* personal beliefs
		foreach x in marry_bin punished_bin shame_bin {
			tab `x' misassign, column missing
		}
		
		* perceived norms
		foreach x in other_marry_bin other_punished_bin other_shame_bin {
			tab `x' misassign, column missing
		}

	* regression analyses for personal beliefs (Table 4)
		foreach x in marry punished shame {
			display "* regression analysis for personal beliefs: `x'"
			poisson `x'_bin i.misassign female i.vid, vce(cluster vid) irr
		}

	* regression analyses for perceived norms (Table 5)
		foreach x in marry punished shame {
			display "* regression analysis for perceived norms: `x'"
			ologit other_`x' i.misassign female i.vid, vce(cluster vid) or

			display "* regression analysis for perceived norms: `x', Brant test"
			quietly ologit other_`x' misassign2-misassign10 female vid2-vid8, vce(cluster vid) or
			brant		
		}

* COMPARE REASSIGNED GROUP TO CORRECTLY ASSIGNED GROUP (Appendix S2)
	* Summary statistics comparing groups (S1 Table)
		tab female discrep, column
		tab married discrep, column missing
		tab agecat discrep, column missing
		tab edcat discrep, column missing
		tab vid discrep, column missing

	* Repeat analysis without people who were reassigned
		preserve
		drop if discrep == 1

		* summary statistics (S2 Table)
			tabstat age, stat(n mean sd)
			tab female, missing
			tab married, missing
			tab agecat, missing
			tab edcat, missing
			tab vid, missing

		* unadjusted findings (S3 & S4 Tables)
			* personal beliefs
			foreach x in marry_bin punished_bin shame_bin {
				tab `x' misassign, column missing
			}
			
			* perceived norms
			foreach x in other_marry_bin other_punished_bin other_shame_bin {
				tab `x' misassign, column missing
			}

		* regression analyses for personal beliefs (S5 Table)
			foreach x in marry punished shame {
				display "* regression analysis for personal beliefs: `x'"
				poisson `x'_bin i.misassign female i.vid, vce(cluster vid) irr
			}

		* regression analyses for perceived norms (S6 Table)
			foreach x in marry punished shame {
				display "* regression analysis for perceived norms: `x'"
				ologit other_`x' i.misassign female i.vid, vce(cluster vid) or

				display "* regression analysis for perceived norms: `x', Brant test"
				quietly ologit other_`x' misassign2-misassign10 female vid2-vid8, vce(cluster vid) or
				brant
			}
		restore

* ANCILLARY ANALYSES (for reviewer memo; not included in paper)
	* checking for social desirability
		foreach x in other_marry other_punished other_shame {
			tab `x' misassign, column missing
			}

		foreach x in marry_bin punished_bin shame_bin {
			foreach v of numlist 1/8 {
				bys vid: tab `x' misassign, column
			}
		}

		foreach x in other_marry other_punished other_shame {
			foreach v of numlist 1/8 {
				bys vid: tab `x' misassign, column
			}
		}

	* check whether people know people with mental illnesses (response to editor comment E1-1)
		tab know_someone misassign, missing column

	* check effect of coding depends differently or keeping missing data (response to reviewer comments R1-4,R3-8,R3-9)
		* show tabs for missing and depends results
			* personal beliefs
			foreach x in marry punished shame {
				tab misassign `x', row missing
			}

			* perceived norms
			foreach x in other_marry other_punished other_shame {
				tab misassign `x', row missing
			}

		* sensitivity analysis with "depends" recoded as missing
			* recreate _bin vars with "depends" recoded as missing
				foreach x in punished shame {
					generate `x'_dmiss = `x' == 1
					replace `x'_dmiss =. if missing(`x') | `x' == 2
				}

				generate marry_dmiss = marry != 1
				replace marry_dmiss =. if missing(marry) | marry == 2

			* regression analyses for personal beliefs (Table 4)
				foreach x in marry punished shame {
					display "* regression analysis for personal beliefs: `x'"
					poisson `x'_dmiss i.misassign female i.vid, vce(cluster vid) irr
				}

		* sensitivity analysis with "depends" recoded as non-stigmatizing
			* recreate _bin vars with "depends" recoded as non-stigmatizing
				foreach x in punished shame {
					generate `x'_dnon = `x' != 0
					replace `x'_dnon =. if missing(`x')
				}
				generate marry_dnon = marry == 0
				replace marry_dnon =. if missing(marry)

			* regression analyses for personal beliefs (Table 4)
				foreach x in marry punished shame {
					display "* regression analysis for personal beliefs: `x'"
					poisson `x'_dnon i.misassign female i.vid, vce(cluster vid) irr
				}

	* redo analysis with treatment lumped together (response to reviewer comment R1-1)
		* generate new variables
			generate misassign_treat=.
			replace misassign_treat=1 if misassign==1
			replace misassign_treat=2 if misassign==2
			replace misassign_treat=3 if misassign==3 | misassign==4
			replace misassign_treat=4 if misassign==5
			replace misassign_treat=5 if misassign==6 | misassign==7
			replace misassign_treat=6 if misassign==8
			replace misassign_treat=7 if misassign==9 | misassign==10
			label define misassign_treat 1 "1:control" 2 "2:scz" 3 "3:szc+treat" ///
			4 "4:bipolar" 5 "5:bipolar+treat" 6 "6:depress" 7 "7:depress+treat"
			label values misassign_treat misassign_treat

		* unadjusted findings (Table 2 & 3)
			* personal beliefs
			foreach x in marry_bin punished_bin shame_bin {
				tab `x' misassign_treat, column missing
			}
			
			* perceived norms
			foreach x in other_marry_bin other_punished_bin other_shame_bin {
				tab `x' misassign_treat, column missing
			}

		* regression analyses for personal beliefs (Table 4)
			foreach x in marry punished shame {
				display "* regression analysis for personal beliefs: `x'"
				poisson `x'_bin i.misassign_treat female i.vid, vce(cluster vid) irr
			}

		* regression analyses for perceived norms (Table 5)
			foreach x in marry punished shame {
				display "***** regression analysis for perceived norms: `x'"
				ologit other_`x' i.misassign_treat female i.vid, vce(cluster vid) or
			}

log close
