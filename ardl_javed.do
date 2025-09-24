*------------------------------------------------*
*   ARDL Regressions on Selected Export Vars     *
*   Final Working Version - No Name Conflicts    *
*------------------------------------------------*

clear all
set more off

*------------------------------------------------*
* Step 1: Load and Prepare Data
*------------------------------------------------*

import delimited "D:/RepTemplates/machine_learning/export_modified.csv", clear

* Convert macro vars to numeric if needed
foreach var in ipi_japan ipi_india ind_jap_bila_exch volatility {
    capture confirm numeric variable `var'
    if _rc != 0 {
        destring `var', replace force
    }
}

* Keep only selected variables
keep year aluminium anim_veg_oils_fats_processed ///
    articles_of_artificial_plastic_m ///
    articles_of_paper_pulp_paperboar ///
    cereal_preps_preps_of_flour_of_f ///
    ipi_japan ipi_india ind_jap_bila_exch volatility

* Keep only data up to 2015
keep if year <= 2015

* Set time variable
tsset year
sort year

*------------------------------------------------*
* Step 2: Define Export and Macro Variables
*------------------------------------------------*

local export_vars aluminium anim_veg_oils_fats_processed ///
    articles_of_artificial_plastic_m ///
    articles_of_paper_pulp_paperboar ///
    cereal_preps_preps_of_flour_of_f

global macrovars ipi_japan ipi_india ind_jap_bila_exch volatility

*------------------------------------------------*
* Step 3: Fill Missing Values (LOCF)
*------------------------------------------------*

foreach var of varlist `export_vars' $macrovars {
    gen __tmpfill = `var'
    replace __tmpfill = __tmpfill[_n-1] if missing(__tmpfill) & _n > 1
    replace `var' = __tmpfill
    drop __tmpfill
}

foreach var of varlist `export_vars' $macrovars {
    drop if missing(`var')
}

*------------------------------------------------*
* Step 4: Run ARDL Regressions
*------------------------------------------------*

cap mkdir "D:/RepTemplates/machine_learning/reg_results"
eststo clear

local i = 1
foreach var of local export_vars {

    di as txt "Running ARDL for `var'..."

    * Generate lag of dependent variable
    gen L1_x = L.`var'

    * Generate lagged macro variables
    local indep_vars L1_x
    local j = 1
    foreach mvar of global macrovars {
        gen L1_m`j' = L.`mvar'
        local indep_vars `indep_vars' L1_m`j'
        local ++j
    }

    * Run regression
    capture noisily regress `var' `indep_vars'

    if _rc == 0 {
        local modelname = model`i'
        eststo `modelname'
        di as result "✅ Stored `modelname' for `var'"
    }
    else {
        di as error "❌ Regression failed for `var'"
    }

    * Clean up lag vars
    capture drop L1_*

    local ++i
}

*------------------------------------------------*
* Step 5: Export Results
*------------------------------------------------*

eststo dir

if `i' > 1 {
    esttab using "D:/RepTemplates/machine_learning/reg_results/ARDL_selected_results.rtf", ///
        replace se label title("ARDL Regressions - Selected Export Variables") ///
        star(* 0.10 ** 0.05 *** 0.01)
    di as result "📄 Results saved to: ARDL_selected_results.rtf"
}
else {
    di as error "⚠️ No models stored. Nothing to export."
}
