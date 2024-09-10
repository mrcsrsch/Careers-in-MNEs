*****************************************************************************
* Estimate Combes et al 2012 method with bootstrapped p-values (using the estquant command) 
* Sets of bootstrapped worker and company fe estimates generated before, see R scripts
* Here: estquant takes main column (fe) for parameter estimates 
* and bootstrapped columns (fe1-fe100) for standard errors
*****************************************************************************
*****************************************************************************

* move to output dir
cd "/PATH TO MAIN DIR/outputs/"

*****************************************************************************

* use the log function to save full outputs on the go. 
log using outputs.txt, text replace

log off

* each model and boodstraps are saved in their own .dta file. Loop estimation procedure over them:
foreach dtafile in basic_company fullwosize_company full_company interacted_company {
	* Prepate data 
	** load data 
	use "/PATH TO MAIN DIR/data/step5/Stata_files/`dtafile'.dta", clear
	
	
	** sort data
	sort MNE fe

	** estquant also needs separate categorical variable per bootstrap
	forvalue i = 1(1)100 {
		gen MNE`i' = MNE
	}

	
	*****************************************************************************
		
	log on
	* estimate Combes et al 2012 method for shift and dilation
	estquant fe, cat(MNE) sh di bvar(on) brep(100) qrange(1000) ci(bootstrap) optech(nm)
	* store estimation result
	est store `dtafile'
	
	log off
	
}

log close

* estimations are stored in their respective file names now

* output table in .tex format
esttab basic_company fullwosize_company full_company interacted_company using table_Combes_companyfe.txt, r2 se replace title(Comparison of company fixed effects distributions, MNEs vs Domestic Companies\label{tab:Combes_company})
esttab basic_company fullwosize_company full_company interacted_company using table_Combes_companyfe.tex, r2 se replace title(Comparison of company fixed effects distributions, MNEs vs Domestic Companies\label{tab:Combes_company})
