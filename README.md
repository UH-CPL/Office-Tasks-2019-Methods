# Multimodal-Email-Study
-----------------
This repository contains the R scripts to curate, quality control, and validate the raw data collected
via the S-Interface and other tools in the Email Stress project.


## Getting Started
-----------------

#### Prerequisites
- R and required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')

	
## Scripts Set
-----------------
1. **Data Curation**
	- curate-and-process-physiological-data.R
		- For each group and subjects the script does the following:
			- Read the *pp.csv (raw perinasal perspiration signal file), remove noise, downsample by 1 fps
			- Read the session markers file and split the session with the PP signal.
			- Read the files from E4 and Zephyr Bioharness signal, downsample each signal and merge with PP signal.
	- nlp-processor.R
		- Note: Run the following line once:
			- initCoreNLP(mem = "4g")
	- essay-email-extractor.R
		- Gather and merge all report and email response for 63 subjects
2. **Quality Control - first level**
	- quality-control-first-phase.Rmd
		- This script is for generating the combined file for all signals, for
			- Merged 1fps data - without quality control
				- Change the variable value to FALSE: is\_filter\_data = F
			- Filtered data - quality control phase 1
				- Change the variable value to TRUE: is\_filter\_data = T
3. **Quality Control - second level**
	- quality-control-second-phase.Rmd
		- Filter data in the second phase
	- final-data-formation.R
		- Convert all the final dataset user-friendly version
4. **Validation Scripts**
	- time-series-plots.Rmd
	- validation_plots.R
	- supplementary_plots.Rmd
	- mean_signal-distribution.Rmd
	- hr-regression-and-distribution-plot.Rmd
	- c-hr-w-hr-comparison-plot.Rmd


#### Other Scripts:
- filter-pp.R
    - Script to remove noise from PP signal
- down-sample-pp.R
    - Script to downsample data to 1 frame per second
- common-functions.R
	- The common functions are used in almost all files
- score-psychometrics.R
    - The scoring calculation for psychometrics variables
