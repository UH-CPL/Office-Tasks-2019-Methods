# Multimodal-Email-Study
This repository contains the R scripts to curate, quality control, and validate the raw data collected
via the S-Interface and other tools in the Email Stress project.


## Getting Started

#### Prerequisites
- R and required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')

	
## Script Set
##### Please run the following scripts sequentially
1. **Data Curation**
	- dc-curate-and-process-physiological-data.R
		- For each group and subjects the script does the following:
			- Read the *pp.csv (raw perinasal perspiration signal file), remove noise, downsample by 1 fps
			- Read the session markers file and split the session with the PP signal.
			- Read the files from E4 and Zephyr Bioharness signal, downsample each signal and merge with PP signal.
	- dc-nlp-processor.R
		- Note: Run the following line once:
			- initCoreNLP(mem = "4g")
	- dc-essay-email-extractor.R
		- Gather and merge all report and email response for 63 subjects
2. **Quality Control - first level**
	- qc-first-phase.Rmd
		- This script is for generating the combined file for all signals, for
			- Merged 1fps data - without quality control
				- Change the variable value to FALSE: is\_filter\_data = F
			- Filtered data - quality control phase 1
				- Change the variable value to TRUE: is\_filter\_data = T
3. **Quality Control - second level**
	- qc-second-phase.Rmd
		- Filter data in the second phase
4. **Validation Scripts**
	- vs-supplementary-plots.Rmd
	- vs-time-series-plots.Rmd
	- vs-validation-plots.R
	- vs-mean-signal-distribution.Rmd
	- vs-hr-regression-and-distribution-plot.Rmd
	- vs-c-hr-w-hr-comparison-plot.Rmd
	- vs-performance-data-analysis.Rmd
	- vs-questionnaire-data-analysis.Rmd
	- vs-time-series-plot-hrv.R
	- vs-validation-plot-hrv.R
5. **Utility Scripts**
	- dc-format-final-data.R
		- Convert all the final dataset user-friendly version
		- **Note:** This is the last script to run. **Please do not run any script following.**
	- common-functions.R
		- The functions from the script are called from almost all scripts
	- dc-filter-pp.R
		- This script is called from dc-curate-and-process-physiological-data.R
	    - Script to remove noise from PP signal
	- dc-down-sample-pp.R
		- This script is called from dc-curate-and-process-physiological-data.R
	    - Script to downsample data to 1 frame per second
	- dc-score-psychometrics.R
		- This script is called from vs-questionnaire-data-analysis.Rmd
	    - The scoring calculation for psychometrics variables
