# Multimodal-Email-Study
This repository contains the R scripts to curate, ensure quality control, and validate the raw data collected
via the S-Interface and other tools in the "Office Tasks 2019" project.


## Getting Started

#### Prerequisites
- R and required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')

	
## Script Set
##### Please run the following scripts sequentially
1. **Data Curation (dc)**
	- dc-curate-and-process-physiological-data.R
		- For each participant the script does the following:
			- Reads the *pp.csv (original perinasal perspiration signal data), removes noise, downsamples to 1 frame per second (fps).
			- Reads the session markers file and splits each PP signal into treatment segments.
			- Reads the E4 and BioHarness signal files, downsamples them, and merges them with the PP signal file.
	- dc-nlp-processor.R
		- Note: Run the following line once:
			- initCoreNLP(mem = "4g")
	- dc-essay-email-extractor.R
		- Gathers and merges all report and email responses for n=63 participants.
2. **Quality Control - first level (qc1)**
	- qc-first-phase.Rmd
		- Generates the combined file for all signals:
			- Unfiltered data option (@1 fps)
				- Set the variable value to FALSE: is\_filter\_data = F
			- Quality Control 1 data option (@1 fps)
				- Set the variable value to TRUE: is\_filter\_data = T
3. **Quality Control - second level (qc2)**
	- qc-second-phase.Rmd
		- Performs a second level of filering (quality control 2)
4. **Validation Scripts (vs)**
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
		- Converts the curated dataset into a user-friendly version
		- **Note:** This is the last script to run. **Please do not run any script after this.**
	- common-functions.R
		- Useful functions that are called from almost all scripts
	- dc-filter-pp.R
	    - Removes noise from PP signals. It is called from dc-curate-and-process-physiological-data.R
	- dc-down-sample-pp.R
	    - Downsamples data to 1 fps. It is called from dc-curate-and-process-physiological-data.R
	- dc-score-psychometrics.R
	    - Scores psychometric variables. It is called from vs-questionnaire-data-analysis.Rmd
