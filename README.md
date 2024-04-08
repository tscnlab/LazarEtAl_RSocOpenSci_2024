# Overview
This R code and data repository is part of the registered report: **Regulation of pupil size in natural vision across the human lifespan** and publicly accessible under the [MIT](https://opensource.org/license/mit/) license (see `LICENSE.md` file). The laboratory log file (`lablog_RSOS-191613.csv`) provides an overview of the most relevant metadata for all invited participants.

The stage 1 in principal accepted (IPA) manuscript RSOS-191613.R1 is available on [OSF](https://osf.io/zrksf/). Additional supporting materials are available on [FigShare](https://doi.org/10.6084/m9.figshare.24230890.v1) under the [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/) license. If you have any comments or queries, please reach out to us at rafael.lazar@unibas.ch and manuel.spitschan@tum.de.

<span style="color: red;"> **Shortcut to hypothesis testing:** </span>

You can find the results of the hypothesis tests as output from the  `hypotheses.rmd` file in two versions for the two respective samples:

1. n=83,  75% data loss threshold: `hypotheses_n83.html`
2. n=63, 50% data loss threshold: `hypotheses_n63.html`


# Dependency management
This R project applies [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package management. Use `renv::restore()` to download the correct package versions and ensure computational reproducibility. For running the R code files in RStudio, make sure to use an R-Project file (e.g. the uploaded `RSOS-191613.Rproj`) to avoid issues with the working directory.


# Workflow description 
Here we explain the R script processing and any manual steps in sequential order. 

## Survey data
<span style="color: green;">

Folder: `01_surveydata` <br>
Input: `data_chronobiology_2022-04-07_21-20.csv` <br>
Output: `cleaned_survey.rda` <br>
RCode: `10_surveydata_prep.R` <br>

</span>

The health and eligibility screening including demographic data are given in this `01_surveydata`folder. The data file containing all survey data from the project is `data_chronobiology_2022-04-07_21-20.csv`. The R script `10_surveydata_prep.R` is divided into the following sections: 

- <span style="color: blue;">*[1] Data import*</span> imports the csv file generated from the survey.
- <span style="color: blue;">*[2] Data preparation*</span> filters, renames and relabels the variables.
- <span style="color: blue;">*[3] Data transformation*</span> uses the data from the survey to create further interpretable variables.
- <span style="color: blue;">*[4] Data mistake corrections*</span> makes corrections to spelling mistakes and missing values in the survey data
- <span style="color: blue;">*[5] Data saving*</span> saves the survey data in a cleaned R data file as `cleaned_survey.rda`.

<span style="color: red;">**Note**</span>: We also uploaded the resulting `cleaned_survey.rda` file to allow continued analyses in case of problems with the survey data import into R. Import issues can typically arise due to differences in OS and time and language settings.

## Raw data
<span style="color: green;">

Folder: `02_rawdata` <br>
Input: `results.csv` of 113 subjects (`SP001`-`SP113`) <br>
Output: `rawfiles.rda` <br>
RCode: `20_rawdata_import.R` <br>

</span>

The `20_rawdata_import.R` conducts the raw data import, reading in the rawdata csv files and extracting their inclusion/exclusion status from the subject-specific subfolder name and finally renaming some variables. The raw data csv files are given in subject-specific subfolders of the `02_rawdata` folder (`SP001`-`SP113`). The subject-specific subfolders of the participants excluded before the trial contain a results file with the same header but only one row NA as data. The imported data are saved as `rawfiles.rda`

<span style="color: red;">**Note**</span>: We also uploaded the resulting `rawfiles.rda` file to allow continued analyses in case of problems with the survey data import into R. Import issues can typically arise due to differences in OS and time and language settings.

## Quality checks
<span style="color: green;">

Folder: `02_rawdata` <br>
Input: `rawfiles.rda` <br>
Output: `checked_rawfiles.rda` <br>
RCode: `21_qualitychecks.R` <br>

</span>

Code section <span style="color: blue;">*[1] Load raw data*</span> loads the `rawfiles.rda` file generated from the `20_rawdata_import.R` script.

The Code section <span style="color: blue;">*[2] Data cleaning & quality checks*</span> applies the data quality checks. This includes "Lack of good-quality fit", "Pupil size screening", and "Saturated spectroradiometer samples", where invalid pupil and light data are replaced by "NA" values. And finally the "Proportion of excluded data" quality check, which compares the proportion of excluded data points per participant vs. the specified data loss threshold. As described in the manuscript, we use two different data loss thresholds – initial (0.5; n=63) and adjusted (0.75; n=83)  – and report the results for both of them. Any subject that exceeded each respective dataloss threshold was tagged with the value "TRUE" in the logical variables `datalossover_thrs75` and `datalossover_thrs50`. In the `30_datamerge.R` code, these variables are then used to filter the dataset to only included subjects (default: adjusted data loss threshold, 0.75; n=83 included).

The Code section <span style="color: blue;">*[3] Light data transformation*</span> conducts a linear transformation of the alpha-opic irradiances into alpha-opic Equivalent Daylight Illuminances and creates variables with log10-transformed light data (due to violation of the linear regression assumptions, see more details in manuscript). However, *corrected values for all used light units* will be introduced in the `30_datamerge.R` code, as there was a mistake in the interpolation of spectral light data in the initial computation. Thus, if you you want to use any of the light data for calculations, please make sure to use the corrected data introduced in the datamerge code or subdatasets (e.g.`merged_data_conf.rda`, `conf_subdata.rda` ).  

The Code section <span style="color: blue;">*[4] Save checked rawfiles*</span> saves the quality checked data to the file `checked_rawfiles.rda`.

<span style="color: red;">**Note**</span>: We uploaded the resulting `checked_rawfiles.rda` file to allow continued analyses in case of problems with the prior R code.


## Data categorisation 
<span style="color: green;">

Folder: `02_rawdata` <br>
Input: `checked_rawfiles.rda` <br>
Output: `rawdata_ID_all.rda` <br>
RCode: `22_categorisation.r` <br>

</span>

Code section <span style="color: blue;">*[1] Load checked raw data*</span> loads the `checked_rawfiles.rda` file generated from the `21_qualitychecks.R` script.

In the Code section <span style="color: blue;">*[2] Experimental phase categorisation*</span> the observations are "tagged" according to the experimental phases they were collected in. This is done with the help of multiple steps. First, as rough categorisation step  all data <1.5 lx photopic illuminance is tagged as "Dark" data from the dark-adaptation phase. Then the categorisation is refined by tagging the "Lab" data from the laboratory light conditions. This procedure is verified visually, by plotting the first 181 photopic illuminance samples (log10-scale) of each participants' dataset as a function of the sample number. Finally the "Field" data samples are tagged, starting 3 samples after the laboratory data. The transition samples before starting the dark adaptation and in-between conditions are tagged as "NA". Subsequently, all light data from the dark adaptation phase and data below <1 lx photopic illuminance or mEDI are replaced by "NA" values because the used spectroradiometer does not allow valid measurements in these very dim light conditions. Additionally a new light variable giving the ratio between melanopic irradiance and photopic illuminance (`MPratio`) is introduced for later plotting. 

In code section <span style="color: blue;">*[3] Data saving*</span> we save the cleaned, quality checked and categorised dataset to the `rawdata_ID_all.rda` R data file.

## Data merge
<span style="color: green;">

Folder: `03_datamerge` <br>
Input: `cleaned_survey.rda`, `rawdata_ID_all.rda`, `merged_calc.rda` <br>
Output: `mergeddata_dem.rda`, `merged_data_conf.rda` <br>
RCode: `30_datamerge.R` <br>

</span>

First, the cleaned survey and raw data are combined into one dataset matched by id. In the Code section <span style="color: blue;"> *[1] Save demographic dataset*</span>  we save a subdataset called `mergeddata_dem.rda` including all participants for the demographic descriptions. 

In <span style="color: blue;"> *[2] Apply 75% data loss threshold*</span> we then apply the adjusted dataloss threshold (0.75; n=83 included) to select only included participants below 75% dataloss (`merged_data_incl75`).  This data is then used in the subsequent steps as `merged_data_incl`.

Then, in <span style="color: blue;"> *[3] Load corrected light data*</span>  we load the corrected light dataset `merged_calc.rda` which includes the light unit calculations from the correctly interpolated spectral irradiance data. 

Code section <span style="color: blue;"> *[4] Evaluate correction*</span> compares the corrected with the prior (uncorrected) light data across mEDI and illuminance and summarises the deviations and checks the outliers. The corrected data yields ~4% higher light levels compared to before. The values before correction are slightly lower across units because in the prior data, single defective wavelength pixels in the spectral irradiance measurements were set to 0 instead of correctly interpolated.

Code <span style="color: blue;"> *[5] Incorporating corrected light data*</span> then replaces all  light data in `merged_data_incl` with the corrected values from (`merged_calc`), while keeping the values set to NA during the quality checks (`21_qualitychecks.R`). Introducing the corrected values does *not* change the direction, general magnitude or interpretation of any of the results. There are however slight but visible changes in the numeric results (BFs and linear regression analyses), the data visualisation and the data summaries due to the corrected and thus higher (~4%) light level units. You can get get the results using the uncorrected values by just running the `30_datamerge.R` script, while leaving out this code section *[5] Incorporating corrected light data*.

The code <span style="color: blue;"> *[6] 50% data loss threshold*</span> applies the initial threshold (0.5; n=63 included) and creates dataset `merged_data_incl50` . To use this reduced dataset for subsequent calculations one needs to "uncomment" the code line (delete the "#") `#merged_data_incl <- merged_data_incl50`, and rerun the `03_datamerge.R` code.

In <span style="color: blue;"> *[7] Save dataset*</span>, we select the relevant variables for analysis and save them in a dataframe `merged_data_conf.rda`.


## Demographics
<span style="color: green;">

Folder: `04_demographics` <br>
Input: `mergeddata_all.rda` <br>
Output: `dem_tab.pdf`, `suppl_dem_tab.pdf`, `agepyr_plot.pdf` <br>
RCode: `40_demographics.R` <br>

</span>

In the first section <span style="color: blue;">*[1] Demographic data preparation*</span>, we use the data from `mergeddata_dem.rda` and prepare it for visualising the demographic characteristics of the sample by reducing the dataset to 1 row per participant (resulting in the subdataset `dem_data`) and labelling subjects that exceeded the 75% dataloss threshold (n=83 included, used as default) as "excluded". To compute the demographics according to the initial data loss threshold of 50% (n=63 included), one needs to "uncomment" the following two codelines (delete the "#"):
`#dem_data$excl[dem_data$overthrs50==TRUE] <- "EXCLUDED"`
`#dem_data$excl_time [dem_data$overthrs50==TRUE] <- "POST"`


In section <span style="color: blue;">*[2] Demographic table*</span> we then apply the "gtsummary" package to generate a demographic characteristics table (see Table 1 in manuscript) with the following steps:

- categorise the data into data types
- include only variables needed for the table 
- define decimal digits for continuous data
- relabel the variables 
- modify the header format.

In section <span style="color: blue;">*[3] Supplementary demographic table*</span> we repeat the procedure as in *[2] Demographic table*, using further participant characteristics surveyed in the study, generating an additional demographic characteristics table for the supplementary information (see Suppl. Table 2).

The code given in <span style="color: blue;">*[4] Demographic figure*</span> generates a pyramid plot stratified by sex for only the included participants (see Figure 2 in manuscript). If you have problems with the pdf saving code section, this may be related to the cairo_pdf device (especially for MACOS users). In this case, try deleting "device=cairo_pdf" in the ggsave command and rerun the code. 


## Subdatasets
<span style="color: green;">

Folder: `05_analysis` <br>
Input: `merged_data_conf.rda` <br>
Output: `conf_subdata.rda` <br>
RCode: `50_subdatasets.R` <br>

</span>

In the first code section <span style="color: blue;">*[1] Load merged data*</span> we import the `merged_data_conf.rda` file which consists of only included participant and relevant variables.

In <span style="color: blue;">*[2] General Subdatasets*</span>, we first filter the data so that it only contains valid pupil size observations and store it in the environment as `cf_data`. We then create the following subdatasets based on the categorised experimental phases (as categorized in the `22_categorisation.r` script) and check their number of observations.

- data from the field condition &rarr; `Fielddata`
- data from the dark adaptation (positive control) &rarr; `Darkdata`
- data from the laboratory condition (positive control) &rarr; `Labdata`
- uncategorised data in between conditions (not analysed) &rarr; `Transitiondata`

In the code section <span style="color: blue;">*[3] Subdatasets for visualising the age effect*</span> the subdatasets for the field condition are further divided into different light intensities in log-unit steps (plus the dark adaptation data) and summarised in the following coefficients (used in Figure 7 and 8 in the manuscript):

 - median pupil size
 - minimum pupil size
 - maximum pupil size
 - 25% quantile pupil size
 - 75% quantile pupil size
 - age of the participant


In the code section <span style="color: blue;">*[4] Subdataset for weather data*</span> we use the `merged_data_conf.rda` to generate a dataset from the field condition with light and weather data included. This is used in Figure 3 for describing the light data across different weather conditions and in Suppl. Figure 2 showing the density of light measures across the field conditions. Please note that the `weatherdata` subdataset contains more observations than the `Fielddata` subdataset as observations with invalid/missing pupil data but valid light data are included here.

<span style="color: blue;">*[5] Subdatasets for case data*</span>. In this section we generate a dataset with exemplary data from 2 participants of different age. The light data from the dark adaptation (which was priorly set to NA values in the `22_categorisation.r` script) are set to "0", so they can be included in the subplots (see Figure 6 in the manuscript and Suppl. Figure 7).

In the code section <span style="color: blue;">*[6] Subdatasets for autocorrelation*</span> we create a subdataset from the field condition (with all NAs still included) and compute autocorrelations (3 minute lag) used for Suppl. Figure 8. To compute the autocorrelations separately for every participant's trial, we used a loop that adds 19 "NA"" rows to where the observations transition from one participant id to the next.

In <span style="color: blue;">*[7] Saving subdatasets*</span> we save all subdatasets (image of the environment) into the `conf_subdata.rda` file.


## Hypotheses and exploratory analyses
<span style="color: green;">

Folder: `05_analysis` <br>
Input: `conf_subdata.rda` <br>
Output: Hypothesis tests' bayes factors (BF10) in form of `.txt` files (see `07_output/stat`)<br>
RCode: `51_hypotheses.R`<br>

</span>


<span style="color: red;">**Note**</span>: You can find the results of the hypothesis tests (but not the full exploratory analysis) as output from the `hypotheses.rmd` file in two versions for the respective samples: 
1) n=83,  75% data loss threshold: `hypotheses_n83.html`
2) n=63, 50% data loss threshold: `hypotheses_n63.html`

In the first section <span style="color: blue;">*[1] Loading subdatasets*</span> we load the `conf_subdata.rda` file, which includes an image of the environment with all subdatasets generated from the `50_subdatasets.R` code.

In section <span style="color: blue;">*[2] Testing log-transformation*</span> we run additional analyses testing whether the transformation improved the fit in the LMM, comparing the model with log10 transformation against a null model with linear light data, in the same procedure as our hypothesis tests specified in the pre-registered analysis. 

In Code scetion <span style="color: blue;">*[3] Positive control tests*</span> we test our confirmatory hypotheses in the controlled laboratory conditions as positive control tests.

First, we tested hypotheses CH1 with the linear models specified in stage 1 of our report with data from our laboratory conditions (subdataset "Labdata"). The models are tested with log10-transformed and linear (non-transformed) mEDI as a predictor.

Secondly, we tested hypothesis CH2 with the linear models specified in stage 1 of our report with data from our laboratory conditions (subdataset "Labdata"). Again, the test is once run for a model including the log10-transformed light data (mEDI and photopic illuminance) and once with non-transformed light data.

Thirdly, we test our age hypothesis  in the data from the 10-minute dark adaptation (subdataset "Darkdata") as a positive control, where we expected the age effect to be most pronounced. In the tested models, no light variables are used as a predictor because these samples were taken in darkness.

In the code given in <span style="color: blue;">*[4] Hypothesis testing - CH1*</span>, we test our confirmatory hypothesis CH1 in the data from the field condition (subdataset "Fielddata"). First, we conduct the test  with models including the log10-transformed mEDI predictor and then secondly with the linear mEDI predictor.

*CH1: In real-world conditions, light-adapted pupil size changes as a function of melanopsin sensitivity-weighted retinal illumination with higher illumination associated with a smaller pupil size.*

In the code given in <span style="color: blue;">*[5] Hypothesis testing - CH2*</span>, we test our confirmatory hypothesis CH2 in the data from the field condition (subdataset "Fielddata"). First, we conduct the test  with models including the log10-transformed mEDI and photopic illuminance predictors and then secondly with the linear mEDI and photopic illuminance predictors.
 
*CH2: In real-world conditions, melanopsin sensitivity-weighted retinal illumination better predicts pupil size than the weighted sum of L- and M-cone weighted retinal illumination (CIE 1931 Y, photopic illuminance).* 
 
 To double-check that our tests conducted with the "BayesFactors" package were valid, we additionally ran a test comparing the two models defined with a function from the lme4 package ("lmer") and compared the two models from CH2 regarding their Bayesian Information Criterion (BIC). The resulting BIC value for the full model is lower for the full model than with the null model, confirming the results from the BayesFactor analysis. 

We then approximated Bayes factors from the BICs using the equation by [Wagenmakers (2007)](https://doi.org/10.3758/BF03194105), retrieved from [Stevens (2019)](https://osf.io/eszbd), resulting in very similar Bayes Factor magnitudes.

In the code given in <span style="color: blue;">*[6] Hypothesis testing - CH3*</span>, we test our confirmatory hypothesis CH3 in the data from the field condition (subdataset "Fielddata"). First, we conduct the test  with models including the log10-transformed mEDI  predictor and then secondly with the linear mEDI predictor.
 
*CH3: In real-world conditions, light-adapted pupil size changes as a function of age, with higher age associated with a smaller pupil size.*

In the code section <span style="color: blue;">*[7] Exploratory analyses - EH*</span> we conduct our exploratory hypotheses tests specified in the the stage 2 manuscript. The analysis procedure corresponds to the confirmatory hypotheses. However, we only include log10-transformed mEDI as a predictor in these cases.

- *EH1: Sex differences in light-adapted pupil size are present under real-world conditions.*
- *EH2: Light-adapted pupil size varies as a function of iris colour under real-world conditions.*
- *EH3: Light-adapted pupil size varies as a function of habitual caffeine consumption (relative to body weight) under real-world conditions.*
- *EH4: Light-adapted pupil size varies as a function of the acute caffeine consumption (relative to body weight) under real-world conditions.*


In the code section <span style="color: blue;">*[8] Exploratory analyses without hypotheses *</span> Here, we conduct the exploratory analyses of the stage 2 manuscript that were not hypothesis driven. The LMM analysis procedure  uses the same approach as for the exploratory hypothesis tests. The output however is given as matrices of Bayes Factor tests.

The following exploratory tests were run:

1) Exploratory tests of single light level measures as pupil size predictors (6 variables: 5x Alpha-opic EDIs + photopic illumiance).
 
2) Exploratory tests of pairwise light level measures as pupil size predictors (all 15 pairwise combinations of the 6 light level variables).

Please note that these tests are included in the R code but not part of the rmd and html files with the hypothesis tests, due to the high demand on computing power and long runtime when "knitting".


## Figures and tables
<span style="color: green;">

Folder: `05_analysis` <br>
Input: `conf_subdata.rda` <br>
Output: Figures and tables used in the manuscript in form of `.pdf` files (see `07_output` & `07_output/suppl`)<br>
RCode: `52_figures&tables.R`, `ggplot_functions.R` <br>

</span>

In the *<span style="color: blue;">`ggplot_functions.R`*</span> code we specify functions in ggplot that we then use in the `52_figures&tables.R` for generating figures for the manuscript.

The `52_figures&tables.R` file is divided in the following code sections

In the first section <span style="color: blue;">*[1] Loading subdatasets*</span> we load the `conf_subdata.rda` file, which includes an image of the environment with all subdatasets generated from the `50_subdatasets.R` code.


In <span style="color: blue;">*[2] Source ggplot functions*</span> we load the prespecified ggplot functions from the ggplot_functions.R` file to the environment.

Section <span style="color: blue;">*[3] Weather & light conditions*</span> creates the 2 Subplots for Figure 3 of the manuscript, showing the light conditions across weather variations (`weath_panels.pdf`) as well as the 2 subplots of Suppl. Figure 2 (`SupplFig2.pdf`).

In section <span style="color: blue;">*[4] Case data age comparison*</span> we create Figure 6 of the manuscript, where we compare case data of two differently aged participants, plotting the pupillary light response (pupil size as a function of mEDI, see `agecomp_plot.pdf`). Additionally we generate Suppl. Figure 7 (`agecomp_plot_lux`), which includes the same comparison for photopic illuminance instead of mEDI.

In section <span style="color: blue;">*[5] Age effect across light conditions*</span> we prepare the linear regressions and subplots of Figure 7 and 8 of the manuscript, where we compare the age effect on pupil size across different light conditions (median pupil size and pupil size range as functions of age, see `age_panels1.pdf` and `age_panels2.pdf`).

In the <span style="color: blue;">*[6] Autocorrelation*</span> code section we plot the 2 panels of Suppl. Figure 4 where we depict the autocorrelations of pupil size and mEDI across a 3-minute time lag (see `autocor_panels.pdf`).

<span style="color: blue;">*[7] Data loss threshold*</span> creates Figure 4 (see `dataloss_plot.pdf`), where we depict the results of the "Proportion of excluded data" quality check for the two applied data loss threshold (0.5 and 0.75).

In <span style="color: blue;">*[8] Light condition comparison*</span> we create the 2 subplots of Figure 5 in the manuscript (see `lightcomp_panels.pdf`), where we compare the field and laboratory data regarding the correlation of photopic illuminance and mEDI.

In the code section <span style="color: blue;">*[9] Data tables*</span> we create the Supplementary Tables 3-9 summarizing each included participant's pupil and light (alpha-opic EDI & photopic illuminance) data (minimum, median, maximum and data loss ratio) separated between field & positive control data.

Finally we visualise the <span style="color: blue;">*[10] Linear regression assumptions*</span> (corresponding to Suppl. Figures 3-6) for the prediction of pupil size with the non-transformed and log10-transformed mEDI and photopic illuminance variables using the `performance` and `see` packages. The Figures were saved as pdf manually (size: landscape, width = 11.69 in, height = 8.27 in) because the *ggsave* function could not handle the size of those figures.


Note: Figure 1 of the manuscript and Suppl. Figure 1 are not based on raw data and are hence not generated in R. If you have problems with some of the pdf saving code, this may be related to the cairo_pdf device (especially for MACOS users). In this case, try deleting "device=cairo_pdf" in the *ggsave* commands and rerun the code.


## Output

In the `06_output` folder and `06_output/suppl` subfolder, you find the figures and tables generated in the `52_figures&tables.R` and `40_demographics.R` scripts in `.pdf` format. In the subfolder `06_output/stat` you find the results of the `51_hypotheses.R` analyses in separate `.txt` files.
