# Overview
This Code and data repository is part of the registered report RSOS-191613: Regulation of pupil size in natural vision across the human lifespan (the stage 1 in principal accepted (IPA) manuscript RSOS-191613.R1 is found here: https://osf.io/zrksf/).

If you have any comments or queries, please reach out to us at rafael.lazar@unibas.ch and manuel.spitschan@tum.de.

<span style="color: red;">
Shortcut to hypothesis testing: </span> 
You can find the results of the hypothesis tests as output from the  `hypotheses.rmd` file in two versions for the two respective samples: 
1) n=83,  75% data loss threshold: `hypotheses_n83.html`
2) n=63, 50% data loss threshold: `hypotheses_n63.html`

# Dependency management
This R project applies [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package management. Use `renv::restore()` to download the correct package versions and ensure computational reproducibility. 

# Workflow description 
Here we explain the R script processing and any manual steps in sequential order. For running the R code files in RStudio, make sure to use an R-Project file (e.g. the uploaded `RSOS-191613.Rproj`) to avoid issues with the working directory.


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

The Code section <span style="color: blue;">*[2] Data cleaning & quality checks*</span> applies the data quality checks. This includes "Lack of good-quality fit", "Pupil size screening", and "Saturated spectroradiometer samples", where invalid pupil and light data are replaced by "NA" values. And finally the "Proportion of excluded data" quality check, which compares the proportion of excluded data points per participant vs. the specified data loss threshold. As described in the manuscript, we use two different data loss thresholds – initial (0.5; n=63) and adjusted (0.75; n=83)  – and report the results for both of them. In the script we use the adjusted threshold (0.75; n=83 included) as the default. To apply the initial threshold (0.5; n=63 included), one needs to "uncomment" the code line `#data_loss_thres <- 0.5`, and rerun the `21_qualitychecks.R` code.

The Code section <span style="color: blue;">*[3] Light data transformation*</span> conducts a linear transformation of melanopic irradiance into Melanopic Equivalent Daylight Illuminance (mEDI) and creates variables with log10-transformed light data (due to violation of the linear regression assumptions, see more details in manuscript). 

The Code section <span style="color: blue;">*[4] Save checked rawfiles*</span> saves the quality checked data to the file `checked_rawfiles.rda`.

<span style="color: red;">**Note**</span>: Remember that for the "Proportion of excluded data" quality check we used the adjusted data loss threshold (0.75; n=83 included) as a default. We also uploaded the resulting `checked_rawfiles.rda` file to allow continued analyses in case of problems with the prior R code. If you want to continue the analysis with the reduced dataset retained from the initial data loss threshold  (0.5; n=63 included),  "uncomment" the code line `#data_loss_thres <- 0.5`, restart RStudio and rerun the `21_qualitychecks.R` code. The `checked_rawfiles.rda` will then contain the same data but with  only n=63 tagged as "Included". 


## Data categorisation 
<span style="color: green;">
Folder: `02_rawdata` <br>
Input: `checked_rawfiles.rda` <br>
Output: `rawdata_ID_all.rda` <br>
RCode: `22_categorisation.r` <br>
</span>

Code section <span style="color: blue;">*[1] Load checked raw data*</span> loads the `checked_rawfiles.rda` file generated from the `21_qualitychecks.R` script.

In the Code section <span style="color: blue;">*[2] Experimental phase categorisation*</span> the observations are "tagged" according to the experimental phases they were collected in. This is done with the help of multiple steps. First, as rough categorisation step  all data <1.5 lx photopic illuminance is tagged as "Dark" data from the dark-adaptation phase. Then the categorisation is refined by tagging the "Lab" data from the laboratory light conditions. This procedure is verified visually, by plotting the first 181 photopic illuminance samples (log10-scale) of each participants' dataset as a function of the sample number. Finally the "Field" data samples are tagged, starting 3 samples after the laboratory data. The transition samples before starting the dark adaptation and in-between conditions are tagged as "NA". Subsequently, all light data from the dark adaptation phase and data below <1 lx photopic illuminance or mEDI are replaced by "NA" values because the used spectroradiometer does not allow valid measurements in these very dim light conditions. Additionally a new light variable giving the ratio between melanopic irradiance and photopic illuminance (`MPratio`) is introduced for later plotting. 

In code section <span style="color: blue;">*[3] Data saving*</span> we save the cleaned, quality checked and categorised dataset to the `rawdata_ID_all.rda` R data file. The uploaded `rawdata_ID_all.rda`file in the repository uses the adjusted data loss threshold (0.75; n=83 included).

## Data merge
<span style="color: green;">
Folder: `03_datamerge` <br>
Input: `cleaned_survey.rda`, `rawdata_ID_all.rda` <br>
Output: `mergeddata_all.rda`, `merged_data_conf.rda` <br>
RCode: `30_datamerge.R` <br>
</span>

In the datamerge code, the cleaned survey and raw data are combined into one dataset matched by id. The full dataset is saved as `mergeddata_all.rda` including all observations and columns. The full dataset is then reduced to only included participants and relevant variables for confirmatory and exploratory analysis and saved as `merged_data_conf.rda`. 

<span style="color: red;">**Note**</span>: Beware that in the `merged_data_conf.rda` dataset, participants excluded during the "Proportion of excluded data" quality check are also not included in the data anymore. As a default the adjusted data loss threshold (0.75; n=83 included) was used (see uploaded`merged_data_conf.rda` file). If you want to continue the analysis with the reduced dataset retained from the initial data loss threshold  (0.5; n=63 included), you need to go back to `21_qualitychecks.R` code, "uncomment" the code line `#data_loss_thres <- 0.5`, restart RStudio and repeat the workflow from the *Quality checks* section on.


## Demographics
<span style="color: green;">
Folder: `04_demographics` <br>
Input: `mergeddata_all.rda` <br>
Output: `dem_tab.pdf`, `suppl_dem_tab.pdf`, `agepyr_plot.pdf` <br>
RCode: `40_demographics.R` <br>
</span>

In the first section <span style="color: blue;">*[1] Demographic data preparation*</span>, we use the data from `mergeddata_all.rda` and prepare it for visualising the demographic characteristics of the sample, by selecting only relevant columns and reducing the dataset to 1 row per participant (resulting in the subdataset `dem_data`).


In section <span style="color: blue;">*[2] Demographic table*</span> we then apply the "gtsummary" package to generate a demographic characteristics table (see Table 1 in manuscript) with the following steps:

- categorise the data into data types
- include only variables needed for the table 
- define decimal digits for continuous data
- relabel the variables 
- modify the header format.

In section <span style="color: blue;">*[3] Supplementary demographic table*</span> we repeat the procedure as in *[2] Demographic table*, using further participant characteristics surveyed in the study, generating an additional demographic characteristics table for the supplementary information (see Suppl. Table 2).

The code given in <span style="color: blue;">*[4] Demographic figure*</span> generates a pyramid plot stratified by sex for only the included participants (see Figure 2 in manuscript)
The size of the sample depends on the chosen data loss threshold in the `21_qualitychecks.R` script:

* adjusted data loss threshold (0.75; n=83 included) [used as default]
* initial data loss threshold  (0.5; n=63 included)

Thus, the figure is either based on n=83 or n=63 participants. To change the threshold to 0.5, navigate back to the `21_qualitychecks.R` file in the code workflow and "uncomment" the code line `#data_loss_thres <- 0.5`, restart RStudio and rerun whole workflow starting with the `21_qualitychecks.R` code.


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

In the code section <span style="color: blue;">*[3] Subdatasets for visualising the age effect*</span> the subdatasets for the field condition are further divided into different light intensities in log-unit steps (plus the dark adaptation data) and summarised in the following coefficients (used in Figure 3 and 4 in the manuscript):

 - median pupil size
 - minimum pupil size
 - maximum pupil size
 - 25% quantile pupil size
 - 75% quantile pupil size
 - age of the participant


In the code section <span style="color: blue;">*[4] Subdataset for weather data*</span> we use the `merged_data_conf.rda` to generate a dataset from the field condition with light and weather data included. This is used in Figure 3 for describing the light data across different weather conditions. Please note that the `weatherdata` subdataset contains more observations than the `Fielddata` subdataset as observations with invalid/missing pupil data but valid light data are included here.

<span style="color: blue;">*[5] Subdatasets for case data*</span>. In this section we generate a dataset with exemplary data from 2 participants of different age. The light data from the dark adaption (which was priorly set to NA values in the `22_categorisation.r` script) are set to "0", so can be included in the subplots (see Figure 6 in the manuscript).

In the code section <span style="color: blue;">*[6] Subdatasets for autocorrelation*</span> we create a subdataset from the field condition (with all NAs still included) and compute autocorrelations (3 minute lag) used for Suppl. Figure 4. To compute the autocorrelations separately for every participant's trial, we used a loop that adds 19 "NA"" rows to where the observations transition from one participant id to the next.

In the <span style="color: blue;">*[7] Other subdatasets*</span> we created a subdataset for an exploratory analysis of subjective sleepiness values before and after the outdoor part of the field condition.

In <span style="color: blue;">*[8] Saving subdatasets*</span> we save all subdatasets (image of the environment) into the `conf_subdata.rda` file.


## Hypotheses
<span style="color: green;">
Folder: `05_analysis` <br>
Input: `conf_subdata.rda` <br>
Output: Hypothesis tests' bayes factors (BF10) in form of `.txt` files (see `07_output/stat`)<br>
RCode: `51_hypotheses.R`<br>
</span>


<span style="color: red;">**Note**</span>: You can find the results of the hypothesis tests as output from the  `hypotheses.rmd` file in two versions for the respective samples: 
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
 
 To double-check that our tests conducted with the "BayesFactors" package were valid, we additionally ran a test comparing the two models defined with a function from the lme4 package ("lmer") and compared the two models from CH2 regarding their Akaike's Information Criterion (AIC). The resulting AIC value for the full model is lower for the full model than with the null model, confirming the results from the BayesFactor analysis.
 However, the AIC test with using models with the non-transformed light data result in the following warning: *"fit warnings: Some predictor variables are on very different scales: consider rescaling"*
This further confirms the suggestion to use log10-transformed light data  instead of linear light data as predictors in linear regression.

In the code given in <span style="color: blue;">*[6] Hypothesis testing - CH3*</span>, we test our confirmatory hypothesis CH3 in the data from the field condition (subdataset "Fielddata"). First, we conduct the test  with models including the log10-transformed mEDI  predictor and then secondly with the linear mEDI predictor.
 
*CH3: In real-world conditions, light-adapted pupil size changes as a function of age, with higher age associated with a smaller pupil size.*

In the code section <span style="color: blue;">*[7] Exploratory analyses - EH*</span> we conduct our exploratory analyses specified in the the stage 2 manuscript. The analysis procedure corresponds to the confirmatory hypotheses. However, we only include log10-transformed mEDI as a predictor in these cases.

- *EH1: Sex differences in light-adapted pupil size are present under real-world conditions.*
- *EH2: Light-adapted pupil size varies as a function of iris colour under real-world conditions.*
- *EH3: Light-adapted pupil size varies as a function of habitual caffeine consumption (relative to body weight) under real-world conditions.*
- *EH4: Light-adapted pupil size varies as a function of the acute caffeine consumption (relative to body weight) under real-world conditions.*

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

Section <span style="color: blue;">*[3] Weather & light conditions*</span> creates the 2 Subplots for Figure 3 of the manuscript, showing the light conditions across weather variations (`weath_panels.pdf`)

In section <span style="color: blue;">*[4] Case data age comparison*</span> we create Figure 6 of the manuscript, where we compare case data of two differently aged participants, plotting the pupillary light response (pupil size as a function of mEDI, see `agecomp_plot.pdf`).

In section <span style="color: blue;">*[5] Age effect across light conditions*</span> we prepare the linear regressions and subplots of Figure 7 and 8 of the manuscript, where we compare the age effect on pupil size across different light conditions (median pupil size and pupil size range as functions of age, see `age_panels1.pdf` and `age_panels2.pdf`).

In the <span style="color: blue;">*[6] Autocorrelation*</span> code section we plot the 2 panels of Suppl. Figure 4 where we depict the autocorrelations of pupil size and mEDI across a 3-minute time lag (see `autocor_panels.pdf`).

<span style="color: blue;">*[7] Data loss threshold*</span> creates Figure 4 (see `dataloss_plot.pdf`), where we depict the results of the "Proportion of excluded data" quality check for the two applied data loss threshold (0.5 and 0.75).

In <span style="color: blue;">*[8] Light condition comparison*</span> we create the 2 subplots of Figure 5 in the manuscript (see `lightcomp_panels.pdf`), where we compare the field and laboratory data regarding the correlation of photopic illuminance and mEDI.

In the code section <span style="color: blue;">*[9] Data tables*</span> we create the Supplementary Tables 3 & 4 summarizing each included participant's pupil and mEDI data (minimum, median, maximum and data loss ratio)  separated between field & positive control data.

Finally we visualise the <span style="color: blue;">*[10] Linear regression assumptions*</span> (corresponding to Suppl. Figures 2 & 3) for the prediction of pupil size with the non-transformed and log10-transformed mEDI variable using the `performance` and `see` packages. The Figures were saved as pdf manually (see `assumptest_lin.pdf`, `assumptest_log.pdf`; size: landscape, width = 11.69 in, height = 8.27 in) because the *ggsave* function could not handle the size of those figures. 


Note: *Figure 1 and Suppl. Figure 1 of the manuscript are not based on raw data and are hence not generated in R*


## Output

In the `06_output` folder and `06_output/suppl` subfolder, you find the figures and tables generated in the `52_figures&tables.R` and `40_demographics.R` scripts in `.pdf` format. In the subfolder `06_output/stat` you find the results of the `51_hypotheses.R` analyses in separate `.txt` files.
