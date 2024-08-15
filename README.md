# comparative-booster-spring-2023

[View on OpenSAFELY](https://jobs.opensafely.org/repo/https%253A%252F%252Fgithub.com%252Fopensafely%252Fcomparative-booster-spring2023)

Details of the purpose and any published outputs from this project can be found at the link above.

The contents of this repository MUST NOT be considered an accurate or valid representation of the study or its purpose. 
This repository may reflect an incomplete or incorrect analysis with no further ongoing work.
The content has ONLY been made public to support the OpenSAFELY [open science and transparency principles](https://www.opensafely.org/about/#contributing-to-best-practice-around-open-science) and to support the sharing of re-usable code for other subsequent users.
No clinical, policy or safety conclusions must be drawn from the contents of this repository.

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 



# Study details

*Comparative effectiveness of Pfizer versus Sanofi COVID-19 accines during the spring 2023 booster campaign* This study estimates the comparative effectiveness of the two major vaccines used for boosting in England in Spring 2023; Pfizer and Sanofi.

Recipients of each brand are matched on date of receipt, region, COVID-19 vaccine history, and other key characteristics likely to influence the type of booster dose received. 

## Repository navigation

-   If you are interested in how we defined our codelists, look in the [`codelists/`](./codelists/) directory.

-   Analysis scripts are in the [`analysis/`](./analysis) directory.

    -   The instructions used to extract data from the OpensAFELY-TPP database is specified in the [dataset definition](./analysis/dataset_definition.py); this is written in ehrQL.
    -   The [`analysis/`](./analysis) sub-directories `design`, `functions`, `dummydata` contains preliminary design-related scripts, useful functions, and dummy data.
    -   The remaining folders mostly contain the R scripts that process, describe, and analyse the extracted database data.

-   Non-disclosive model outputs, including tables, figures, etc, are in the [`released_outputs/`](./released_outputs) directory.

-   The [`project.yaml`](./project.yaml) defines run-order and dependencies for all the analysis scripts. **This file should *not* be edited directly**. To make changes to the yaml, edit and run the [`create-project.R`](./create-project.R) script instead.

## R scripts

#### Pre-server scripts
These scripts can be run locally, as they do not depend on any data extracted from the database. 
-   [`study-dates.R`](./analysis/design/study-dates.R) defines the key study dates (start date, end date, vaccine roll-out dates, etc) that are used throughout the study. It create a json file that is used by R scripts and the study definition.
-   [`design.R`](./analysis/design/design.R) defines some common design elements used throughout the study such as outcomes and matching variables.
-   [`dummydata.R`](./analysis/dummydata/dummydata.R) contains the script used to generate dummy data. This is used instead of the usual dummy data specified in the dataset definition, because it is then possible to impose some more useful structure in the data, like more realistic vaccination schedules and events that occur in the expected order. If the study definition is updated, this script must also be updated to ensure variable names and types match.

#### On-server scripts
These scripts use data extracted from the OpenSAFELY-TPP server, after [`analysis/dataset_definition.py`](analysis/dataset_definition.py) has run.
-   [`data_process.R`](./analysis/data_process.R) imports the extracted database data (or dummy data), standardises some variables and derives some new ones.
-   [`data_selection.R`](./analysis/data_selection.R) filters out participants who should not eligible for matching, and creates a small table used for the inclusion/exclusion flowchart.
-   [`match.R`](./analysis/match.R) runs the matching algorithm to pair Pfizer with Sanofi booster recipients. It outputs a dataset given the matching status (TRUE/FALSE) of each candidate, and a matching ID.  and other matching diagnostics. The script takes two argument:
    - `cohort`, the name of the cohort, currently "cv" or "age75plus" .
    - `matchset`, the set of matching variables used to match, as defined in the [`design.R`](analysis/design/design.R) script.
-   [`match_report.R`](./analysis/match_report.R) describes baseline information for the matched cohort, eg Table 1 type cohort characteristics, and an inclusion criteria flowchart. The script also uses the `cohort` and `matchset` arguments to pick up the matching data from the previous script.
-   [`km.R`](./analysis/km.R) derives Kaplan-Meier survival estimates for the treatment groups and calculates relative risk and risk differences. The script takes four arguments:
    - `cohort`, as before.
    - `matchset`, as before.
    - `subgroup` to choose which subgroup to run the analysis within. Choose _all_ for no subgroups (i.e., the main analysis). Choose _<variable>_ to select a specific variable to stratify on -- this variable must be exactly matched in the matching run. 
    - `outcome` to choose the outcome of interest, for example _covidadmitted_ or _coviddeath_.
-   [`combine`](./analysis/combine.R) collects treatment contrasts from the [`km.R`](./analysis/km.R) script and other preliminary outputs (flowcharts, table1, matching coverage, etc) and combines into a single dataset for each output type.
-   [`release_objects`](./analysis/release_objects.R) collects files that are intended for release and puts them in a single directory to make releasing files easier. 


## Protocol

This study is an adaption of a prior study comparing Pfizer and Moderna during the Autumn/Winter 2021 booster campaign, and does not have a distinct protocol.

## Workspace

https://jobs.opensafely.org/datalab/covid-19-vaccine-effectiveness/comparative-booster-spring2023_main/

## Pre-print

https://doi.org/10.1101/2024.03.15.24304277

