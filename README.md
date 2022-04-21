# Comparative effectiveness of BNT162b2 versus Moderna COVID-19 booster doses

This study estimates the comparative effectiveness of the two major booster doses administered in England; BNT162b2 (Pfizer-BioNTech) and Moderna mRNA-1273.

Recipients of each brand are matched on date of receipt, region, first and second dose type, and other key characteristics likely to influence the type of booster dose received. 

## Repository navigation

-   If you are interested in how we defined our codelists, look in the [`codelists/`](./codelists/) directory.

-   Analysis scripts are in the [`analysis/`](./analysis) directory.

    -   The instructions used to extract data from the OpensAFELY-TPP database is specified in the [study definition](./analysis/study_definition.py); this is written in Python, but non-programmers should be able to understand what is going on there
    -   The [`lib/`](./lib) directory contains preliminary (pre data extract) scripts, useful functions, and dummy data.
    -   The remaining folders mostly contain the R scripts that process, describe, and analyse the extracted database data.

-   Non-disclosive model outputs, including tables, figures, etc, are in the [`released_outputs/`](./released_outputs) directory.

-   The [`project.yaml`](./project.yaml) defines run-order and dependencies for all the analysis scripts. **This file should *not* be edited directly**. To make changes to the yaml, edit and run the [`create-project.R`](./create-project.R) script instead.

-   You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/comparative-booster)


## R scripts

-   [`design.R`](analysis/R/design.R) defines some common design elements used throughout the study, such as follow-up dates, model outcomes, and matching variables.
-   [`dummydata.R`](analysis/R/dummydata.R) contains the script used to generate dummy data. This is used instead of the usual dummy data specified in the study definition, because it is then possible to impose some more useful structure in the data, such as ensuring nobody has a first dose of both the Pfizer and Astra-Zeneca vaccines. If the study definition is updated, this script must also be updated to ensure variable names and types match.
-   [`data_process.R`](analysis/R/data_process.R) imports the extracted database data (or dummy data), standardises some variables and derives some new ones.
-   [`data_selection.R`](./analysis/data_selection.R) filters out participants who should not eligible for matching, and creates a small table used for the inclusion/exclusion flowchart.
-   [`match.R`](./analysis/match.R) runs the matching algorithm to pair BNT162b2 with Moderna booster recipients. It outputs a dataset given the matching status (TRUE/FALSE) of each candidate, and a matching ID.  and other matching diagnostics. The script takes one argument:
    -   `matchset`, the set of matching variables (exact or caliper) used to match, as defined in the [`design.R`](analysis/R/design.R) script.
-   [`match_report.R`](./analysis/match_report.R) describes baseline information for the matched cohort, eg Table 1 type cohort characteristics, and a inclusions criteria flowchart. The script also uses the `matchset` argument to pick up the matching data from the previous script.
-   [`km.R`](./analysis/km.R) derives Kaplan-Meier survival estimates for the treatment groups and calculates relative risk and risk differences. The script takes three arguments:
    -  `matchset`, as before.
    -   `subgroup` to choose which subgroup to run the analysis within. Choose _all_ for no subgroups (i.e., the main analysis). Choose _<variable>_ to select a specific variable to stratify on -- this variable must be exactly matched in the matching run. 
    -   `outcome` to choose the outcome of interest, for example _postest_ or _covidadmitted_.
-   [`km_combine`](./analysis/km_combine.R) collects data from the [`km.R`](./analysis/km.R) script and combines into one dataset.
-   [`release_objects`](./analysis/release_objects.R) collects files that are intended for release and puts them in a single directory to make releasing files easier. 



## Licences
As standard, research projects have a MIT license. 

## Manuscript

Not yet drafted.

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).
