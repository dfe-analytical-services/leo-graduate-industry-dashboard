<h1 align="center">
  <br>
Longitudinal Education Outcomes - Graduate Industry Dashboard
  <br>
</h1>

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#how-to-contribute">How to contribute</a> |
  <a href="#contact">Contact</a>
</p>

---


## Introduction 

This dashboard provides users an opportunity to investigate the Longitudinal Education Outcomes (LEO) new graduate industry data. 

Live version of the dashboard can be accessed at:

- [https://department-for-education.shinyapps.io/leo-graduate-industry-dashboard/](https://department-for-education.shinyapps.io/leo-graduate-industry-dashboard/)

The dashboard is split into four themes:

- <b>Interactive Sankey charts</b> that shows the longitudinal journey of the 5 year after graduation (YAG) cohort. This shows the number of graduates working in each industry for the selected subject area at one, three and five years after graduation. 
- <b>Regional analysis</b> that compares the number of graduates who studied in and are currently living in each region. 

<em><b>Tables</b> that show proportions and median earnings for combinations of industry, subject, qualification level, sex, prior attainment, current region, ethnicity and FSM status. </em>
- <b>Subject by industry tables</b>, which show for the selected subject, which industries those graduates work in at the selected year after graduation. These tables are also expandable to the 3-digit SIC code level providing more granular breakdowns for each of the industry sections.   
- <b>Industry by subject tables</b>, which show for the selected industry, what subjects the graduates working in that industry studied.  

---

## Requirements

### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 4.1.3 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)
  
---

## How to use

### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies. If it gets stuck on the BH package, manually download the zip from CRAN and unzip into your library folder.

4. Run `shiny::runApp()` to run the app locally.


### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest that test the app loads, that content appears correctly when different inputs are selected, and that tab content displays as expected. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The function run_tests_locally() is created in the Rprofile script and is available in the RStudio console at all times to run both the unit and ui tests.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions, to [https://department-for-education.shinyapps.io/leo-graduate-industry-dashboard/](https://department-for-education.shinyapps.io/leo-graduate-industry-dashboard/). The yaml file for this can be found in the .github/workflows folder.

If you have any questions about the shinyapps.io subscription and deployment in DfE please contact the Statistics Development Team at [statistics.development@education.gov.uk](mailto:statistics.development@education.gov.uk).

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function tidy_code() is created in the Rprofile script and therefore is always available in the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.


---

## How to contribute

Our contributing guidelines can be found at [https://github.com/dfe-analytical-services/leo-graduate-industry-dashboard/blob/main/CONTRIBUTING.md](https://github.com/dfe-analytical-services/leo-graduate-industry-dashboard/blob/main/CONTRIBUTING.md).

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug.

### Merging pull requests

Only members of the development team can merge pull requests. Add chfoster, cjrace and rmbielby as requested reviewers, and the team will review before merging.

---

## Contact

If you have any questions about the dashboard please contact [HE.LEO@education.gov.uk](mailto:HE.LEO@education.gov.uk).
