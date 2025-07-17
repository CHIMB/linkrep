
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

`linkrep` provides an easy-to-use and extensible architecture for
generating linkage quality reports in R. It simplifies the process of
evaluating and reporting on the quality of data linkage.

This package aims to provide a comprehensive tool for data linkage
analysts to assess the quality of their linkage processes, while also
helping data providers and researchers understand linkage errors and
evaluate potential biases.

Reports generated with `linkrep` can be further customized to fit your
specific needs:

- **Add new elements**: Include additional tables, figures, sections or
  data as required.

- **Modify content**: Customize written portions, including the Methods
  section, to better reflect your specific linkage processes.

- **Personalize appearance**: Adjust the report’s background, layout,
  styles, and references to tailor the report to your needs.

# Installation

## R Studio Installation

To install `linkrep` from GitHub, begin by installing and loading the
`devtools` package:

``` r
# install.packages("devtools")
library(devtools)
```

Afterwards, you may install the automated data linkage package using
`install_github()`:

``` r
devtools::install_github("CHIMB/linkrep")

# You may need to install tinytex:
# install.packages("tinytex")
# tinytex::install_tinytex()
```

## Local Installation

To install `linkrep` locally from GitHub, select the most recent release
from the right-hand tab on the GitHub repository page. Download the
<b>Source code (zip)</b> file, then move over to RStudio. You may then
run the code:

``` r
path_to_pkg <- file.choose() # Select the unmodified package you downloaded from GitHub.
devtools::install_local(path_to_pkg)

# You may need to install tinytex:
# install.packages("tinytex")
# tinytex::install_tinytex()
```

# Main Report Elements

**Summary**: Overview of the Methods section and results

**How to Read This Report**: Provides recommendations on how to
interpret the tables and figures to assess for potential biases.

**Linkage Rate Summary**: Includes the linkage rate table which
stratifies linkage rates by sociodemographic factors and other
characteristics.

**Linkage Algorithm Summary**: If provided, includes tables and figures
describing the linkage algorithm and its quality.

**Performance Metrics**: If provided, includes multiple performance
metrics (0-100) listed in table, visualized using a radar chart.

**Background**: Describes record linkage, how it’s performed, and its
limitations.

**Methods**: Details the linkage process, including pre-processing and
techniques used.

**Appendix**: Algorithms that were considered for testing, along with
their performance metrics, can be included at the end of the report.

# Additional Information & Documentation

For detailed instructions on formatting data for the report and
customizing features, refer to the [User
Documentation](https://github.com/CHIMB/linkrep/blob/main/inst/docs/User_Documentation.pdf)

For examples of reports that can be generated using the `linkrep`
package, download and view the sample [Final
Report](https://github.com/CHIMB/linkrep/raw/main/inst/docs/Sample%20Final%20Report.pdf)
and [Sensitivity Analysis
Report](https://github.com/CHIMB/linkrep/raw/main/inst/docs/Sample%20Sensitivity%20Analysis%20Report.pdf)
which uses fake/synthetic data to better help showcase the elements that
make up each report. The reports generated here use two synthetic
datasets, both of which can be downloaded, with the [Left
Dataset](https://github.com/CHIMB/autolink/blob/main/data/syndataA_final.csv)
containing 150 records that have a corresponding link to one of the 250
records in the [Right
Dataset](https://github.com/CHIMB/autolink/blob/main/data/syndataB_final.csv).

# Authors

- [Elizabeth Stoughton](https://github.com/stoughty111)
- [Cole Chuchmach](https://github.com/Cole-Chuchmach)
- [Barret A. Monchka](https://github.com/barretmonchka)
