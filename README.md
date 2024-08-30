
<!-- README.md is generated from README.Rmd. Please edit that file -->

# linkrep

`linkrep` provides an easy-to-use and extensible architecture for
generating linkage quality reports in R. It simplifies the process of
evaluating and reporting on the quality of data linkage.

This package aims to provide a comprehensive tool for data linkers to
assess the quality of their linkage processes, while also helping data
providers and researchers understand linkage errors and evaluate
potential biases.

Reports generated with `linkrep` can be customized to fit your specific
needs:

- **Add new elements**: Include additional tables, figures, sections or
  data as required

- **Modify content**: Customize written portions, including the Methods
  section, to better reflect your specific linkage processes

- **Personalize appearance**: Adjust the report’s background, layout and
  styles to tailor the report to your needs

## Main Report Elements

**Summary**: Overview of the Methods section and results

**How to Read This Report**: Provides recommendations on how to
interpret the tables and figures to assess for potential biases

**Linkage Rate Summary**: Includes the linkage rate table which
stratifies linkage rates by sociodemographic factors and other
characteristics

**Linkage Algorithm Summary**: If provided, includes tables and figures
describing the linkage algorithm and its quality

**Background**: Describes record linkage, how it’s performed, and its
limitations

**Methods**: Details the linkage process, including pre-processing and
techniques used

## Example Report

\[insert report generation, including the sample data\]

## Installation

``` r
# install.packages("pak")
pak::pak("CHIMB/linkrep")

# You may need to install tinytex:
# install.packages("tinytex")
# tinytex::install_tinytex()
```

### Local Installation

Navigate to **Releases** on the right-hand side of the GitHub page and
select the most recent release. Download the **Source code (zip)** file,
then run the following code to complete installation:

``` r
# Select the unmodified package you downloaded from GitHub:
pkg_path <- file.choose()
devtools::install_local(pkg_path)

# You may need to install tinytex:
# install.packages("tinytex")
# tinytex::install_tinytex()
```

## Additional Information

\[link documentation\]

## Authors

- [Elizabeth Stoughton](https://github.com/stoughty111)
- [Barret A. Monchka](https://github.com/barretmonchka)
