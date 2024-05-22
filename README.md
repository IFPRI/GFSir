
# GFSir

<!-- badges: start -->

[![license](https://img.shields.io/badge/Licence-GPL%20(%3E%3D%203)-red)](https://github.com/IFPRI/GFSir/blob/master/LICENSE.md)

[![:name status
badge](https://ifpri.r-universe.dev/badges/:name)](https://ifpri.r-universe.dev)

[![R-CMD-check](https://github.com/IFPRI/GFSir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/IFPRI/GFSir/actions/workflows/R-CMD-check.yaml)

[![:registry status
badge](https://ifpri.r-universe.dev/badges/:registry)](https://ifpri.r-universe.dev)

[![GFSir status
badge](https://ifpri.r-universe.dev/badges/GFSir)](https://ifpri.r-universe.dev)
<!-- badges: end -->

Global Food Security analysis using IMPACT model and R (GFSir) is an R
package which helps in compiling results from the standard outputs of
the International Model for Policy Analysis of Agricultural Commodities
and Trade (IMPACT) model.

The IMPACT model was developed in the early 1990s to explore the long
term challenges facing policymakers in reducing hunger and poverty in a
sustainable fashion. The IMPACT model has been expanded and improved
repeatedly to respond to increasingly complex policy questions and the
state-of-the-art of modeling. [See documentation of most recent
update](http://www.ifpri.org/publication/international-model-policy-analysis-agricultural-commodities-and-trade-impact-model-0).

The goal of GFSir is to to be able to setup some standard functions
which will help in reading the outputs from a *gdx* file belonging to
the outputs of an IMPACT run.

## Outputs

GFSir can generate IMPACT outputs for the following group of indicators

- GDP and population for SSP scenarios
- Area
- Production from IMPACT activities
- Crop yields
- Commodity prices
- Trade
- Commodity demand
- Per capita demand for commodities
- Per capita kcal availability

## Installation

You can install the development version of GFSir from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("IFPRI/GFSir")
```

## Additional repository

For installation of the most recent package version of IFPRI managed R
packages, `magclass` (for array based operations), additional
repositories have to be added in R:

``` r
options(repos = c(CRAN  = "https://cloud.r-project.org/",
                  PIK   = "https://rse.pik-potsdam.de/r/packages", 
                  KIRAN = "https://ifpri.r-universe.dev"))
```

Here,

- CRAN is the Comprehensive R Archive Network

- PIK is the repository managed by PIK in Germany

- KIRAN is the Key IMPACT R Archive Network managed by IFPRI

See Imports section in the `DESCRIPTION` file of this package for a list
of additional packages needed for GFSir to be used correctly.

## Questions / Problems

In case of questions / problems please contact Abhijeet Mishra
(<A.Mishra@cgiar.org>)
