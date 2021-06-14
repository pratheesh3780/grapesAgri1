# `grapesAgri1`: Collection of shiny applications for data analysis in Agriculture-Part 1 <img src="man/figures/logo.PNG" align="right" alt="logo" width="173" height = "200" style = "border: none; float: right;">
### General R-shiny based Analysis Platform Empowered by Statistics in Agriculture part-1 (grapesAgri1)
###### Version : 0.1.0; Copyright (C) 2020-2021: [Pratheesh P. Gopinath](https://www.kaugrapes.com); License: [GPL-3](https://www.r-project.org/Licenses/) 
##### *Gopinath, P. P.<sup>1</sup>, Parsad, R.<sup>2</sup>, Joseph, B.<sup>1</sup>, Adarsh, V.S.<sup>3</sup>*

1.  Department of Agricultural Statistics, College of Agriculture, Vellayani, Kerala Agricultural Univesity.
2.  ICAR-Indian Agricultural Statistics Research Institute,
    New Delhi.
3.  Department of Agricultural Statistics, BCKV, West Bengal

---

![CRAN/METACRAN](https://img.shields.io/cran/v/grapesAgri1?style=for-the-badge)
![GitHub](https://img.shields.io/github/license/pratheesh3780/grapesAgri1)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/grapesAgri1)](https://cran.r-project.org/package=grapesAgri1)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4923220.svg)](https://doi.org/10.5281/zenodo.4923220)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/pratheesh3780/grapesAgri1)
![GitHub R package version](https://img.shields.io/github/r-package/v/pratheesh3780/grapesAgri1)
![GitHub language count](https://img.shields.io/github/languages/count/pratheesh3780/grapesAgri1)
![Libraries.io dependency status for GitHub repo](https://img.shields.io/librariesio/github/pratheesh3780/grapesAgri1)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/pratheesh3780/grapesAgri1)
[![R-CMD-check](https://github.com/pratheesh3780/grapesAgri1/workflows/R-CMD-check/badge.svg)](https://github.com/pratheesh3780/grapesAgri1/actions)
[![](https://cranlogs.r-pkg.org/badges/grapesAgri1)](https://cran.r-project.org/package=grapesAgri1)

---

## Introduction
<div align="justify">Agricultural experiments demands a wide range of statistical tools for analysis, which includes from Exploratory analysis, Design of experiments and Statistical genetics. Majority of the Agricutural scientists prefer graphical user interface for performing analysis . As R-shiny provides a platform to create intercative user interface, we have utilzed it to produce interactive interfaces for commonly used analysis tools in Agrciultural experiments. grapesAgri1(General R-shiny based Analysis Platform Empowered by Statistics for data analysis in Agriculture-part1) is a collection of shiny based applications for some basic statistical analysis commonly used in agricultural research. It can be utilised by scientific community who prefers an interactive user interface. On using the functions in this package a Graphical User Interface will pop up. Apps Works by simple upload of files in CSV format. Results can be downloaded as docx, PDF or HTML format. Plots and Graphs can be generated, which is also downloadable as .png file.</div>

## Installation
The package can be installed from CRAN as follows:

``` r
# Install from CRAN
install.packages('grapesAgri1', dependencies=TRUE)
```

The development version can be installed from github as follows:

``` r
# Install grapesAgri1 development version from Github using the code below:
if (!require('devtools')) install.packages('devtools')
devtools::install_github("pratheesh3780/grapesAgri1")
```

## usage
```r
grapesAgri1::descApp() # descriptive Statistics and Visualization 
grapesAgri1::corrApp() # Correlation Analysis
grapesAgri1::ttApp() # Compare Means
grapesAgri1::crdApp() # Completely Randomized Design
grapesAgri1::layoutApp() # Field layout of experiments
grapesAgri1::rbdApp() # Randomized Block Design 
```
## Apps included in the package

|Sl. No.| App Title | Function to call |Remark |
|:-----:| :----------- | :-----------:|:----------------|
|1|Descriptive Statistics and Visualization   | descApp()      |Summary Statistics, Summary Statistics by Group, Box plot, Histogram, Q-Q plot and Shapiro-Wilk's test|
|2|Correlation Analysis   | corrApp()      | Simple correlation, Correlation Matrix, correlogram and scatterplot|
|3|Compare Means: Small samle tests  | ttApp()      | One sample t-test, Two sample unpaired t-test, paired t-test, Two sample unpaired Welch t-test, F test, Box plot, Paired Plot|
|4|Completely Randomized Design  | crdApp()      |One-way Analysis of variance (equal and unequal replication), Multiple comparison test, boxplot and barchart with confidence interval|
|5|Field layout of experiments | layoutApp()      |Field layout of following designs can be obtained: Completely Randomized Design (CRD), Randomized Complete Block Design (RCBD), Split-plot design, Strip-plot design, Augmented RCBD|
|6|Randomized Block Design  | rbdApp()      |Two-way Analysis of variance, Multiple comparison test, boxplot and barchart with confidence interval|


## Glimpse to grapesAgri1 in Action!
It is very user friendly. Just upload your file in CSV fomat.
Correlation Analysis         |  Correlation Matrix
:-------------------------:|:-------------------------:
![alt text](man/figures/corr.PNG) |  ![](man/figures/corr2.PNG)

Several types of plots can be generated
Correlogram 1      |  Correlogram 2
:-------------------------:|:-------------------------:
![alt text](man/figures/corr3.PNG) |  ![](man/figures/corr4.PNG)

One-way ANOVA and Two-way ANOVA
Completely Randomized Design  |  Barchart with confidence Interval
:-------------------------:|:-------------------------:
![alt text](man/figures/crd.PNG) |  ![](man/figures/plot.PNG)

Detailed analysis and multiple comparison
Treatmnent grouping  | Field Layout of designs
:-------------------------:|:-------------------------:
![alt text](man/figures/results.PNG) |  ![](man/figures/layout.PNG)


##A LOT MORE.....
