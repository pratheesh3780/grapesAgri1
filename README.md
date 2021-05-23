# `grapes`: Collection of shiny applications for data analysis in Agriculture <img src="https://github.com/pratheesh3780/inst/extdata/logo.png" align="right" alt="logo" width="173" height = "200" style = "border: none; float: right;">
###### Version : 0.1.0; Copyright (C) 2020-2021: [Pratheesh P. Gopinath](https://www.coagrapes.com); License: [GPL-3](https://www.r-project.org/Licenses/)
## Introduction
Agricultural experiments demands a wide range of statistical tools for analysis, which includes from Exploratory analysis, Design of experiments and Statistical genetics. Majority of the Agricutural scientists prefer graphical user interface for performing analysis . As Rshiny provides a platform to create intercative user interface, we have utilzed it to produce interactive interfaces for commonly used analysis tools in Agrciultural experiments. GRAPES (General Rshiny based Analysis Platform Empowered by Statistics) is a collection of shiny based applications that can be utilised by scientific community who prefers an interactive user interface. On using the functions in this package a Graphical User Interface will pop up. Apps Works by simple upload of files in CSV format. Results can be downloaded as word file or HTML file. Plots and Graphs can be generated, which is also downloadable as .png file.
## Installation
The development version can be installed from github as follows:

``` r
# Install grapes development version from Github using the code below:
if (!require('devtools')) install.packages('devtools')
devtools::install_github("pratheesh3780/grapes")
```
## Apps included in the package

| App Title | Function to call |Remark |
| :----------- | :-----------:|:----------------|
|Descriptive Statistics and Visualization   | descAPP()      |Summary Statistics, Summary Statistics by Group, Box plot, Histogram, Q-Q plot and Shapiro-Wilk's test|
|Correlation Analysis   | corrApp()      | Simple correlation, Correlation Matrix, correlogram and scatterplot|
|Compare Means  | ttApp()      | t-test, Paired t-test, F test|
