---
title: 'grapesAgri1: Collection of Shiny Apps for Data Analysis in Agriculture'
tags:
  - R
  - Data analysis in Agriculture
  - shiny app
  - Design of experiments
  - Compare means
  - Field Layout
  - Correlation Analysis
  - Descriptive Statistics
authors:
  - name: Pratheesh P. Gopinath
    orcid: 0000-0003-3290-0436
    affiliation: "1"
  - name: Rajender Parsad
    affiliation: "2"
  - name: Brigit Joseph
    affiliation: "1"
  - name: Adarsh V. S.
    affiliation: "3"
affiliations:
 - name: Department of Agricultural Statistics, College of Agriculture, Vellayani, Kerala Agricultural University, Kerala, India.
   index: 1
 - name: ICAR-Indian Agricultural Statistics Research Institute, New Delhi, India.
   index: 2
 - name: Department of Agricultural Statistics, BCKV, West Bengal, India
   index: 3
date: June 11, 2021 
bibliography: paper.bib
---

# Summary

<p>
Agricultural experiments demand a wide range of statistical tools for analysis, which includes from Exploratory analysis to Design of experiments and Statistical genetics. It is a challenge for the scientists and the students to find a suitable platform for data analysis and to eventually publish the research outputs in quality journals. Most of the software available for data analysis are proprietary or lack a simple user interface, for example SAS is available in ICAR (Indian Council of Agricultural Research) for data analysis, though it is a highly advanced statistical analysis platform, its complexity hold back students and researchers from using it. Some of the webapplications like WASP (https://ccari.res.in/waspnew.html) and OPSTAT (http://14.139.232.166/opstat/) used by agricultural research community were userfriendly and very simple to use but these applications don't provide opportunities to generate plots and graphs.
</p>
<p>
R, which is an open source software, provides an excellent platform for data analysis. This powerful computation platform is not much utilised by researchers in the agricultural field. Programming and computational knowledge hinders agricultural researchers from using R for analysis. Also, researchers in Agriculture prefer a graphical user interface.
</p>
<p>
Efforts should be made to develop a high quality, reliable open-source platform with a simple interactive user interface for data analysis in agriculture. In this light utilising the features of shiny package in R, we have developed a collection of shiny apps for agricultural research called `grapesAgri1` (General R-shiny based Analysis Platform Empowered by Statistics for data analysis in Agriculture-part1). It is hosted on the web (www.kaugrapes.com), downloadable as a standalone application and now we have realised it in to an R package.
</p>
<p>
On using the functions in `grapesAgri1` package a Graphical User Interface will pop up. Apps are self-explanatory and works by simple upload of files in CSV format. Results can be downloaded in HTML format. Plots and Graphs can be generated, which are also downloadable as .png file.
</p>

# Statement of need
<p>
India has one of the world's largest agricultural education systems. According to the Indian Council of Agricultural Research (ICAR), the main regulator of agricultural education in India, there are 63 State Agricultural Universities, 4 deemed universities and 3 Central Agricultural Universities. These institutions enrol on an annual basis about 15,000 students at Undergraduate and over 7,000 students at Post Graduate and more than 2000 at PhD level. At any point, there are over 75,000 students studying in these institutions. Research activities are performed actively in all these universities. `grapesAgri1` not only serves as a platform for data analysis but also can be used as a teaching tool in Agricultural Statistics. `grapesAgri1` includes some basic statistical tools which were covered in the syllabus of Undergraduate as well as in Post Graduate programmes.

</p>

# Information for Users

<p>
`grapesAgri1` is a collection of six shiny applications. Details of six applications are given below. Model dataset for testing can be downloaded from the main window of the application. Details for preparing CSV file is also included in the main window. Users just need to click on the browse and upload the CSV file for analysis.
</p>
## Apps included in the package

|Sl. No.| App Title | Function to call |Utility|
|:-----:| :----------- | :-----------:|:----------------|
|1|Descriptive Statistics and Visualization   | descApp()      |Summary Statistics, Summary Statistics by Group, Box plot, Histogram, Q-Q plot and Shapiro-Wilk's test|
|2|Correlation Analysis   | corrApp()      | Simple correlation, Correlation Matrix, correlogram and scatterplot|
|3|Compare Means: Small samle tests  | ttApp()      | One sample t-test, Two sample unpaired t-test, paired t-test, Two sample unpaired Welch t-test, F test, Box plot, Paired Plot|
|4|Completely Randomized Design  | crdApp()      |One-way Analysis of variance (equal and unequal replication), Multiple comparison test, boxplot and barchart with confidence interval|
|5|Field layout of experiments | layoutApp()      |Field layout of following designs can be obtained: Completely Randomized Design (CRD), Randomized Complete Block Design (RCBD), Split-plot design, Strip-plot design, Augmented RCBD|
|6|Randomized Block Design  | rbdApp()      |Two-way Analysis of variance, Multiple comparison test, boxplot and barchart with confidence interval|

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
# usage
``` r
grapesAgri1::descApp() # descriptive Statistics and Visualization 
grapesAgri1::corrApp() # Correlation Analysis
grapesAgri1::ttApp() # Compare Means
grapesAgri1::crdApp() # Completely Randomized Design
grapesAgri1::layoutApp() # Field layout of experiments
grapesAgri1::rbdApp() # Randomized Block Design 
```
# Acknowledgements

We wish to thank Kerala Agricultural University and Regional Agricultural Reaserch Station, Vellayani, Thiruvananthapuram for the financial support through the revolving fund scheme and observation trial.

# References
