# **SVD-Component Mortality Model using Child or Child/Adult Mortality as Inputs** 

Published article: [*A General Age-Specific Mortality Model With an Example Indexed by Child Mortality or Both Child and Adult Mortality*](https://doi.org/10.1007/s13524-019-00785-3) - Clark, S.J. Demography (2019). https://doi.org/10.1007/s13524-019-00785-3

Preprint: [*A General Age-Specific Mortality Model with An Example Indexed by Child or Child/Adult Mortality*](https://arxiv.org/abs/1612.01408).
   
Also see: [*A Singular Value Decomposition-based Factorization and Parsimonious Component Model of Demographic Quantities Correlated by Age: Predicting Complete Demographic Age Schedules with Few Parameters*](https://arxiv.org/abs/1504.02057).

## Overview
This GitHub repository contains code for the model and the LaTeX source files for the manuscript that describes the model, published in the journal [*Demography*](https://doi.org/10.1007/s13524-019-00785-3) 

All of the code used to create the model is contained in the R Markdown file *Rmd/SVD-Comp.Rmd*.  The code saves R Data objects containing the data, various intermediate results, and outputs in *RData*.  It also saves tables and figures for the manuscript in *tables* and *figures*.  It is compiled and rendered into the PDF *Rmd/SVD-Comp.pdf* that contains all the results.  **NB**: The Human Mortality Database web site used to store the full dataset in a file with a standard name that could be automatically downloaded; that is no longer the case in 2022 after an update. Now, you must download the data manually; follow the directions in the 2022 version of the Rmd file. There is a 2024 update now as well.

The manuscript itself is composed of two LaTeX files in *manuscript* - one for the main manuscript and one for the appendices.  They cross reference each other so you need to compile both and then recompile both to get all the references updated.

Finally, the original not-on-CRAN package to implement the final method - **svdComp5q0** - is is stored in *package* as an RStudio package project. 

## CRAN Package **SVDMx** 
* The **SVDMx** package on CRAN implements the mortality model using 2018, 2022, or 2024 calibration data
* Install using the *Packages* pane of RStudio, or
* Install from the R command line using "install.packages("SVDMx")"
* **SVDMx** is very compact in compressed form: 121KB!

## General Requirements: Software, etc.
This material has been prepared on an Apple Macintosh running Mac OS High Sierra.  All software necessary is open source and free, including
* R: [Download](https://cran.r-project.org/bin/macosx/)
* RStudio: [Download](https://www.rstudio.com/products/rstudio/download/)
* LaTeX via 
    * MacTex: [Download](http://www.tug.org/mactex/index.html)
    * TeXShop: [Download](https://pages.uoregon.edu/koch/texshop/obtaining.html)

## To Run the Markdown File
* If you're a regular GitHub user, clone the *svd-comp* repository
* If you're not a regular GitHub user, go to the upper right corner of this web page and click 'Clone or Download' and in the small window that appears click 'Download Zip'.  After the .zip file has downloaded, move it somewhere convenient and unzip it.
*  Use RStudio to open *Rmd/SVD-Comp.Rmd*

## Directory structure
* *Rmd* contains the R Markdown file that executes the main code - *SVD-Comp.Rmd*.
* *R* contains R code
    * *logQuad* implementation code from Wilmoth et al.  There is a directory that contains the relevant papers describing the Log Quad model.
    * *getHMD.R* contains uncommented code used to download and parse the HMD life tables from the HMD web site.
* *RData* contains a set of intermediate and final data sets, empty until populated by the code.
* *data* contains the HMD data downloaded from the HMD web site.  
    * *non-HMD life tables* contains the data used to test the model on other countries.  The .csv files are read by *Rmd/SVD-Comp.Rmd* to conduct the comparisons.
    * *HMD* is where the downloaded HMD data are stored, empty until populated by the code.
* *tables* contains a set of text files, .txt and .csv, that contain the contents of all the tables in the manuscript. These are produced by code in the R Markdown file *Rmd/SVD-Comp.Rmd*.
* *figures* contains a set of PDF files that contain the figures included in the manuscript.  These are produced by code in the R Markdown file *Rmd/SVD-Comp.Rmd*.
* *manuscript* contains two .tex files and related .bib and .bst files for the main manuscript and appendices.
* *package* contains an RStudio package project with all of the materials used to create the **svdComp5q0** R package that implements the model.
