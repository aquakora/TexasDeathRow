# Texas Death Row Inmate Analysis

This Repo is used to store an analysis of Texas Death Row Inmates that I completed.

Files in this repo are:

1. Original R code that includes everything (webscraping, data processing, exploratory data analysis, and topic modelling)
2. The .Rmd file and HTML files that are used for the write up of the analysis
3. Three data sources used in the analysis

## Data

1. inmateData.csv - Basic inmate data, including name, TDCJ number, age, execution date, race, county and the last statement
2. executionsPerYear.csv - Number of executions per year, and race
3. offendersOnDeathRow.csv - This file is not currently used in the analysis.  Includes data on inmates currently sitting on Death Row.  

## Analysis

There are two analysis sections in this file, and a topic modelling section.

1.  First, I take a look at some general characteristcs of the inmates that were executed
2.  The second analysis is a look at the statements that each inmate provided before their executions
3.  The third is an experiment in topic modelling using the last statements
