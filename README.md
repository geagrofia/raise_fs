---
output:
  pdf_document: default
  html_document: default
---
# raise_fs
# IRM for RAISE-FS project

## Introduction

There are three major activities:

2.1 General renewal of script(s) to account for R package changes and deprecation (e.g. terra and rgdal) 

2.2 Easier choice of criteria (more dynamic and flexible) easier combination of criteria to understand scalability for different commodities and innovations

2.3 Simplicity in the script (use R shiny app) or restructuring of the script

## 2.1 General renewal of script(s)

As of 29th August 2023 the most recent version of R is 4.3.1 and the most recent version of Rtools is Rtools 43.

Since 2019 a number of packages are being deprecated, while others are new or are being expanded.
The legacy packages maptools, rgdal, and rgeos, underpinning the sp package, will retire in October 2023.

The most relevant for IRM are the deprecation of rgdal and rgeos, and the replacement of raster with the terra package.

Some new helper packages have also been developed, such as tidyterra - which allows a better integration of terra objects like SpatRasters and SpatVectors with the tidyverse (e.g. ggplot).

Previous IRM scripts used sf and raster objects.


## 2.2 Easier choice of criteria

In the current version of IRM it is possible to choose the criteria that are considered in the evaluation of likelihood of adoption / biophysical suitability / socio-economic feasibility.

The criteria are set in the priorities csv file.

However, some components are hard-wired in the script. For instance the climatic suitability expects both temperature and rainfall criteria. Also the map of limitations expects all the criteria to be present (i.e. socio-economic feasibility, soil physical, soil fertility, landscape, land use and climate).

The priorities csv file needs to be given codes which link rule base stacks with lower level rule bases. It is quite easy to make mistakes.


## 2.3 Simplify the script

The current script is extremely long, especially when evaluating more than one innovation.

A user interface was produced in shiny with some pre-programmed innovations.

Most of the interface still functions but the drawing components for adding locations and roads no longer appears.

### Shiny
### Progressive Web Applications
### Electron



