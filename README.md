---
output:
  html_document: default
  pdf_document: default
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


### 2.1.1 Base script

D:/repos/irm/code/rmd/IRM guide.Rmd

D:/repos/irm/code/rmd/irm_modular20210203.Rmd 

This version from 3rd February 2021 includes modular organisation with separate functions.R file.
Includes rule base diagram.

D:/repos/irm/code/rmd/irm_modular.Rmd 

This version from 9th May 2021 includes better coding of multiple innovations, and an extra section on production.




### 2.1.2 Extensions


#### 2.1.2.1 Multiple innovations

D:/repos/rcm/shiny_hide_sidebar/dig_dtd_hma.Rmd this allowed for three innovations but was hardcoded




#### 2.1.2.2 Yield Estimates

D:/repos/rcm/shiny_hide_sidebar/yield_test.Rmd

This looks at fuzzy partitions and compares with the hypothetical yield over a range of precipitation values.

D:/repos/irm/code/rmd/IRM_criteria_correlation.Rmd
D:/repos/irm/code/rmd/IRM_yield_calculation_arsi_hawassa09.Rmd


#### 2.1.2.3 Soil texture from sand, silt clay fractions

D:/repos/irm/code/rmd/Soil texture.rmd


#### 2.1.2.4 Spatially variable sowing date

e.g. D:/repos/sourcing_targets/sowing_date_test_4_CRASA_ZMB_soybean.Rmd


#### 2.1.2.5 Replacement of raster with terra

e.g. D:/repos/sourcing_targets/IRM_4_CRASA_soybean_ZM_GP_spatial_new.rmd

This script includes automatic date settings.
A new parameter for spatially variable season onset.
Allows for different growth stages for temperature and precipitation.
Uses Well Known Text (WKT) for defining the projection instead of proj4.
Uses the terra package for raster and vector management.
New functions for monthly and dekad growth periods.
Uses more dplyr methods such as mutate, select and filter
USDA texture values filled in a separate external csv file
Includes new crisp classification of results to show just optimal and completely suboptimal classifications



## 2.2 Easier choice of criteria

In the current version of IRM it is possible to choose the criteria that are considered in the evaluation of likelihood of adoption / biophysical suitability / socio-economic feasibility. The criteria are set in the external 'priorities' csv file.

However, some components are hard-wired in the script. For instance the climatic suitability expects both temperature and rainfall criteria. Also the map of limitations expects all the criteria to be present (i.e. socio-economic feasibility, soil physical, soil fertility, landscape, land use and climate).

## 2.2.1 Rule Base Diagram

In 2021 a rule base diagram was introduced to make clear to the user how the rule bases contributed to the overall likelihood for adoption (D:/repos/irm/code/rmd/rule_base_diagram_relational_tables.Rmd). This relied on user defined codes, which link rule base stacks with lower level rule bases. The solution assumes some rule base stacks e.g. soil physical, soil fertility. 

The codes are set in the priorities table (an external csv file), and are used in the script to create lists of rule bases and stacks, for creating the rule base diagram. The script assumes that certain codes relate to certain types of stacks (e.g. Climatic Aptitude is always given a code of ba_2b).

The solution is not ideal and it is quite easy to make mistakes in the coding, and the rule base codes are not intuitive. However, the codes are short so are good for field names in the data frame and lists.

The script does not evaluate any rule bases or rule base stacks that are not required.


The diagram shows the hierarchy of the rule bases but the process is not interactive. An improvement would be to allow users to define a hierarchy of rule bases, visualise the rule bases in real-time, and use the model to determine which rule bases to evaluate and how to combine them.

### 2.2.1 Hierarchical database of rule bases

#### 2.2.1.1 Separate Tables

Creating separate tables for each level of the hierarchy is what the script currently does. The script uses regular expressions to get the level for each object, and places objects from the same level within the same table. 


*Table 1. Stack table level 0*

Stack 0 code|Stack 0 name
------------|----------
0_adop|Adoption Likelihood


*Table 2. Stack table level 1*

Stack 1 code|Stack 1 name|Stack 0 code
----------|----------|----------
01_ba|Biophysical Aptitude|0_adop
01_se|Socioeconomic Feasibility|0_adop


*Table 3. Stack table level 2*

Stack 2 code|Stack 2 name|Stack 1 code
----------|----------|----------
02_lu|Land Use Aptitude|01_ba
02_clim|Climatic Aptitude|01_ba
02_soilp|Soil Physical Properties Aptitude|01_ba
02_soilf|Soil Fertility Aptitude|01_ba
02_lscape|Landscape Aptitude|01_ba
02_land|Land Use Aptitude|01_ba
02_fprod|Farm Production Feasibility|01_se
02_mkt|Market Access|01_se


*Table 4. Stack table level 3*

Stack 3 code|Stack 3 name|Stack 2 code
----------|----------|----------
03_mgt|Crop and Farm Management|02_fprod
03_inputs|Access to Farm Inputs|02_fprod
03_lmkt|Access to Local Market|02_mkt
03_wmkt|Access to Woreda Market|02_mkt
03_whole|Access to Wholesalers|02_mkt
03_rain|Rainfall Aptitude|02_clim
03_temp|Temperature Aptitude|02_clim
03_wrsi|WRSI Aptitude|02_clim
03_stext|Soil texture|02_soilp
03_cfrag|Coarse fragments|02_soilp
03_depth|Soil Depth|02_soilp
03_soc|Soil organic carbon|02_soilp
03_cec|CEC|02_soilf
03_ph|Soil pH-H2O|02_soilf
03_elev|Elevation|02_lscape
03_slope|Slope|02_lscape


*Table 5. Stack table level 4*

Stack 4 code|Stack 4 name|Stack 3 code
----------|----------|----------
04_ext|Access to Extension / Information|03_mgt
04_lab|Access to Farm Labour|03_mgt
04_fertpest|Access to Fertilisers and Pesticides|03_inputs
04_seeds|Access to Seeds|03_inputs
04_land|Access to Land|03_inputs
04_raingp|rainfall growing period|03_rain
04_rainest|rainfall est period|03_rain
04_rainveg|rainfall vegetative period|03_rain
04_raintas|rainfall tassel/silk period|03_rain
04_rainfill|rainfall grain fill period|03_rain
04_rainrip|rainfall ripening period|03_rain
04_tempgp|tmean growing period|03_temp
04_tempest|tmean est period|03_temp
04_tempveg|tmean vegetative period|03_temp
04_temptas|tmean tassel/silk period|03_temp
04_tempfill|tmean grain fill period|03_temp
04_temprip|tmean ripening period|03_temp


*Table 6. Stack table level 5*

Stack 5 code|Stack 5 name|Stack 4 code
----------|----------|----------
05_extk|Access to extension officers at Kebele centres|04_ext
05_extw|Access to extension officers at Woreda centres|04_ext
05_lapop|Kebele population density (labour)|04_lab
05_lahh|Average household size (labour)|04_lab
05_ldpop|Kebele population density (land)|04_land
05_ldhh|Average household size (land)|04_land
05_fpl|Access to bags F/P at Local Farmer Group|04_fertpest
05_fpu|Access to bulk F/P at Farmer Union|04_fertpest

#### 2.2.1.2 Adjacency List Model 

A more compact model, and the current form of the priorities csv file, is an adjacency list.
An adjacency list represents a graph (in this case our rule base diagram) as an array of linked lists. The index of the array represents a vertex and each element in its linked list represents the other vertices that form an edge with the vertex.



## 2.3 Simplify the script

The current script is extremely long, especially when evaluating more than one innovation.

A user interface was produced in shiny with some pre-programmed single innovations.

Most of the shiny app still works but the drawing components for adding locations and roads no longer appears.

The existing shiny app does not allow for multiple innovations like the maize-haricot bean intercropping or rotations of crops which are modelled individually but which are analysed together.



### 2.3.1 Shiny

https://cran.r-project.org/web/packages/shiny/shiny.pdf


### 2.3.2 Progressive Web Applications

https://en.wikipedia.org/wiki/Progressive_web_app


### 2.3.3 Electron

https://en.wikipedia.org/wiki/Electron_(software_framework)
https://foretodata.com/how-to-make-a-standalone-desktop-application-with-shiny-and-electron-on-windows/
https://towardsdev.com/converting-a-shiny-app-into-a-standalone-desktop-app-for-windows-ca3656da8468




