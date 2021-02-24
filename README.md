# MIS581-M7

This project cleans and joins data to evaluate the food inspection process of King County, Washington. 
This project specifically looks at data from 2016-2018 as well as demographic and economic data of the census tracks in King County. 


Primary data is Food Establishment Inspection Data,produced and maintained by the Public Health Department of King County, Washington: https://data.kingcounty.gov/Health-Wellness/Food-Establishment-Inspection-Data/f29f-zza5/data

Data is joined with: 
- Small Area Demographic Estimates (SADE) data, produced by the Washington State Office of Financial Management (OFM): https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/estimates-april-1-population-age-sex-race-and-hispanic-origin
- Qualified Census Tract (QCT) Table Generator data from the U.S. Department of Housing and Urban Development (HUD) Office of Policy Development and Research (PD&R)  https://www.huduser.gov/portal/qct/QCT_Algorithm_2021.html


NOTE: Prior to running project, need to geocode the Food Inspection data using the US Census Geocoder: https://geocoding.geo.census.gov/


After data processing, descriptive statistics, correlation analysis, and an attempt at multiple linear regression is made. 
