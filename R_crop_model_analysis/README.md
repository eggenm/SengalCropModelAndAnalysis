# crop_model_analysis #
These are R files for the analysis and visualization of aquacrop Senegal crop model.

## File descriptions and usage ##

## Code files ##
***millet_factors.R*** : Start with this file. It takes the yield summary results generated in the python aquacrop output (millet_FINAL_xxx.csv) and merges it with the soil and weather data to create a master file output with yield and many potential soil and weather factors. Output: millet_factors_fullrun.csv is used as a starting point in other routines below.

***soil_summary.R*** : This file encapsulates useful soil routines and is a supporting script to millet_factors.R. It has a method for combining the first 4 layers (60cm) of soil into aggregated texture components. It uses those texture components to assign a USDA texture class to the soil. Finally it has a a method get_soil() that wraps the other methods to return the complete soil datatable to other routines that might need it.  Relies on inputfile:  AEZ_samples_with_soil3.csv

***weather_summary.R***: This file encapsulates reading weather data and creating monthly aggregated statistics. It is used by millet_factors.R to make the master list of yield influencing factors. The collect_weather_inputs() function relies on per zone csv input files named 'XXXX_weather_from_python.csv' to create a datatable of month_weather_summary used by other routines. 

***yield_resampling_functions.R***: This is a set of functions used in the production of figures for the draft publication. It has methods for taking the median yearly value from all planting dates used at a field(soil_id). It has a method for resampling these yield observations based on soil textures found for a particular zone. Another method takes the set of high rainfall yield observations and low rainfall yield observations and creating a dataset of resampled observations for each of those. All of these assume some set of yieldobservations are supplied to the function.

***water_content_by_texture_class.R*** : This routine creates a variety of figures (7-14) showing water fluxes (soil evaporation, soil water content, transpiration) through the season. It aggregates water fluxes based on the days after planting in a zone.

***result_figure_precipMaxMinYears.R***: This routine creates the major resampled yield figures (Figures 4-6) for extreme rainfall in a season (highest and lowest 5 seasonal precip totals). Relies heavily on yieldResamplingFunctions.R and input:  fullrun_summary_median.csv

***result_figure_combined_zonal_yield_distributions.R***:This routine creates Figure 2 in the text showing yield distributions for all zones in a single graphic.

***result_figure_yield_all_soil_texture***: This routine creates figure 3 in the text showing all yields across zones as function of soil texture.

***senegal_crop_model_analysis.R***:  This was an early version of many of the functions that are in other places. Not sure if there is anything new or useful.

## Input files (found in \input\ ) ##
***millet_FINAL_fullrun.csv*** : Aquacrops seasonal summarized yield output
***millet_FLUX_fullrun.csv*** : Water fluxes in soil and from atmosphere summarized by python routine to ...
***millet_GROWTH_fullrun.csv*** : Aquacrop daily output of growth factors, most importantly yield and biomass,
## Intermediate output files, input to other R code ##
***millet_factors_fullrun.csv***: All soil and weather factors joined to all yield observations including every planting date.
***fullrun_summary_median.csv***: This file is the most common input to data analysis and figures routines. It groups a year/soil combination into a single yield observation by dropping the lowest yielding planting date and taking the median value from the remaining planting dates.