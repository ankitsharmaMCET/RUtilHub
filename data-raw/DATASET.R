# library(sf)
# library(ggplot2)
#
#
# # Read STATE shapefile
# STATE_SHAPEDATA <- st_read("C:/Users/Lab 2/Desktop/misc/Administrative Boundary Database/STATE_BOUNDARY.shp")
#
# # Read DISTRICT shapefile
# DISTRICT_SHAPEDATA <- st_read("C:/Users/Lab 2/Desktop/misc/Administrative Boundary Database/DISTRICT_BOUNDARY.shp")
#
# # Read SUBDISTRICT shapefile
# SUBDISTRICT_SHAPEDATA <- st_read("C:/Users/Lab 2/Desktop/misc/Administrative Boundary Database/SUBDISTRICT_BOUNDARY.shp")
#
# # Combine into a list
# shapefile_list <- list(STATE_SHAPEDATA = STATE_SHAPEDATA, DISTRICT_SHAPEDATA = DISTRICT_SHAPEDATA, SUBDISTRICT_SHAPEDATA = SUBDISTRICT_SHAPEDATA)
#
# # View the structure of the list
# str(shapefile_list)
#
#
# use_data(shapefile_list, overwrite = TRUE)
#
#
# ## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)
