
"""
This script is designed to read gridded habitat quality tiff files and calculate the national mean habitat quality score 
author: Shiyu Deng
date: 2025-06-20
email:shiyu.deng.23@ucl.ac.uk
"""

library(terra)
library(sf)   
library(ggplot2)
library(RColorBrewer) 
library(openxlsx) 

folder_path <- "/.../invest/habitatquality"  
tif_files <- list.files(path = folder_path, pattern = "^quality.*\\-09\\.tif$", full.names = TRUE)
quality_rasters <- lapply(tif_files, rast)


n <- length(quality_rasters)


shp_path <- "/.../China_shp.shp"  
shp_path2 <- "/.../九段线和群岛.shp" 

# load the shapefiles
china_boundary <- tryCatch({
  st_read(shp_path)
}, error = function(e) {
  stop("Error in loading China boundary shapefile: ", e$message)
})

china_boundary2 <- tryCatch({
  st_read(shp_path2)
}, error = function(e) {
  stop("Error in loading 九段线 shapefile: ", e$message)
})

cmap <- brewer.pal(9, "Greens")

# unify the coordinate reference system (CRS) of the shapefile and raster files
crs_shp <- st_crs(china_boundary)$proj4string
quality_rasters <- lapply(quality_rasters, function(r) project(r, crs_shp))
# creat a data frame to store mean scores
mean_scores <- data.frame(File = character(), Mean_Score = numeric(), stringsAsFactors = FALSE)

#  loop through each raster file and calculate the national mean score of habitat quality
for (i in 1:n) {
  cat("Processing file:", tif_files[i], "\n")
  tryCatch({
    cat("Processing file:", tif_files[i], "\n")
    cropped_raster <- mask(quality_rasters[[i]], vect(china_boundary))
    mean_score <- global(cropped_raster, fun = "mean", na.rm = TRUE)
    mean_scores <- rbind(mean_scores, data.frame(File = basename(tif_files[i]), Mean_Score = mean_score))
  }, error = function(e) {
    cat("Error processing file:", tif_files[i], "-", e$message, "\n")
  })
}
# print the mean scores
wb <- createWorkbook()
addWorksheet(wb, "Mean Scores")
writeData(wb, "Mean Scores", mean_scores)
saveWorkbook(wb, file = "/.../habitatquality_mean_scores0.5_0.9.xlsx", overwrite = TRUE)