# read in rds
prasts <- readRDS("data/spatial/10m_lidar_depth.rds")

# write to raster
terra::writeRaster(prasts, paste0("output/lidar.tif"),
                   overwrite = T)
