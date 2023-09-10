###PACKAGES
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(sp)
library(spatstat)
library(maptools)
library(sf)
library(spatialreg)
library(rgdal)
library(spatstat)
library(raster)
library(tigris)
library(ggplot2)
library(broom)
library(tidyverse)
library(ggmap)
library(spdep)
library(lattice)
library(gstat)
library(maptools)
library(gridExtra)
###AREAL ANALYSIS

PitcherReleasePointsFastball <- read.csv("PitcherReleasePointsFastball.csv")
points_pitching_release_point <- SpatialPointsDataFrame(coords = PitcherReleasePointsFastball[,c("meanxrelease", "meanyrelease")],
                                                        data = PitcherReleasePointsFastball[,c( "meanvelocity", "meanspinrate")])

extent_points_pitching_release_point <- extent(points_pitching_release_point)

cell_sizes_pitching_release_point <-  seq(0.1, 3, length.out = 1000)

nrowsvalues_pitching_release_point <-numeric(length(cell_sizes_pitching_release_point))
ncolsvalues_pitching_release_point <- numeric(length(cell_sizes_pitching_release_point))

differenceinyrelease<-numeric(length(cell_sizes_pitching_release_point))

differenceinxrelease<- numeric(length(cell_sizes_pitching_release_point))
numberofdifferentgrids_release_point <- numeric(length(cell_sizes_pitching_release_point))


numberofna_release_point <- numeric(length(cell_sizes_pitching_release_point))
for (i in seq_along(cell_sizes_pitching_release_point)) {
  nrowsvalues_pitching_release_point[i] <- ceiling((extent_points_pitching_release_point@ymax - extent_points_pitching_release_point@ymin) / cell_sizes_pitching_release_point[i])
  ncolsvalues_pitching_release_point[i] <- ceiling((extent_points_pitching_release_point@xmax - extent_points_pitching_release_point@xmin) / cell_sizes_pitching_release_point[i])
  grid_topo_release_point <- GridTopology(cellcentre.offset = c(-4,1.5),
                                          cellsize = c(cell_sizes_pitching_release_point[i], cell_sizes_pitching_release_point[i]),
                                          cells.dim = c(ncolsvalues_pitching_release_point[i], nrowsvalues_pitching_release_point[i]))
  
  
  grid_release_point <- SpatialGrid(grid_topo_release_point, proj4string = CRS(proj4string(points_pitching_release_point)))
  
  
  points_pitching_release_point$grid_index <- over(points_pitching_release_point, grid_release_point)
  
  PitcherReleasePoints <- as.data.frame(points_pitching_release_point)
  
  groupedData <- PitcherReleasePoints %>%
    group_by(grid_index) %>%
    mutate(rangeyrelease = max(meanyrelease) - min(meanyrelease),
           rangexrelease = max(meanxrelease) - min(meanxrelease))
  
  differenceinyrelease[i] <- max(groupedData$rangeyrelease)
  
  differenceinxrelease[i] <- max(groupedData$rangexrelease)
  
  numberofna_release_point[i] <- length(which(is.na(PitcherReleasePoints$grid_index)))
  numberofdifferentgrids_release_point[i] <- length(table(PitcherReleasePoints$grid_index))
}


na0_release_point <- which(numberofna_release_point == 0)

xreleaserangesdataframe <- as.data.frame(differenceinxrelease)
yreleaserangesdataframe <- as.data.frame(differenceinyrelease)

numberofdifferentgridsdataframe_release_point <- as.data.frame(numberofdifferentgrids_release_point)

xreleaserangesdataframe <- xreleaserangesdataframe %>% mutate(row_num = row_number())

yreleaserangesdataframe <- yreleaserangesdataframe %>% mutate(row_num = row_number())

numberofdifferentgridsdataframe_release_point <- numberofdifferentgridsdataframe_release_point %>% mutate(row_num = row_number())

xreleaserangesdataframe <- xreleaserangesdataframe %>%
  filter(row_num %in% na0_release_point)
yreleaserangesdataframe <- yreleaserangesdataframe %>%
  filter(row_num %in% na0_release_point)

differenceinreleases <- merge(yreleaserangesdataframe, xreleaserangesdataframe, by = "row_num")

differenceinreleases2 <- differenceinreleases %>%
  mutate(totaldifference = differenceinxrelease + differenceinyrelease)

finaltesttable_release_point <- merge(differenceinreleases2, numberofdifferentgridsdataframe_release_point,by = "row_num")

cell_sizes_pitching_release_point[7137] 

print(cell_sizes_pitching_release_point[395], digits = 15)

cell_size_release_point <- 1.24374374374374

# Calculate the number of rows and columns in the grid
nrows_release_point <- ceiling((extent_points_pitching_release_point@ymax - extent_points_pitching_release_point@ymin) / cell_size_release_point)
ncols_release_point <- ceiling((extent_points_pitching_release_point@xmax - extent_points_pitching_release_point@xmin) / cell_size_release_point)

grid_topo_release_point <- GridTopology(cellcentre.offset = c(-4,1.5),
                                        cellsize = c(cell_size_release_point, cell_size_release_point),
                                        cells.dim = c(ncols_release_point, nrows_release_point))

grid_release_point <- SpatialGrid(grid_topo_release_point, proj4string = CRS(proj4string(points_pitching_release_point)))

plot(grid_release_point)

grid_polygons_release_point <- as(grid_release_point, "SpatialPolygons")
points_pitching_release_point$grid_index <- over(points_pitching_release_point, grid_polygons_release_point)

PitcherReleasePointsFastball <- as.data.frame(points_pitching_release_point)

table(PitcherReleasePointsFastball$grid_index)
which(is.na(PitcherReleasePointsFastball$grid_index))


PitcherReleasePointsEDA <- PitcherReleasePointsFastball %>%
  group_by(grid_index) %>%
  summarize(meanmeanvelocity = mean(meanvelocity),
            maxvelocity = max(meanvelocity),
            minvelocity = min(meanvelocity),
            meanmeanxrelease = mean(meanxrelease),
            maxxrelease = max(meanxrelease),
            minxrelease = min(meanxrelease),
            meanmeanyrelease = mean(meanyrelease),
            maxyrelease = max(meanyrelease),
            minyrelease = min(meanyrelease),
            count = n()) %>%
  unique()


PitcherReleasePointsInformation <- PitcherReleasePointsFastball %>%
  group_by(grid_index) %>%
  summarize(meanvelocity = mean(meanvelocity),
            count = n()) %>%
  unique()


PitcherReleasePointsInformation <- PitcherReleasePointsInformation %>%
  rename("ID" = "grid_index")

pid <- sapply(slot(grid_polygons_release_point, "polygons"), function(x) slot(x, "ID")) 
p.df <- data.frame( ID=1:length(grid_polygons_release_point), row.names = pid)
grid_polygons_release_point_data_frame <- SpatialPolygonsDataFrame(grid_polygons_release_point, p.df)
grid_polygons_release_point_data_frame <- sp::merge(grid_polygons_release_point_data_frame, PitcherReleasePointsInformation, by = "ID")
plot(grid_polygons_release_point_data_frame)
pitcherreleasepointstidy <- tidy(grid_polygons_release_point_data_frame)
grid_polygons_release_point_data_frame$id <- row.names(grid_polygons_release_point_data_frame)

grid_polygons_release_point_data_frame <- subset(grid_polygons_release_point_data_frame, !is.na(meanvelocity))
grid_polygons_release_point_data_frame$logcount <- log(grid_polygons_release_point_data_frame$count)
PitchReleasePoints.lm <- lm(logcount ~  meanvelocity,
                            data = grid_polygons_release_point_data_frame@data)
summary(PitchReleasePoints.lm)
resid <- PitchReleasePoints.lm$residuals
grid_polygons_release_point_data_frame@data$resid <- resid

pitchreleasepoints_nb <- poly2nb(grid_polygons_release_point_data_frame)
RowStandardized_list <- nb2listw(pitchreleasepoints_nb, style="W")
set.seed(999)
#calculate using MC permutation testing
moran.mc(grid_polygons_release_point_data_frame@data$resid, listw=RowStandardized_list, nsim=50000)


Binary_list <- nb2listw(pitchreleasepoints_nb, style="B")
set.seed(999)
#calculate using MC permutation testing
moran.mc(grid_polygons_release_point_data_frame@data$resid, listw=Binary_list, nsim=50000)


releasepoint_knn1 <- knearneigh(coordinates(grid_polygons_release_point_data_frame), k=1)
releasepoint_knn_nb1 <- knn2nb(releasepoint_knn1)
knn1_list <- nb2listw(releasepoint_knn_nb1, style="W")
set.seed(999)
#calculate using MC permutation testing
moran.mc(grid_polygons_release_point_data_frame@data$resid, listw=knn1_list, nsim=50000)



releasepoint_knn2 <- knearneigh(coordinates(grid_polygons_release_point_data_frame), k=2)
#convert back to nb
releasepoint_knn_nb2 <- knn2nb(releasepoint_knn2)
knn2_list <- nb2listw(releasepoint_knn_nb2, style="W")
set.seed(999)
#calculate using MC permutation testing
moran.mc(grid_polygons_release_point_data_frame@data$resid, listw=knn2_list, nsim=50000)


releasepoint_knn3 <- knearneigh(coordinates(grid_polygons_release_point_data_frame), k=3)
#convert back to nb
releasepoint_knn_nb3 <- knn2nb(releasepoint_knn3)
knn3_list <- nb2listw(releasepoint_knn_nb3, style="W")
set.seed(999)
#calculate using MC permutation testing
moran.mc(grid_polygons_release_point_data_frame@data$resid, listw=knn3_list, nsim=50000)


releasepoint_knn4 <- knearneigh(coordinates(grid_polygons_release_point_data_frame), k=4)
#convert back to nb
releasepoint_knn_nb4 <- knn2nb(releasepoint_knn4)
knn4_list <- nb2listw(releasepoint_knn_nb4, style="W")
set.seed(999)
#calculate using MC permutation testing
moran.mc(grid_polygons_release_point_data_frame@data$resid, listw=knn4_list, nsim=50000)


#sar and car models
releasepoint_sar <- errorsarlm(logcount ~ meanvelocity,
                               data = grid_polygons_release_point_data_frame@data, listw = knn1_list)


releasepoint_car <- spautolm(logcount ~ meanvelocity,
                             data = grid_polygons_release_point_data_frame@data, family = "CAR",
                             listw = knn1_list)


#model comparision
summary(releasepoint_sar)
summary(releasepoint_car)
