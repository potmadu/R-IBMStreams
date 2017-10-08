#################################
# INITIALIZATION
#################################

library(dplyr);
library(phylin);
library(lubridate);

working_directory = "F:/Github/R-IBMStreams/";

setwd(working_directory);

alerts1 = read.csv("Waze_Json/feed_waze_1.csv",stringsAsFactors=FALSE);
alerts2 = read.csv("Waze_Json/feed_waze_2.csv",stringsAsFactors=FALSE);
alerts3 = read.csv("Waze_Json/feed_waze_3.csv",stringsAsFactors=FALSE);
alerts4 = read.csv("Waze_Json/feed_waze_4.csv",stringsAsFactors=FALSE);
alerts5 = read.csv("Waze_Json/feed_waze_5.csv",stringsAsFactors=FALSE);

alerts1 = rbind(alerts1,alerts2,alerts3,alerts4,alerts5);
alerts1 = alerts1 %>% 
filter(city %in% c("Jakarta Utara","Jakarta Selatan","Jakarta Pusat","Jakarta Barat","Jakarta Timur")) %>%
as.data.frame();

alerts20170509 = read.csv("F:/JSC/Waze/alerts_20170509_all.csv",stringsAsFactors=FALSE,header=FALSE);
colnames(alerts20170509)[16] = "uuid";
colnames(alerts20170509)[14] = "subtype";
colnames(alerts20170509)[6] = "location.x";
colnames(alerts20170509)[7] = "location.y";

#################################
# Using Phylin
#################################

dki_grid = read.csv("Phylin/coords.csv",stringsAsFactors=FALSE);
dki_grid = dki_grid[,c("X","Y")];
colnames(dki_grid) = c("x","y");

row.names(alerts1) = paste(alerts1$uuid,alerts1$pubMillis); 

alerts1$level = 100;

alerts_loc = alerts1[,c("location.x","location.y")];
alerts_level = t(alerts1[,"level"]);

alerts_level = data.frame(alerts_level);
colnames(alerts_level) = row.names(alerts1);

alerts_idw = phylin::idw(alerts_level, alerts_loc, dki_grid);
alerts_idw2 = phylin::idw(alerts_level, alerts_loc, dki_grid, 'Modified', R=10);

grid.image(alerts_idw2, dki_grid, main='IDW interpolation', xlab='Longitude', 
           ylab='Latitude', sclab="Genetic distance to sample s2")

> grid.image(alerts_idw2, dki_grid, main='IDW interpolation', xlab='Longitude', 
+            ylab='Latitude', sclab="Genetic distance to sample s2")
Error in tapply(intpl[, ic], list(grid[, 1], grid[, 2]), mean) : 
  total number of levels >= 2^31
> 

##################################
## Using Gstat
##################################

library(raster)
library(rgdal)
library(tmap)
library(maptools)
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Load DKI Jakarta boudary map
W = shapefile("Phylin/dki_kelurahan_1.1.shp")

alerts1 = alerts20170509;

# Load Waze Alerts data into SpatialPointsDataframe
alerts1$level=100;
alerts1$level[alerts1$subtype=="JAM_HEAVY_TRAFFIC"]=200;
alerts1$level[alerts1$subtype=="JAM_STAND_STILL_TRAFFIC"]=300;
alerts1$level[alerts1$subtype=="HAZARD_ON_ROAD_CAR_STOPPED"]=150;
alerts1$level[alerts1$subtype=="HAZARD_ON_SHOULDER_CAR_STOPPED"]=150;

alerts2 = alerts1[1:500,];
alerts_level = alerts2[,c("location.x","location.y","level")];
coordinates(alerts_level) = cbind(alerts_level$location.x , alerts_level$location.y);
proj4string(alerts_level) = proj4string(W);

alerts_level@bbox = W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(alerts_level) +
  tm_dots(col="level", palette = "RdBu", auto.palette.mapping = FALSE,
             title="Sampled precipitation \n(in inches)", size=10) +
  tm_text("level", just="left", xmod=0.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

# Create an empty grid where n is the total number of cells
grd              = as.data.frame(spsample(W, "regular", n=50000))
names(grd)       = c("X", "Y")
coordinates(grd) = c("X", "Y")
gridded(grd)     = TRUE  # Create SpatialPixel object
fullgrid(grd)    = TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) = proj4string(W);

alerts_level.idw = gstat::idw(log(level) ~ 1, alerts_level, newdata=grd,idp=0.01);

# Convert to raster object then clip to Texas
r       = raster(alerts_level.idw);
r.m     = mask(r, W);

tm_shape(r.m) + 
  tm_raster(n=1000,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(alerts_level) + tm_dots(size=1) +
  tm_legend(legend.outside=TRUE)

##################################
## Using Spstat
##################################

library(spatstat);

p = as(alerts_level["level"], "ppp")
plot(Smooth.ppp(p,0.03))

spstat.smooth = Smooth.ppp(p,0.03);
r = raster(spstat.smooth);
proj4string(r) = proj4string(W);
r.m = mask(r, W);
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_legend(legend.outside=TRUE)

spstat.idw = idw(p);
r = raster(spstat.idw);
proj4string(r) = proj4string(W);
r.m = mask(r, W);
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_legend(legend.outside=TRUE)

plot(density.ppp(p, sigma = bw.ppl(p),edge=T))

plot(density.ppp(p, sigma = bw.scott(p)[1],edge=T))
plot(density.ppp(p, sigma = bw.scott(p)[2],edge=T))
plot(density.ppp(p, sigma = bw.diggle(p),edge=T))

den = density.ppp(p, sigma = bw.ppl(p),edge=T);

r = raster(den);
proj4string(r) = proj4string(W);
r.m = mask(r, W);
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_legend(legend.outside=TRUE)

den_df = as.data.frame(den);

write.csv(den_df,"Phylin/density_output.csv",row.names=FALSE);

######################################

map_wgs84 = spTransform(W, CRS("+proj=longlat +datum=WGS84"))

alerts_level = remove.duplicates(alerts_level)

window = as.owin(map_wgs84)
Alerts.ppp = ppp(x=alerts_level@coords[,1],y=alerts_level@coords[,2],window=window)

den = density.ppp(Alerts.ppp, sigma = bw.ppl(Alerts.ppp),edge=T);

den_df = as.data.frame(den);

write.csv(den_df,"Phylin/density_output.csv",row.names=FALSE);
write.csv(alerts2,"Phylin/dataset_500.csv",row.names=FALSE);



