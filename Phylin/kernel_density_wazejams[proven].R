#################################
# INITIALIZATION
#################################

library(dplyr);
library(phylin);
library(lubridate);
library(raster);
library(rgdal);
library(tmap);
library(maptools);
library(spatstat);
library(gstat); 
library(sp);    

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

# Load DKI Jakarta boudary map
W = shapefile("Phylin/dki_kelurahan_1.1.shp")

#alerts1 = alerts20170509;

# Load Waze Alerts data into SpatialPointsDataframe
alerts1$level=100;
alerts1$level[alerts1$subtype=="JAM_HEAVY_TRAFFIC"]=200;
alerts1$level[alerts1$subtype=="JAM_STAND_STILL_TRAFFIC"]=300;
alerts1$level[alerts1$subtype=="HAZARD_ON_ROAD_CAR_STOPPED"]=150;
alerts1$level[alerts1$subtype=="HAZARD_ON_SHOULDER_CAR_STOPPED"]=150;

alerts2 = alerts1;
alerts_level = alerts2[,c("location.x","location.y","level")];
coordinates(alerts_level) = cbind(alerts_level$location.x , alerts_level$location.y);
proj4string(alerts_level) = proj4string(W);

##################################
## Using Spstat
##################################

map_wgs84 = spTransform(W, CRS("+proj=longlat +datum=WGS84"));

alerts_level = remove.duplicates(alerts_level);

window = as.owin(map_wgs84);
Alerts.ppp = ppp(x=alerts_level@coords[,1],y=alerts_level@coords[,2],window=window);

den = density.ppp(Alerts.ppp, sigma = bw.ppl(Alerts.ppp),edge=T);

den_df = as.data.frame(den);

write.csv(den_df,"Phylin/density_output_1minutes.csv",row.names=FALSE);
write.csv(alerts2,"Phylin/dataset_1minutes.csv",row.names=FALSE);



