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

kasus_kosong1 = alerts1[100:101,];
kasus_kosong2 = alerts1[100:102,];
kasus_kosong3 = alerts1[100:103,];
kasus_kosong4 = alerts1[100:101,];
kasus_kosong5 = alerts1[100:102,];
kasus_kosong10 = alerts1[100:110,];

alerts_all = rbind(alerts1,alerts2,alerts3,alerts4,alerts5);
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

##################################
## Using Spstat
##################################

calc_kerneldensity_diggle = function(input){

	alerts2 = input;

	# Load Waze Alerts data into SpatialPointsDataframe
	alerts2$level=100;
	alerts2$level[alerts2$subtype=="JAM_HEAVY_TRAFFIC"]=200;
	alerts2$level[alerts2$subtype=="JAM_STAND_STILL_TRAFFIC"]=300;
	alerts2$level[alerts2$subtype=="HAZARD_ON_ROAD_CAR_STOPPED"]=150;
	alerts2$level[alerts2$subtype=="HAZARD_ON_SHOULDER_CAR_STOPPED"]=150;

	alerts_level = alerts2[,c("location.x","location.y","level")];
	coordinates(alerts_level) = cbind(alerts_level$location.x , alerts_level$location.y);
	proj4string(alerts_level) = proj4string(W);

	map_wgs84 = spTransform(W, CRS("+proj=longlat +datum=WGS84"));

	alerts_level = remove.duplicates(alerts_level);

	window = as.owin(map_wgs84);
	Alerts.ppp = ppp(x=alerts_level@coords[,1],y=alerts_level@coords[,2],window=window);

	den = density.ppp(Alerts.ppp, sigma = bw.diggle(Alerts.ppp),edge=T);

	den_df = as.data.frame(den);

	return(den);

}

calc_kerneldensity_diggle_512 = function(input){

	alerts2 = input;

	# Load Waze Alerts data into SpatialPointsDataframe
	alerts2$level=100;
	alerts2$level[alerts2$subtype=="JAM_HEAVY_TRAFFIC"]=200;
	alerts2$level[alerts2$subtype=="JAM_STAND_STILL_TRAFFIC"]=300;
	alerts2$level[alerts2$subtype=="HAZARD_ON_ROAD_CAR_STOPPED"]=150;
	alerts2$level[alerts2$subtype=="HAZARD_ON_SHOULDER_CAR_STOPPED"]=150;

	alerts_level = alerts2[,c("location.x","location.y","level")];
	coordinates(alerts_level) = cbind(alerts_level$location.x , alerts_level$location.y);
	proj4string(alerts_level) = proj4string(W);

	map_wgs84 = spTransform(W, CRS("+proj=longlat +datum=WGS84"));

	alerts_level = remove.duplicates(alerts_level);

	window = as.owin(map_wgs84);
	Alerts.ppp = ppp(x=alerts_level@coords[,1],y=alerts_level@coords[,2],window=window,dimyx=c(512,512));

	den = density.ppp(Alerts.ppp, sigma = bw.diggle(Alerts.ppp),edge=T);

	den_df = as.data.frame(den);

	return(den);

}

calc_kerneldensity_ppl = function(input){

	alerts2 = input;

	# Load Waze Alerts data into SpatialPointsDataframe
	alerts2$level=100;
	alerts2$level[alerts2$subtype=="JAM_HEAVY_TRAFFIC"]=200;
	alerts2$level[alerts2$subtype=="JAM_STAND_STILL_TRAFFIC"]=300;
	alerts2$level[alerts2$subtype=="HAZARD_ON_ROAD_CAR_STOPPED"]=150;
	alerts2$level[alerts2$subtype=="HAZARD_ON_SHOULDER_CAR_STOPPED"]=150;

	alerts_level = alerts2[,c("location.x","location.y","level")];
	coordinates(alerts_level) = cbind(alerts_level$location.x , alerts_level$location.y);
	proj4string(alerts_level) = proj4string(W);

	map_wgs84 = spTransform(W, CRS("+proj=longlat +datum=WGS84"));

	alerts_level = remove.duplicates(alerts_level);

	window = as.owin(map_wgs84);
	Alerts.ppp = ppp(x=alerts_level@coords[,1],y=alerts_level@coords[,2],window=window);

	den = density.ppp(Alerts.ppp, sigma = bw.ppl(Alerts.ppp),edge=T);

	den_df = as.data.frame(den);

	return(den);

}

calc_kerneldensity_scott = function(input){

	alerts2 = input;

	# Load Waze Alerts data into SpatialPointsDataframe
	alerts2$level=100;
	alerts2$level[alerts2$subtype=="JAM_HEAVY_TRAFFIC"]=200;
	alerts2$level[alerts2$subtype=="JAM_STAND_STILL_TRAFFIC"]=300;
	alerts2$level[alerts2$subtype=="HAZARD_ON_ROAD_CAR_STOPPED"]=150;
	alerts2$level[alerts2$subtype=="HAZARD_ON_SHOULDER_CAR_STOPPED"]=150;

	alerts_level = alerts2[,c("location.x","location.y","level")];
	coordinates(alerts_level) = cbind(alerts_level$location.x , alerts_level$location.y);
	proj4string(alerts_level) = proj4string(W);

	map_wgs84 = spTransform(W, CRS("+proj=longlat +datum=WGS84"));

	alerts_level = remove.duplicates(alerts_level);

	window = as.owin(map_wgs84);
	Alerts.ppp = ppp(x=alerts_level@coords[,1],y=alerts_level@coords[,2],window=window);

	den = density.ppp(Alerts.ppp, sigma = bw.scott(Alerts.ppp)[1],edge=T);

	den_df = as.data.frame(den);

	return(den);

}

den_df1_diggle = calc_kerneldensity_diggle(alerts1);
den_df2_diggle = calc_kerneldensity_diggle(alerts2);
den_df3_diggle = calc_kerneldensity_diggle(alerts3);
den_df4_diggle = calc_kerneldensity_diggle(alerts4);
den_df5_diggle = calc_kerneldensity_diggle(alerts5);
den_df_diggle_all = calc_kerneldensity_diggle(alerts_all);

den_df1_ppl = calc_kerneldensity_ppl(alerts1);
den_df2_ppl = calc_kerneldensity_ppl(alerts2);
den_df3_ppl = calc_kerneldensity_ppl(alerts3);
den_df4_ppl = calc_kerneldensity_ppl(alerts4);
den_df5_ppl = calc_kerneldensity_ppl(alerts5);
den_df_ppl_all = calc_kerneldensity_ppl(alerts_all);

den_df1_scott = calc_kerneldensity_scott(alerts1);
den_df2_scott = calc_kerneldensity_scott(alerts2);
den_df3_scott = calc_kerneldensity_scott(alerts3);
den_df4_scott = calc_kerneldensity_scott(alerts4);
den_df5_scott = calc_kerneldensity_scott(alerts5);
den_df_scott_all = calc_kerneldensity_scott(alerts_all);

par(mfrow=c(3,6));

plot(den_df1_diggle);
plot(den_df2_diggle);
plot(den_df3_diggle);
plot(den_df4_diggle);
plot(den_df5_diggle);
plot(den_df_diggle_all);

plot(den_df1_ppl);
plot(den_df2_ppl);
plot(den_df3_ppl);
plot(den_df4_ppl);
plot(den_df5_ppl);
plot(den_df_ppl_all);

plot(den_df1_scott);
plot(den_df2_scott);
plot(den_df3_scott);
plot(den_df4_scott);
plot(den_df5_scott);
plot(den_df_scott_all);

den_kasuskosong_diggle1 = calc_kerneldensity_diggle(kasus_kosong1);
den_kasuskosong_ppl1 = calc_kerneldensity_ppl(kasus_kosong1);
den_kasuskosong_scott1 = calc_kerneldensity_scott(kasus_kosong1);

den_kasuskosong_diggle2 = calc_kerneldensity_diggle(kasus_kosong2);
den_kasuskosong_ppl2 = calc_kerneldensity_ppl(kasus_kosong2);
den_kasuskosong_scott2 = calc_kerneldensity_scott(kasus_kosong2);

den_kasuskosong_diggle3 = calc_kerneldensity_diggle(kasus_kosong3);
den_kasuskosong_ppl3 = calc_kerneldensity_ppl(kasus_kosong3);
den_kasuskosong_scott3 = calc_kerneldensity_scott(kasus_kosong3);

den_kasuskosong_diggle4 = calc_kerneldensity_diggle(kasus_kosong4);
den_kasuskosong_ppl4 = calc_kerneldensity_ppl(kasus_kosong4);
den_kasuskosong_scott4 = calc_kerneldensity_scott(kasus_kosong4);

den_kasuskosong_diggle5 = calc_kerneldensity_diggle(kasus_kosong5);
den_kasuskosong_ppl5 = calc_kerneldensity_ppl(kasus_kosong5);
den_kasuskosong_scott5 = calc_kerneldensity_scott(kasus_kosong5);

den_kasuskosong_diggle10 = calc_kerneldensity_diggle(kasus_kosong10);
den_kasuskosong_ppl10 = calc_kerneldensity_ppl(kasus_kosong10);
den_kasuskosong_scott10 = calc_kerneldensity_scott(kasus_kosong10);

par(mfrow=c(3,6));

plot(den_kasuskosong_diggle1)
plot(den_kasuskosong_diggle2)
plot(den_kasuskosong_diggle3)
plot(den_kasuskosong_diggle4)
plot(den_kasuskosong_diggle5)
plot(den_kasuskosong_diggle10)

plot(den_kasuskosong_ppl1)
plot(den_kasuskosong_ppl2)
plot(den_kasuskosong_ppl3)
plot(den_kasuskosong_ppl4)
plot(den_kasuskosong_ppl5)
plot(den_kasuskosong_ppl10)

plot(den_kasuskosong_scott1)
plot(den_kasuskosong_scott2)
plot(den_kasuskosong_scott3)
plot(den_kasuskosong_scott4)
plot(den_kasuskosong_scott5)
plot(den_kasuskosong_scott10)

par(mfrow=c(1,3));

plot(den_df_diggle_all);
plot(den_df_ppl_all);
plot(den_df_scott_all);

write.csv(den_df,"Phylin/density_output_1minutes.csv",row.names=FALSE);
write.csv(alerts2,"Phylin/dataset_1minutes.csv",row.names=FALSE);

> den_kasuskosong_diggle1 = calc_kerneldensity_diggle(kasus_kosong1);
Error in bw.diggle(Alerts.ppp) : K function yields too many NA/NaN values
> kasus_kosong1
    country nThumbsUp          city reportRating confidence reliability type
100      ID         0 Jakarta Utara            1          0           5  JAM
                                    uuid roadType magvar              subtype
100 4bbc4811-faf8-37ee-8f8c-7b7118404125        2    261 JAM_MODERATE_TRAFFIC
                  street location.x location.y   pubMillis reportDescription
100 Danau Sunter Selatan   106.8766  -6.145631 1.50727e+12                  
> den_kasuskosong_diggle1 = calc_kerneldensity_diggle(kasus_kosong1);
Error in bw.diggle(Alerts.ppp) : K function yields too many NA/NaN values
> den_kasuskosong_ppl1 = calc_kerneldensity_ppl(kasus_kosong1);
Error in seq.default(from = log(from), to = log(to), length.out = length.out) : 
  'from' cannot be NA, NaN or infinite
> den_kasuskosong_scott1 = calc_kerneldensity_scott(kasus_kosong1);
Error: all(sigma > 0) is not TRUE

#################################
## SJ DPI Method
#################################

calc_kerneldensity_diggle_st = function(input){

	alerts2 = input;

	# Load Waze Alerts data into SpatialPointsDataframe
	alerts2$level=100;
	alerts2$level[alerts2$subtype=="JAM_HEAVY_TRAFFIC"]=200;
	alerts2$level[alerts2$subtype=="JAM_STAND_STILL_TRAFFIC"]=300;
	alerts2$level[alerts2$subtype=="HAZARD_ON_ROAD_CAR_STOPPED"]=150;
	alerts2$level[alerts2$subtype=="HAZARD_ON_SHOULDER_CAR_STOPPED"]=150;

	alerts_level = alerts2[,c("location.x","location.y","level")];
	coordinates(alerts_level) = cbind(alerts_level$location.x , alerts_level$location.y);
	proj4string(alerts_level) = proj4string(W);

	map_wgs84 = spTransform(W, CRS("+proj=longlat +datum=WGS84"));

	alerts_level = remove.duplicates(alerts_level);

	window = as.owin(map_wgs84);
	Alerts.ppp = ppp(x=alerts_level@coords[,1],y=alerts_level@coords[,2],window=window);

	den = density.ppp(Alerts.ppp, width = "SJ", n = 1024, sigma = bw.diggle(Alerts.ppp),edge=T);

	den_df = as.data.frame(den);

	return(den);

}

den_diggle_sj1 = calc_kerneldensity_diggle_st(alerts1);
den_diggle_sj2 = calc_kerneldensity_diggle_st(alerts2);
den_diggle_sj3 = calc_kerneldensity_diggle_st(alerts3);
den_diggle_sj4 = calc_kerneldensity_diggle_st(alerts4);
den_diggle_sj5 = calc_kerneldensity_diggle_st(alerts5);
den_diggle_sj_all = calc_kerneldensity_diggle_st(alerts_all);

par(mfrow=c(2,6));

plot(den_df1_diggle);
plot(den_df2_diggle);
plot(den_df3_diggle);
plot(den_df4_diggle);
plot(den_df5_diggle);
plot(den_df_diggle_all);

plot(den_diggle_sj1);
plot(den_diggle_sj2);
plot(den_diggle_sj3);
plot(den_diggle_sj4);
plot(den_diggle_sj5);
plot(den_diggle_sj_all);

#################################
## Spatial Clustering DBSCAN
#################################

#################################
## Benchmark Quantile Threshold
## Results : 95% quantile threshold
#################################

den_df = as.data.frame(den_df_diggle_all);

den75 = quantile(den_df$value,0.75); 
den80 = quantile(den_df$value,0.80);
den85 = quantile(den_df$value,0.85);
den90 = quantile(den_df$value,0.90);
den95 = quantile(den_df$value,0.95);

den_df_diggle_all75 = den_df %>% filter(value>=den75) %>% as.data.frame();
den_df_diggle_all80 = den_df %>% filter(value>=den80) %>% as.data.frame();
den_df_diggle_all85 = den_df %>% filter(value>=den85) %>% as.data.frame();
den_df_diggle_all90 = den_df %>% filter(value>=den90) %>% as.data.frame();
den_df_diggle_all95 = den_df %>% filter(value>=den95) %>% as.data.frame();

DBSCAN75 = dbscan(cbind(den_df_diggle_all75$x, den_df_diggle_all75$y), eps = 0.0022, minPts = 3);
DBSCAN80 = dbscan(cbind(den_df_diggle_all80$x, den_df_diggle_all80$y), eps = 0.0022, minPts = 3);
DBSCAN85 = dbscan(cbind(den_df_diggle_all85$x, den_df_diggle_all85$y), eps = 0.0022, minPts = 3);
DBSCAN90 = dbscan(cbind(den_df_diggle_all90$x, den_df_diggle_all90$y), eps = 0.0022, minPts = 3);
DBSCAN95 = dbscan(cbind(den_df_diggle_all95$x, den_df_diggle_all95$y), eps = 0.0022, minPts = 3);

par(mfrow=c(1,5));

plot(cbind(den_df_diggle_all75$x, den_df_diggle_all75$y), col = DBSCAN75$cluster + 1L);
plot(cbind(den_df_diggle_all80$x, den_df_diggle_all80$y), col = DBSCAN80$cluster + 1L);
plot(cbind(den_df_diggle_all85$x, den_df_diggle_all85$y), col = DBSCAN85$cluster + 1L);
plot(cbind(den_df_diggle_all90$x, den_df_diggle_all90$y), col = DBSCAN90$cluster + 1L);
plot(cbind(den_df_diggle_all95$x, den_df_diggle_all95$y), col = DBSCAN95$cluster + 1L);

#################################
## Benchmark Quantile Threshold
## Results : 95% quantile threshold
#################################

den_df_1 = as.data.frame(den_df1_diggle);
den_df_2 = as.data.frame(den_df2_diggle);
den_df_3 = as.data.frame(den_df3_diggle);
den_df_4 = as.data.frame(den_df4_diggle);
den_df_5 = as.data.frame(den_df5_diggle);
den_df_all = as.data.frame(den_df_diggle_all);

den1 = quantile(den_df_1$value,0.95); 
den2 = quantile(den_df_2$value,0.95);
den3 = quantile(den_df_3$value,0.95);
den4 = quantile(den_df_4$value,0.95);
den5 = quantile(den_df_5$value,0.95);
den_all = quantile(den_df_all$value,0.95);

den1_95 = den_df_1 %>% filter(value>=den1) %>% as.data.frame();
den2_95 = den_df_2 %>% filter(value>=den2) %>% as.data.frame();
den3_95 = den_df_3 %>% filter(value>=den3) %>% as.data.frame();
den4_95 = den_df_4 %>% filter(value>=den4) %>% as.data.frame();
den5_95 = den_df_5 %>% filter(value>=den5) %>% as.data.frame();
den_all_95 = den_df_all %>% filter(value>=den_all) %>% as.data.frame();

DBSCAN1 = dbscan(cbind(den_df_1$x, den_df_1$y), eps = 0.0030, minPts = 3);
DBSCAN2 = dbscan(cbind(den_df_2$x, den_df_2$y), eps = 0.0030, minPts = 3);
DBSCAN3 = dbscan(cbind(den_df_3$x, den_df_3$y), eps = 0.0030, minPts = 3);
DBSCAN4 = dbscan(cbind(den_df_4$x, den_df_4$y), eps = 0.0030, minPts = 3);
DBSCAN5 = dbscan(cbind(den_df_5$x, den_df_5$y), eps = 0.0030, minPts = 3);
DBSCAN_ALL = dbscan(cbind(den_df_all$x, den_df_all$y), eps = 0.0030, minPts = 3);

par(mfrow=c(1,6));

plot(cbind(den1_95$x, den1_95$y), col = DBSCAN1$cluster + 1L);
plot(cbind(den2_95$x, den2_95$y), col = DBSCAN2$cluster + 1L);
plot(cbind(den3_95$x, den3_95$y), col = DBSCAN3$cluster + 1L);
plot(cbind(den4_95$x, den4_95$y), col = DBSCAN4$cluster + 1L);
plot(cbind(den5_95$x, den5_95$y), col = DBSCAN5$cluster + 1L);
plot(cbind(den_all_95$x, den_all_95$y), col = DBSCAN_ALL$cluster + 1L);



        


