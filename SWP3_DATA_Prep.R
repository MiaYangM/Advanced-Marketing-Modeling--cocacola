library(plyr)
library(utils)
library(lmtest)
library(orcutt)
library(car)
library(AER)
library(sandwich)
library(lmtest)
library(dplyr)


# 1. MAIN DATA
data<- read.csv("cola.coke.pepsi.csv")
head(data, 3)

# 2. STORE DATA (for market definition)
stores <- read.csv("stores_eauclaire_pittsfield.csv")
stores<-rename(stores, replace =c("stores"="IRI_KEY"))

delivery.stores <- read.csv("Delivery_Stores.csv")
stores<-merge(stores,delivery.stores,by="IRI_KEY")
stores<-subset(stores,stores$OU=="GR")
stores<-stores[,c(1,3,4)] #c("IRI_KEY", "MARKET", "OU")

head(stores, 3)

# grocery data
groc<-read.table("carbbev_groc_1270_1321",header=TRUE)
groc_04 <- merge(groc,stores, by="IRI_KEY")
groc<-read.table("carbbev_groc_1322_1373",header=TRUE)
groc_05 <- merge(groc,stores, by="IRI_KEY")
groc<-read.table("carbbev_groc_1374_1426",header=TRUE)
groc_06 <- merge(groc,stores, by="IRI_KEY")
groc_04$YEAR  <-2004
groc_05$YEAR  <-2005
groc_06$YEAR  <-2006
groc <- rbind(groc_04,groc_05,groc_06)
head(groc)
prod_carbbev <- read.csv("prod_carbbev.csv")
head(prod_carbbev)
groc <- merge (groc, prod_carbbev, by=(c("SY","GE","VEND","ITEM")))
groc$liter <- groc$UNITS*groc$VOL_EQ*2/0.3521
head(groc,10)
nrow(groc)
tmp<-aggregate(DOLLARS ~FLAVOR.SCENT, sum, data=groc)
tmp
groc <- subset(groc,groc$FLAVOR.SCENT=="COLA") #### select only Cola flavor
#### Compute liter and revenue for COLA for each store and week
total.revenue.liter <- aggregate(cbind(DOLLARS,liter)~MARKET+IRI_KEY+WEEK,sum, data=groc)
names(total.revenue.liter)[names(total.revenue.liter) == "DOLLARS"] <- "total.rev"
names(total.revenue.liter)[names(total.revenue.liter) == "liter"] <- "total.liter"
head(total.revenue.liter)

# 3. TRIPS DATA (for market size/outside good)
##### compute number of panelist visiting a store
trips.2004<-read.csv("trips_2004.csv")
trips.2005<-read.csv("trips_2005.csv")
trips.2006<-read.csv("trips_2006.csv")

trips <- rbind(trips.2004,trips.2005,trips.2006)

stores <- read.csv("stores_eauclaire_pittsfield.csv")
stores<-rename(stores, replace =c("stores"="IRI_KEY"))
delivery.stores <- read.csv("Delivery_Stores.csv")
stores<-merge(stores,delivery.stores,by="IRI_KEY")
stores<-subset(stores,stores$OU=="GR")
stores<-stores[,c(1,3,4)]
trips <- merge(trips,stores, by="IRI_KEY")



# 4. TOTAL TRIPS PER STORE-WEEK (market size proxy)
total.trips <- aggregate(PANID~WEEK+IRI_KEY,FUN=length, data=trips)
tmp <- aggregate(PANID~IRI_KEY,mean, data=total.trips)
head(total.trips)
head(tmp)


# 5. PANEL DATA (cola buyers only - for within-market trips)
##### compute number of panelist buying carbonated soft drinks
panel.04<-read.table("carbbev_PANEL_GR_1270_1321.dat",header=TRUE)
panel.05<-read.table("carbbev_PANEL_GR_1322_1373.dat",header=TRUE)
panel.06<-read.table("carbbev_PANEL_GR_1374_1426.dat",header=TRUE)
#head(panel.04)
panel<-rbind(panel.04,panel.05,panel.06)
prod <- read.csv("prod_carbbev.csv")
head(prod)
head(panel)
prod$COLUPC <- prod$SY*100000000000+prod$GE*10000000000+prod$VEND*100000+prod$ITEM
summary(prod$ITEM)

panel <- merge(panel, prod, by="COLUPC")
panel <- subset(panel, panel$FLAVOR.SCENT=="COLA")  # Keep only cola buyers
within.trips <- aggregate(PANID ~ WEEK + IRI_KEY, FUN=length, data=panel)
names(within.trips)[names(within.trips) == "PANID"] <- "within.trips"

# 5. PANEL DATA (cola buyers only - for within-market trips)
##### compute number of panelist buying carbonated soft drinks

panel.04<-read.table("carbbev_PANEL_GR_1270_1321.dat",header=TRUE)
panel.05<-read.table("carbbev_PANEL_GR_1322_1373.dat",header=TRUE)
panel.06<-read.table("carbbev_PANEL_GR_1374_1426.dat",header=TRUE)
head(panel.04)

panel<-rbind(panel.04,panel.05,panel.06)
prod <- read.csv("prod_carbbev.csv")
head(prod)
head(panel)
prod$COLUPC <- prod$SY*100000000000+prod$GE*10000000000+prod$VEND*100000+prod$ITEM
summary(prod$ITEM)

panel <- merge(panel, prod, by="COLUPC")
tmp<-aggregate(DOLLARS ~FLAVOR.SCENT, sum, data=panel)
panel
panel <- subset(panel, panel$FLAVOR.SCENT=="COLA")  # Keep only cola buyers
within.trips <- aggregate(PANID~WEEK+IRI_KEY,FUN=length, data=panel)
tmp <- aggregate(PANID~IRI_KEY,mean, data=within.trips)
names(within.trips)[names(within.trips) == "PANID"] <- "within.trips"

within.trips <- merge(within.trips,tmp,by="IRI_KEY")
within.trips$test <- within.trips$within.trips-(within.trips$PANID/2)
within.trips$tmp <- within.trips$within.trips
within.trips$within.trips <- ifelse(within.trips$test>0,within.trips$tmp,within.trips$PANID)
head(within.trips)
summary(within.trips)
within.trips<-within.trips[,-c(4:6)]
head(within.trips)
summary(within.trips)

# 6. INSTRUMENTS: Weather, Sugar, Aluminum, Gasoline
weather <- read.csv("weather_pittsfield_eauclaire.csv")
weather$Date <- as.Date(weather$date, format="%m/%d/%Y")
weather$W <- as.Date(cut(weather$Date, "week"))
weather<-aggregate(cbind(Temp.Max,Temp.Avg,Temp.Min,
                         DP.Max,DP.Mean,DP.Min,
                         Humidity.Max,Humidity.Mean,Humidity.Min,
                         Wind.Max,Wind.Mean,Wind.Min,
                         Pressure.Max,Pressure.Mean,Pressure.Min) 
                   ~ MARKET+W, data=weather, mean, na.rm = TRUE)
W <- unique(weather$W)
W <- as.data.frame(W)
W$WEEK <- 1270:(1269+nrow(W))
weather <- merge(weather, W, by="W")
weather <- weather[,-1]
head(weather)
dat<-merge(dat,weather, by=c("MARKET","WEEK"))


gas <- read.csv("gasoline.csv")
gas$Date <- as.Date(gas$date, format="%m/%d/%Y")
gas$W <- as.Date(cut(gas$Date, "week"))
gas <- aggregate(cbind(gas.deep, gas.high) ~ W, data=gas, mean)
W <- unique(gas$W)
W <- as.data.frame(W)
W$WEEK <- 1270:(1269+nrow(W))
gas <- merge(gas, W, by="W")
gas <- gas[,-1]

############# Instruments SUGAR
data.sugar<-read.csv("week_sugar.csv")
head(data.sugar)
plot(data.sugar$WEEK,data.sugar$sugar.price)
dat <- merge(dat,data.sugar,by=c("WEEK")) 

data.alumnium<-read.csv("alumnium_price.csv")
head(data.alumnium)
plot(data.alumnium$WEEK,data.alumnium$alumnium.price)
dat <- merge(dat,data.alumnium,by=c("WEEK")) 
head(dat)
cor(dat$alumnium.price,dat$sugar.price)





