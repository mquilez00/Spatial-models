########################################################
##MODELLING: Provincias
#INLA
########################################################
library(ggpubr)
library(dplyr)
library(rpart.plot)
library(sf)
library(rpart)
library(mlr)
library(Metrics)
library(ParamHelpers)
library(caTools)
library(surveillance)
library(randomForest)
library(caret)
library(class)
library(missForest)
library(leaflet)
library(tidyr)
library(sp)
library(rgdal)
library(rgdal)
library(mapSpain)
library(mice)
library(MASS)
library(rattle)
library(RColorBrewer)
#1. DATA AND MAP
data <- read.csv("C:/Users/maria/OneDrive/Escritorio/PROVINCIA/provFINAL18.csv")
p <- sum(data$mayores65)/sum(data$total)
data$esperanza <- p*data$total
colnames(data)[1]<-"prov"
provs <- esp_get_prov()
colnames(provs)[6]<-"prov"
provs$centroid <- st_centroid(provs$geometry)

##1.1. geo: id + long lat

geo <- as.data.frame(1:52)
provs1 <- separate(provs, centroid, into = c("longitude", "latitude"), sep = ",")
geo$longitude <- as.numeric(gsub("[^0-9.,-]", "", provs1$longitude))
geo$latitude <- as.numeric(gsub("[^0-9.,-]", "", provs1$latitude))

##1.2. data: id + info
data$id<-1:52

##1.3. longevidad: id + prop
longevidad <- as.data.frame(1:52)
longevidad$prov <- data$prov
longevidad$prop <- data$mayores65/data$total

##1.4. spatial.polygon
spatial.polygon <- readOGR(dsn = "C:/Users/maria/OneDrive/Escritorio/PROVINCIA/MODELLING/shapefiles_provincias_espana/shapefiles_provincias_espana.shp")
map<-spatial.polygon
provLC<-list(geo,data,longevidad,spatial.polygon)

plot(spatial.polygon)

#2. Data preparation

d<-as.data.frame(data$prov)
colnames(d)[1]<-"prov"
d$mayores65<-data$mayores65
d$total<-data$total
d$E<-data$esperanza
d$As<-data$As
d$BaP<-data$BaP
d$Cd<-data$Cd
d$Ni<-data$Ni
d$gini<-data$gini
d$Pb<-data$Pb
d$PM25<-data$PM25
d$PM10<-data$PM10
d$p_min<-data$p_min
d$p_max<-data$p_max
d$ta_max<-data$ta_max
d$ta_min<-data$ta_min
d$w_racha<-data$w_racha
d$w_med<-data$w_med
d$n_nub<-data$n_nub
d$p_sol<-data$p_sol
d$n_fog<-data$n_fog
d$n_gra<-data$n_gra
d$inso<-data$inso
d$media_neta_hogar<-data$media_neta_hogar
d$media_neta_persona<-data$media_neta_persona
d$pensiones<-data$pensiones
d$hogar_size<-data$hogar_size
d$gini<-data$gini
d$Y<-data$mayores65
d$p<-data$mayores65/data$total
d$SMR<-data$mayores65/data$esperanza
d$ta_max<-data$ta_max
d$media_neta_persona<-data$media_neta_persona
d$ID<-1:52
d$ID<-ifelse(d$prov=="Alava","0",d$ID)
d$ID<-ifelse(d$prov=="Albacete","1",d$ID)
d$ID<-ifelse(d$prov=="Alicante","2",d$ID)
d$ID<-ifelse(d$prov=="Almeria","3",d$ID)
d$ID<-ifelse(d$prov=="Asturias","31",d$ID)
d$ID<-ifelse(d$prov=="Avila","4",d$ID)
d$ID<-ifelse(d$prov=="Badajoz","5",d$ID)
d$ID<-ifelse(d$prov=="Baleares","6",d$ID)
d$ID<-ifelse(d$prov=="Barcelona","7",d$ID)
d$ID<-ifelse(d$prov=="Bizkaia","45",d$ID)
d$ID<-ifelse(d$prov=="Burgos","8",d$ID)
d$ID<-ifelse(d$prov=="Caceres","9",d$ID)
d$ID<-ifelse(d$prov=="Cadiz","10",d$ID)
d$ID<-ifelse(d$prov=="Cantabria","37",d$ID)
d$ID<-ifelse(d$prov=="Castellon","11",d$ID)
d$ID<-ifelse(d$prov=="Ceuta","50",d$ID)
d$ID<-ifelse(d$prov=="Ciudad-real","12",d$ID)
d$ID<-ifelse(d$prov=="Cordoba","13",d$ID)
d$ID<-ifelse(d$prov=="Coruna","14",d$ID)
d$ID<-ifelse(d$prov=="Cuenca","15",d$ID)
d$ID<-ifelse(d$prov=="Gipuzkoa","49",d$ID)
d$ID<-ifelse(d$prov=="Girona","16",d$ID)
d$ID<-ifelse(d$prov=="Granada","17",d$ID)
d$ID<-ifelse(d$prov=="Guadalajara","18",d$ID)
d$ID<-ifelse(d$prov=="Huelva","19",d$ID)
d$ID<-ifelse(d$prov=="Huesca","20",d$ID)
d$ID<-ifelse(d$prov=="Jaen","21",d$ID)
d$ID<-ifelse(d$prov=="Las-palmas","33",d$ID)
d$ID<-ifelse(d$prov=="Leon","22",d$ID)
d$ID<-ifelse(d$prov=="Lleida","23",d$ID)
d$ID<-ifelse(d$prov=="Lugo","25",d$ID)
d$ID<-ifelse(d$prov=="Madrid","26",d$ID)
d$ID<-ifelse(d$prov=="Malaga","27",d$ID)
d$ID<-ifelse(d$prov=="Melilla","51",d$ID)
d$ID<-ifelse(d$prov=="Murcia","28",d$ID)
d$ID<-ifelse(d$prov=="Navarra","29",d$ID)
d$ID<-ifelse(d$prov=="Ourense","30",d$ID)
d$ID<-ifelse(d$prov=="Palencia","32",d$ID)
d$ID<-ifelse(d$prov=="Pontevedra","34",d$ID)
d$ID<-ifelse(d$prov=="Rioja","24",d$ID)
d$ID<-ifelse(d$prov=="Salamanca","35",d$ID)
d$ID<-ifelse(d$prov=="Segovia","38",d$ID)
d$ID<-ifelse(d$prov=="Sevilla","39",d$ID)
d$ID<-ifelse(d$prov=="Soria","40",d$ID)
d$ID<-ifelse(d$prov=="Tarragona","48",d$ID)
d$ID<-ifelse(d$prov=="Tenerife","36",d$ID)
d$ID<-ifelse(d$prov=="Teruel","41",d$ID)
d$ID<-ifelse(d$prov=="Toledo","42",d$ID)
d$ID<-ifelse(d$prov=="Valencia","43",d$ID)
d$ID<-ifelse(d$prov=="Valladolid","44",d$ID)
d$ID<-ifelse(d$prov=="Zamora","46",d$ID)
d$ID<-ifelse(d$prov=="Zaragoza","47",d$ID)
d <- d[order(d$ID), ]

# Impute missing values using mice
d2 <- d[,-1]
d2 <- d2[,-30]
d2 <- d2[,-1]
d2 <- d2[,-1]
amelia_fit <- missForest(d2)
d2<-amelia_fit$ximp
d2<-as.data.frame(scale(d2))
d2$prov<-d$prov
d2$ID<-d$ID
d2$E<-d$E
d2$Y<-d$Y
d2$mayores65<-d$mayores65
d2$total<-d$total
d<-d2
##2.1. Add data to map
rownames(d) <- as.character(d$ID)
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)


#3. Mapping SMR
l <- leaflet(map) %>% addTiles()
pal <- colorNumeric(palette = "YlOrRd", domain = map$SMR)
labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>Proportion: %s <br/>SMR: %s",
                  map$prov, map$Y,  round(map$E, 2), map$p, round(map$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

#4. Modelling
library(INLA)
library(spdep)
library(bamlss)
##4.1. Neighbourhood matrix
map_sp <- SpatialPolygons(map@polygons)
nb <- poly2nb(map_sp)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

#4.1. Inference using INLA
map$re_u <- 1:nrow(map@data)
map$re_v <- 1:nrow(map@data)
map@data$Y<-as.integer(map@data$Y)
#Inference using INLA
formula <- Y ~ As + Ni + media_neta_hogar + Pb + pensiones + PM10 +
  p_min + w_racha + n_fog + gini + n_gra+ PM25 + BaP + Cd + ta_max + ta_min + p_sol + inso +
  hogar_size + media_neta_persona + p_max + w_med + n_nub +
  f(re_u, model = "besag", graph = g, scale.model = TRUE)

res <- inla(formula, family = "poisson", data = map@data, 
            E = E, 
            control.predictor = list(compute = TRUE),
            control.compute=list(dic=TRUE, waic=TRUE))

summary(res)
c(WAIC=res$waic$waic, DIC=res$dic$dic)
#Model Selection
listcombo <- unlist(sapply(0:5,function(x) combn(5, x, simplify=FALSE)),recursive=FALSE)
predterms <- lapply(listcombo, function(x) paste(c("media_neta_persona",c("p_max","n_gra","ta_max","p_min","gini")[x]),collapse="+"))
coefm <- matrix(NA,32,3)
for(i in 1:32){
  formula1 <- as.formula(paste("Y ~ ",predterms[[i]],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  formula2 <- as.formula(paste("Y ~ lat + lon +",predterms[[i]]))
  mod2 <- glm(formula2, offset=(log(E)), family = "poisson", data = dnuevo)
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
  coefm[i,3] <- AIC(mod2)
}

n <- nrow(map@data)
mse <- numeric(n)

for (i in 1:n) {
  data_loo <- map@data[-i, ]
  model_loo <- inla(formula, data = data_loo, family = "poisson", E = E, control.compute = list(config = TRUE))
  
  pred<-model_loo$summary.fitted.values$mean
  mse[i] <- mean((map@data$Y[-i]/map@data$E[-i] - pred)^2)
}
mse_inla<-mean(mse)
rownames(coefm) <- predterms
colnames(coefm) <- c("DIC","WAIC","AIC")
A<-as.data.frame(round(coefm,6))


map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]

coef<-cbind(c("Intercept","As","Ni","media_neta_hogar","Pb",
              "pensiones","PM10","p_min",
              "w_racha","n_fog","gini",
              "n_gra","PM25","BaP","Cd",
              "ta_max","ta_min","p_sol","inso",
              "hogar_size","media_neta_persona","p_max",
              "w_med","n_nub"),
            c(0.036,0.025,0.075,0.197,-0.020,0.130,-0.064,-0.075,0.060,-0.005,
              -0.074,-0.028,0.024,0.062,0.030,0.018,-0.013,-0.401,0.41,-0.088,-0.121,
              0.040,-0.072,0.014))
coef<-as.data.frame(coef)
colnames(coef)<-c("variable","coefficient")
coef$coefficient <- as.numeric(coef$coefficient)

barplot(coef$coefficient, names.arg = coef$variable, ylab = "", col = "indianred1",
        las = 2, cex.names = 0.45, horiz = FALSE, ylim = c(-0.45, 0.45))

summary(map@data[, c("RR", "LL", "UL")])
mapsf <- st_as_sf(map)
mapsf$SIR<-mapsf$Y/mapsf$E
gRR <- ggplot(mapsf) + geom_sf(aes(fill = UL)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(min(mapsf$SIR)-0.05, max(mapsf$SIR)+1.5)
  ) +
  theme_bw()
gLL <- ggplot(mapsf) + geom_sf(aes(fill = LL)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.7, 1.5)
  ) +
  theme_bw()
gUL <- ggplot(mapsf) + geom_sf(aes(fill = UL)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.7, 1.5)
  ) +
  theme_bw()
library(cowplot)
plot_grid(gRR, gLL, gUL, ncol = 1)
map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]

########################################################
##MODELING: Provincias
#regresión lineal
########################################################

dnuevo<-data
dnuevo$lat<-geo$latitude
dnuevo$lon<-geo$longitude
dnuevo$id<-NULL
dnuevo$barrio<-NULL
dnuevo$p<-NULL
dnuevo$edad_media<-NULL
dnuevo$indice1<-NULL
dnuevo$indice2<-NULL
dnuevo$prop2<-NULL
dnuevo$prop3<-NULL
dnuevo$esperanza<-NULL
#dnuevo<-as.data.frame(scale(dnuevo))
dnuevo$Y<-d$Y
dnuevo$E<-d$E
amelia_fit <- missForest(dnuevo)
dnuevo<-amelia_fit$ximp

mod2 <- glm(Y ~ . - E, offset=(log(E)), family = "poisson", data = dnuevo)
summary(mod2)

#variable selection

library(MASS)
modAIC <- stepAIC(mod2, trace=TRUE, direction="forward")
summary(modAIC)
modBIC <- MASS::stepAIC(mod2, k = log(nrow(dnuevo)))

car::compareCoefs(modBIC, modAIC)

# Confidence intervals
confint(modBIC)

########################################################
##MODELING: Provincias
#GAM
########################################################

mod3 <- gam(Y ~ s(Pb) + Pb + pensiones + s(media_neta_persona), family = "poison", data = d)
summary(mod3)
plot(ggeffects::ggpredict(mod3), facets = TRUE)
gratia::draw(mod3)

dnuevo$E<-data$esperanza
dnuevo$Y<-data$mayores65
dnuevo$lat<-geo$latitude
dnuevo$lon<-geo$longitude
n <- nrow(dnuevo)
mse <- numeric(n)

for (i in 1:n) {
  data_loo <- dnuevo[-i, ]
  model_loo<-gam(Y ~ BaP+
                   gini+
                   Cd+
                   Ni+
                   PM25+PM10+
                   p_min+p_max+
                   ta_min+ta_max+ti(p_sol)+
                   ti(media_neta_hogar)+n_fog+
                   w_med+ti(inso)+pensiones+ ti(lat)+ti(lon)+ti(lat,lon), 
                 offset = log(E), family = "poisson", data = data_loo)
  pred <- predict(object=model_loo, newdata = data_loo)
  mse[i] <- mean((log(dnuevo[-i,]$p) - pred)^2)
}
mse_gam<-mean(mse)


########################################################
##MODELING: Provincias
#MACHINE LEARNING
########################################################
dnuevo$p<-dnuevo$Y/dnuevo$E
dnuevo$Y<-NULL
dnuevo$E<-NULL
dnuevo$habitantes<-NULL
dnuevo$hogares_monoparentales_hombre<-NULL

#### 1) Decison tree
m1 <- rpart(
  formula = p ~ .,
  data    = dnuevo,
  method  = "anova",
  minsplit = 4,
  maxdepth = 4
)

fancyRpartPlot(m1, caption = NULL, palettes=c("Oranges"), type = 1, cex=0.45)

m1$variable.importance
var_names <- names(m1$variable.importance)

barplot(m1$variable.importance, 
        main = "Variable Importances",
        ylab = "Importance", 
        names.arg = var_names,
        las = 2,
        ylim = c(0,15000),
        col = "plum3")

n <- nrow(dnuevo)
mse <- numeric(n)

for (i in 1:n) {
  data_loo <- dnuevo[-i, ]
  model <- randomForest(p ~ .,data = data_loo)
  pred <- predict(model, newdata = data_loo)
  mse[i] <- mean((dnuevo[-i,]$p - pred)^2)
}
mse_rf<-mean(mse)
