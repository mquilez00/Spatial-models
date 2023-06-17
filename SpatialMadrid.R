########################################################
##MODELLING: Madrid
#INLA
########################################################
#1. DATA AND MAP
library(tidyr)
library(rgdal)
library(missForest)
library(sf)
data <- read.csv("C:/Users/maria/OneDrive/Escritorio/madrid22.csv")
p <- sum(data$pob_mas65)/sum(data$habitantes)
data$esperanza = p*data$habitantes

##1.1. geo: id + long lat
madrid <- st_read("C:/Users/maria/OneDrive/Escritorio/MADRID/mapa/Barrios/Barrios.shp")
colnames(madrid)[3]<-"distrito"
colnames(madrid)[5]<-"barrio"
geo<-as.data.frame(1:131)
madrid$centroid <- st_centroid(madrid$geometry)
mad1 <- separate(madrid, centroid, into = c("longitude", "latitude"), sep = ",")
geo$longitude <- as.numeric(gsub("[^0-9.,-]", "", mad1$longitude))
geo$latitude <- as.numeric(gsub("[^0-9.,-]", "", mad1$latitude))

##1.2. data: id + info
data$id<-1:131

##1.3. longevidad: id + prop
longevidad <- as.data.frame(1:131)
longevidad$barrio <- data$barrio
longevidad$prop <- data$pob_mas65/data$habitantes

##1.4. spatial.polygon
madrid <- readOGR("C:/Users/maria/OneDrive/Escritorio/MADRID/mapa/Barrios/Barrios.shp")
map<-madrid
madLC<-list(geo,data,longevidad,map)

plot(map)

#2. Data preparation
d<-as.data.frame(0:130)
colnames(d)[1]<-"ID"
d$barrio<-data$barrio
d$p<-data$pob_mas65/data$habitantes
r<-sum(data$pob_mas65, rm.na=TRUE)/sum(data$habitantes, rm.na=TRUE)
d$E<-r*data$habitantes
d$Y<-data$pob_mas65
d$SIR<-d$Y/d$E
d$tasa_natalidad <- data$tasa_natalidad
d$tama_hogar <- data$tama_hogar
d$sup_vivienda <- data$sup_vivienda
d$renta_neta_hogar <- data$renta_neta_hogar 
d$prop_joven <- data$prop_joven 
d$primaria_incompleta <- data$primaria_incompleta 
d$pob_masculina <- data$pob_masculina 
d$pob_etapa_educativa <- data$pob_etapa_educativa
d$pob_estudios_super <- data$pob_estudios_super
d$pob_espa <- data$pob_espa
d$pob_bachiller_bup <- data$pob_bachiller_bup
d$pension_mujeres <- data$pension_mujeres 
d$pension_hombres <- data$pension_hombres
d$num_hogares <- data$num_hogares
d$pb_mujeres <- data$pb_mujeres
d$mas25_sin_est <- data$mas25_sin_est
d$inmuebles_residencial <- data$inmuebles_residencial
d$indice_dependencia <- data$indice_dependencia 
d$hogares_monoparentales_mujer <- data$hogares_monoparentales_mujer 
d$hogares_monoparentales_hombre <- data$hogares_monoparentales_hombre 
d$extranjeros  <- data$extranjeros  
d$desempleo_mujeres_45<- data$desempleo_mujeres_45 
d$desempleo_mujeres_25<- data$desempleo_mujeres_25
d$densidad<- data$densidad 
d$desempleo_hombres_45<- data$desempleo_hombres_45 
d$desempleo_hombres_25<- data$desempleo_hombres_25 
d$col_publicos_inf_prim <- data$col_publicos_inf_prim  
d$ano_medio_inmuebles <- data$ano_medio_inmuebles  
# Impute missing values using mice
d2 <- d[,-c(1:6)]
amelia_fit <- missForest(d2)
d2<-amelia_fit$ximp
d2<-as.data.frame(scale(d2))
d2$barrio<-d$barrio
d2$ID<-d$ID
d2$E<-d$E
d2$Y<-d$Y
d2$SIR<-d2$Y/d2$E
d2$p<-d$p
d<-d2

##2.1. Add data to map
rownames(d) <- as.character(d$ID)
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)

#3. Mapping SMR
map_sf <- st_as_sf(map)
library(ggplot2)
ggplot(map_sf) + geom_sf(aes(fill = SIR),size = 0.05) +
  ggtitle("") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(min(map_sf$SIR)-0.5,max(map_sf$SIR)+0.5)
  )
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
#Inference using INLA
formula <- Y ~ densidad + tasa_natalidad + tama_hogar + sup_vivienda + renta_neta_hogar +
  primaria_incompleta + pob_masculina + pob_estudios_super + pob_bachiller_bup + pension_mujeres + pension_hombres + num_hogares +
  mas25_sin_est + inmuebles_residencial + indice_dependencia + hogares_monoparentales_hombre+
  hogares_monoparentales_mujer + extranjeros + desempleo_hombres_25 + desempleo_hombres_45 +
  desempleo_mujeres_25 + desempleo_mujeres_45 + col_publicos_inf_prim + ano_medio_inmuebles +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) + f(re_v, model = "iid")


res <- inla(formula, family = "poisson", 
            data = map@data, E = E, 
            control.predictor = list(compute = TRUE), 
            control.compute=list(dic=TRUE, waic=TRUE))
summary(res)

coef<-cbind(c("Intercept","densidad","tasa_natalidad","tama_hogar","sup_vivienda",
              "renta_neta_hogar","primaria_incompleta","pob_masculina",
              "pob_estudios_super","pob_bachiller_bup","pension_mujeres",
              "pension_hombres","num_hogares","mas25_sin_est","inmuebles_residencial",
              "indice_dependencia","hogares_monoparentales_hombre",
              "hogares_monoparentales_mujer","extranjeros","desempleo_hombres_25",
              "desempleo_hombres_45","desempleo_mujeres_25","desempleo_mujeres_45",
              "col_publicos_inf_prim","ano_medio_inmuebles"),
            c(-0.014,-0.002,0.064,-0.313,0.156,0.133,0.008,-0.047,-0.231,-0.112,
              -0.078,-0.116,0.109,-0.345,0.170,0.170,-0.236,0.260,-0.130,0.090,
              0.336,-0.127,-0.429,0.323,0.002))
coef<-as.data.frame(coef)
colnames(coef)<-c("variable","coefficient")
coef$coefficient <- as.numeric(coef$coefficient)

barplot(coef$coefficient, names.arg = coef$variable, ylab = "", col = "indianred1",
        las = 2, cex.names = 0.35, horiz = FALSE, ylim = c(-0.5, 0.5))
#Model Selection
vars <- colnames(map@data)
coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ ",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))


coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia + pension_hombres +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia + pension_hombres +
  tasa_natalidad +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia + pension_hombres +
  tasa_natalidad + inmuebles_residencial +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia + pension_hombres +
  tasa_natalidad + inmuebles_residencial + sup_vivienda +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia + pension_hombres +
  tasa_natalidad + inmuebles_residencial + sup_vivienda + pension_mujeres +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia + pension_hombres +
  tasa_natalidad + inmuebles_residencial + sup_vivienda + pension_mujeres +
  num_hogares +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

coefm <- matrix(NA,28,2)
for(i in 1:28){
  formula1 <- as.formula(paste("Y ~ pob_estudios_super + indice_dependencia + pension_hombres +
  tasa_natalidad + inmuebles_residencial + sup_vivienda + pension_mujeres +
  num_hogares + hogares_monoparentales_hombre +",vars[i],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- vars[1:28]
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))

#Model Selection
listcombo <- unlist(sapply(0:8,function(x) combn(8, x, simplify=FALSE)),recursive=FALSE)
predterms <- lapply(listcombo, function(x) paste(c("pob_estudios_super",c("indice_dependencia","pension_hombres","tasa_natalidad",
                                                                          "inmuebles_residencial","sup_vivienda",
                                                                          "pension_mujeres","num_hogares",
                                                                          "hogares_monoparentales_hombre")[x]),collapse="+"))
coefm <- matrix(NA,256,2)
for(i in 1:256){
  formula1 <- as.formula(paste("Y ~ ",predterms[[i]],"+
  f(re_u, model = \"besag\", graph = g, scale.model = TRUE) + f(re_v, model = \"iid\")"))
  result <- inla(formula1, family="poisson", 
                 data=map@data, E = E, 
                 control.predictor = list(compute = TRUE),
                 control.compute=list(dic=TRUE, waic=TRUE))
  coefm[i,1] <- result$dic$dic
  coefm[i,2] <- result$waic$waic
}

rownames(coefm) <- predterms
colnames(coefm) <- c("DIC","WAIC")
A<-as.data.frame(round(coefm,6))


##leave-one-out
formula_final<-Y ~ pob_estudios_super+indice_dependencia+pension_hombres+
  tasa_natalidad+inmuebles_residencial+sup_vivienda+pension_mujeres+
  num_hogares+hogares_monoparentales_hombre +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) + f(re_v, model = "iid")


# Define your model using the inla() function

model <- inla(formula_final, family = "poisson", 
              data = map@data, E = E, 
              control.predictor = list(compute = TRUE), 
              control.compute=list(dic=TRUE, waic=TRUE))
summary(model)
# Perform leave-one-out cross-validation (LOOCV)
n <- nrow(map@data)
mse <- numeric(n)

for (i in 1:n) {
  data_loo <- map@data[-i, ]
  model_loo <- inla(formula, data = data_loo, family = "poisson", E = E, control.compute = list(config = TRUE))
  
  pred<-model_loo$summary.fitted.values$mean
  mse[i] <- mean((map@data$Y[-i]/map@data$E[-i] - pred)^2)
}
mse_inla<-mean(mse)
map_sf$RR <- res$summary.fitted.values[, "mean"]
map_sf$LL <- res$summary.fitted.values[, "0.025quant"]
map_sf$UL <- res$summary.fitted.values[, "0.975quant"]

ggplot(map_sf) + geom_sf(aes(fill = UL),size = 0.05) +
  ggtitle("") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(min(map_sf$SIR)-0.5,max(map_sf$SIR)+0.5)
  )

########################################################
##MODELING: Madrid
#regresión lineal
########################################################

dnuevo<-data
dnuevo$lat<-geo$latitude
dnuevo$lon<-geo$longitude
dnuevo$prov<-NULL
dnuevo$SMR<-NULL
dnuevo$p<-NULL
dnuevo$prop<-NULL
dnuevo$prop2<-NULL
dnuevo$prop3<-NULL
dnuevo$sobre<-NULL
dnuevo$sobre2<-NULL
dnuevo$sobre3<-NULL
dnuevo$rate<-NULL
dnuevo$esperanza<-NULL
dnuevo$id<-NULL
dnuevo$n_llu<-NULL
dnuevo$glo<-NULL
dnuevo$n_tor<-NULL
dnuevo$evap<-NULL
dnuevo$n_nie<-NULL
dnuevo$super<-NULL
dnuevo$super2<-NULL
dnuevo$super3<-NULL
dnuevo$total<-NULL
dnuevo$mayores65<-NULL
dnuevo$t_100<-NULL
dnuevo$barrio<-as.factor(dnuevo$barrio)
dnuevo$Y<-d$Y
dnuevo$E<-d$E
amelia_fit <- missForest(dnuevo)
dnuevo<-amelia_fit$ximp
dnuevo$p<-dnuevo$Y/dnuevo$E

#### RANDOM FOREST
dnuevo$E<-NULL
dnuevo$Y<-NULL
dnuevo$barrio<-NULL
# Load required libraries
library(randomForest)
library(caret)

model <- randomForest(p ~ ., data = dnuevo)
predictions <- predict(model, newdata = dnuevo)
mse <- mean((predictions - dnuevo$p) ^ 2)

# Tune the Random Forest model
tuned_model <- tuneRF(x = dnuevo[, -42], y = dnuevo$p,
                      ntreeTry = 500,
                      stepFactor = 1.5,
                      improve = 0.01,
                      trace = TRUE,
                      plot = TRUE)

# Get the best model from tuning
best_model <- randomForest(p ~ ., mtry = 19, data = dnuevo)

# Make predictions on the test set using the best model
predictions <- predict(best_model, newdata = dnuevo)

# Calculate the mean squared error (MSE)
mse <- mean((predictions - dnuevo$p) ^ 2)
mse
n <- nrow(dnuevo)
mse <- numeric(n)

for (i in 1:n) {
  data_loo <- dnuevo[-i, ]
  model <- randomForest(p ~ ., mtry = 13,data = data_loo)
  pred <- predict(model, newdata = data_loo)
  mse[i] <- mean((dnuevo[-i,]$p - pred)^2)
}
mse_rf<-mean(mse)


########################################################
##MODELING: Madrid
#GAM
########################################################
dnuevo$Y<-data$pob_mas65
dnuevo$E<-data$esperanza
dnuevo$p<-dnuevo$Y/dnuevo$E
dnuevo$lat<-geo$latitude
dnuevo$lon<-geo$longitude
mod11 <- gam(Y ~ s(num_hogares)+
               s(pob_estudios_super)+
               s(indice_dependencia)+
               s(tasa_natalidad)+
               s(sup_vivienda)+
               s(pension_hombres)+
               s(pension_mujeres)+
               s(inmuebles_residencial)+
               s(hogares_monoparentales_hombre)+ti(lat)+ti(lon)+ti(lat,lon), 
             offset = log(E), family = "poisson", data = dnuevo)
summary(mod11)

# Perform leave-one-out cross-validation (LOOCV): Decision tree
n <- nrow(dnuevo)
mse <- numeric(n)

for (i in 1:n) {
  data_loo <- dnuevo[-i, ]
  model_loo <- gam(Y ~ s(num_hogares)+
                     s(pob_estudios_super)+
                     s(indice_dependencia)+
                     s(tasa_natalidad)+
                     s(sup_vivienda)+
                     s(pension_hombres)+
                     s(pension_mujeres)+
                     s(inmuebles_residencial)+
                     s(hogares_monoparentales_hombre)+ ti(lat)+ti(lon)+ti(lat,lon), 
                   offset = log(E), family = "poisson", data = data_loo)
  pred <- predict(object=model_loo, newdata = data_loo)
  mse[i] <- mean((log(dnuevo$p[-i]) - pred)^2)
}
mse_gam<-mean(mse)

