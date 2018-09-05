# PART 4: K-Means y busqueda de la mejor K

####################################
# Carga y limpieza datos
####################################
datosSeasons <- read.csv( file = "nba-players-stats-since-1950/Seasons_Stats.csv", header = T )
# Algunas instancias están vacias => borrar
datosSeasons <- datosSeasons[!is.na( x = datosSeasons$Year ), ]
# delete "blanl" and "blank2" (empty columns)
datosSeasons <- subset( x = datosSeasons, select = -c( blanl , blank2 ) )
# primera variable codigo identificador => elimino
datosSeasons <- subset( x = datosSeasons, select = -c(X) )
# Retocar posiciones para dejar solo 5 posiciones
# PF = BASE, SG = ESCOLTA, SF = ALERO, PF = ALA-PIVOT, C = PIVOT
# retocar redundancias posiciones
datosSeasons[datosSeasons$Pos == "SG-PG", ]$Pos = "PG-SG"
datosSeasons[datosSeasons$Pos == "F-G", ]$Pos   = "G-F"
datosSeasons[datosSeasons$Pos == "SF-PG", ]$Pos = "PG-SF"
datosSeasons[datosSeasons$Pos == "SF-SG", ]$Pos = "SG-SF"
datosSeasons[datosSeasons$Pos == "PF-SF", ]$Pos = "SF-PF"
datosSeasons[datosSeasons$Pos == "C-PF", ]$Pos  = "PF-C"
datosSeasons[datosSeasons$Pos == "C-F", ]$Pos   = "F-C"
# retocar posiciones
datosSeasons[datosSeasons$Pos == "G", ]$Pos     = "PG"
datosSeasons[datosSeasons$Pos == "G-F", ]$Pos   = "SG"
datosSeasons[datosSeasons$Pos == "PG-SF", ]$Pos = "SG"
datosSeasons[datosSeasons$Pos == "PG-SG", ]$Pos = "SG"
datosSeasons[datosSeasons$Pos == "SG-PF", ]$Pos = "SF"
datosSeasons[datosSeasons$Pos == "SG-SF", ]$Pos = "SF"
datosSeasons[datosSeasons$Pos == "F", ]$Pos     = "SF"
datosSeasons[datosSeasons$Pos == "F-C", ]$Pos   = "PF"
datosSeasons[datosSeasons$Pos == "SF-PF", ]$Pos = "PF"
datosSeasons[datosSeasons$Pos == "PF-C", ]$Pos  = "PF"
datosSeasons[datosSeasons$Pos == "C-SF", ]$Pos  = "PF"
# Eliminar levels que ya no sirven
datosSeasons <- droplevels( x = datosSeasons )
# Dataset con alturas y pesos
datos0Players <- read.csv( file = "nba-players-stats-since-1950/Players.csv", header = T )
datos0Players <- datos0Players[, c("Player", "height", "weight")]
# Merge para tener en temporadas la altura y peso de cada jugador
datosSeasons <- merge( x = datosSeasons, y = datos0Players, by = "Player" )

######################################
# Busqueda mejor K (Silhouette)
######################################
library(cluster)
set.seed(2011) # para intentar obtener siempre los mismos resultados, aunque el KMeans saca diferentes resultados cada vez (random a desempatar¿?)
               # los indices Silhouette se modifican muy poco, los cluster obtenidos en el ultimo experimento son los mismos
               # aunque puede variar el orden

# PRE: Datos originales, numero maximo de K, tag, color y estilo de puntos para el grafico
# POST: aplica KMeans y calcula indices Silhouette para valores desde k a pKMax
experimento_kmeans <- function(pDatos, pKMax, pTag, pCol, pPch){
  cat(paste('######################################', pTag,'######################################\n', sep = '\n'))
  datos <- pDatos
  K <- pKMax # número máximo de clases

  datos <- droplevels( x = datos )
  variables_numericas <- Filter( f = is.numeric, x = datos ) # is.numeric reconoce tanto numeric como integer
  variables_numericas[is.na(variables_numericas)] <- 0 # NA = 0
  variables_numericas <- scale(variables_numericas, center = T, scale = T)
  
  # La distancia debe estar en consonancia
  # con la función objetivo de 'kmeans()'
  # distancia euclidiana cuadrática
  distancias <- dist(variables_numericas, method="euclidean", diag=TRUE, upper=TRUE)^2

  avg.width <- NA # avg.width[1] no tiene sentido
  for(k in 2:K)
  {
    km <- kmeans(variables_numericas, k, nstart=100, iter.max=10000)
    silk <- silhouette(km$cluster, distancias)
    avg.width[k] <- summary(silk)$avg.width
    print(summary(silk)$avg.width)
  }
  cbind(k=1:length(avg.width), avg.width=round(avg.width, digits=6))
  
  # Representación gráfica de la variación de 'avg.width' en función de 'k'
  points(avg.width, type="b", las=1, xlab="k", col = pCol, pch = pPch)
  # plot(avg.width, type="b", las=1, xlab="k", main = pTag)
  # axis(1, at=c(1,seq(0,K,5)))
  # abline(v=c(1:4,seq(0,K,5)), col="gray")
}

#que hayan jugado más del 25% de minutos, Tm (Team)=TOT cuando el jugador ha sido traspasado (aparece duplicado)
datos0 <- datosSeasons[datosSeasons$MP > 1000 & datosSeasons$Tm!="TOT",]
KMax <- 15 #K maximo a buscar

##########################################################################################
# Experimento 1: Todas las variables
##########################################################################################
plot(c(1:2), type="n", las=1, main = "Mejor K para todas las variables",
     xlab="k", ylab="Media Silhouette", ylim = c(0.3,0.85), xlim = c(1,KMax))
abline(v=c(1:4,seq(0,KMax,5)), col="gray")
##########################################################################################
# Experimento 1.1: Temporadas >2009
##########################################################################################
datos <- datos0[datos0$Year>2009,]
tag <- ">2009_TodasVariables"
experimento_kmeans(datos, KMax, tag, "blue", 15)
##########################################################################################
# Experimento 1.2: Temporadas >2014
##########################################################################################
datos <- datos0[datos0$Year>2014,]
tag <- ">2014_TodasVariables"
experimento_kmeans(datos, KMax, tag, "green", 18)
##########################################################################################
# Experimento 1.3: Temporadas 2017
##########################################################################################
datos <- datos0[datos0$Year==2017,]
datos$Year <- NULL
tag <- "2017_TodasVariables"
experimento_kmeans(datos, KMax, tag, "red", 19)
legend("topright", bty = "n",
       legend=c("Temporadas >2009","Temporadas >2014", "Temporada   2017"),
       pch = c(15, 18, 19), pt.bg = 'white', col=c("blue","green","red"), lwd = 1.25)

##########################################################################################
# Experimento 2: Variables tradicionales
##########################################################################################
plot(c(1:2), type="n", las=1, main = "Mejor K para todas variables tradicionales",
     xlab="k", ylab="Media Silhouette", ylim = c(0.3,0.85), xlim = c(1,KMax))
abline(v=c(1:4,seq(0,KMax,5)), col="gray")
##########################################################################################
# Experimento 2.1: Temporadas >2009
##########################################################################################
variables_tradicionales <- c("Player","Age", "G", "GS", "MP",
                             "FG", "FGA", "FG.",
                             "FT", "FTA", "FT.",
                             "X3P", "X3PA", "X3P.",
                             "X2P", "X2PA", "X2P.",
                             "ORB", "DRB", "TRB",
                             "AST", "STL", "BLK", "TOV", "PTS",
                             "PF", "height", "weight",
                             "Pos")
datos <- datos0[datos0$Year>2009,variables_tradicionales]
tag <- ">2009_VariablesTradicionales"
experimento_kmeans(datos, KMax, tag, "blue", 15)
##########################################################################################
# Experimento 2.2: Temporadas >2014
##########################################################################################
datos <- datos0[datos0$Year>2014,variables_tradicionales]
tag <- ">2014_VariablesTradicionales"
experimento_kmeans(datos, KMax, tag, "green", 18)
##########################################################################################
# Experimento 2.3: Temporadas 2017 y variables tradicionales
##########################################################################################
datos <- datos0[datos0$Year==2017,variables_tradicionales]
datos$Year <- NULL
tag <- "2017_VariablesTradicionales"
experimento_kmeans(datos, KMax, tag, "red", 19)

legend("topright", bty = "n",
       legend=c("Temporadas >2009","Temporadas >2014", "Temporada   2017"),
       pch = c(15,18,19),pt.bg = 'white', col=c("blue","green","red"), lwd = 1.25)


##########################################################################################
# Experimento 3: PCA
##########################################################################################
plot(c(1:2), type="n", las=1, main = "Mejor K para PCA (Temporada 2017)",
     xlab="k", ylab="Media Silhouette", ylim = c(0.2,0.55), xlim = c(1,KMax))
abline(v=c(1:4,seq(0,KMax,5)), col="gray")
##########################################################################################
# Experimento 3.1: Temporada 2017 y PCA con todas las variables
##########################################################################################
datos <- datos0[datos0$Year==2017,]
datos$Year <- NULL
datos <- datos[complete.cases(datos),]
datos <- droplevels( x = datos )
tag <- "2017_PCA"
variables_numericas <- Filter( f = is.numeric, x = datos ) # is.numeric reconoce tanto numeric como integer
variables_numericas[is.na(variables_numericas)] <- 0 # NA = 0
variables_numericas <- scale(variables_numericas, center = T, scale = T)
mi_pca <- prcomp(variables_numericas, center = T, scale = T)
summary(mi_pca) # 15 compontentes para 95%

distancias <- dist(mi_pca$x[,1:15], method="euclidean", diag=TRUE, upper=TRUE)^2
avg.width <- NA # avg.width[1] no tiene sentido
for(k in 2:KMax)
{
  km <- kmeans(variables_numericas, k, nstart=100, iter.max=10000)
  silk <- silhouette(km$cluster, distancias)
  avg.width[k] <- summary(silk)$avg.width
}
cbind(k=1:length(avg.width), avg.width=round(avg.width, digits=6))

# Representación gráfica de la variación de 'avg.width' en función de 'k'
points(avg.width, type="b", las=1, xlab="k", col = "black", pch = 15)
##########################################################################################
# Experimento 3.2: Temporada 2017 y PCA con variables tradicionales
##########################################################################################
datos <- datos0[datos0$Year==2017, variables_tradicionales]
datos$Year <- NULL
datos <- datos[complete.cases(datos),]
datos <- droplevels( x = datos )
tag <- "2017_PCA"
variables_numericas <- Filter( f = is.numeric, x = datos ) # is.numeric reconoce tanto numeric como integer
variables_numericas[is.na(variables_numericas)] <- 0 # NA = 0
variables_numericas <- scale(variables_numericas, center = T, scale = T)
mi_pca <- prcomp(variables_numericas, center = T, scale = T)
summary(mi_pca) # 12 compontentes para 95%

distancias <- dist(mi_pca$x[,1:12], method="euclidean", diag=TRUE, upper=TRUE)^2
avg.width <- NA # avg.width[1] no tiene sentido
for(k in 2:KMax)
{
  km <- kmeans(variables_numericas, k, nstart=100, iter.max=10000)
  silk <- silhouette(km$cluster, distancias)
  avg.width[k] <- summary(silk)$avg.width
}
cbind(k=1:length(avg.width), avg.width=round(avg.width, digits=6))

# Representación gráfica de la variación de 'avg.width' en función de 'k'
points(avg.width, type="b", las=1, xlab="k", col = "red", pch = 18)

legend("topright", bty = "n",
       legend=c("Todas las variables","Variables tradicionales"),
       pch = c(15,18),pt.bg = 'white', col=c("black","red"), lwd = 1.25)

##########################################################################################
# Analisis mejor cluster (2017, tradicionales)
##########################################################################################
# experimento 2.3
datos <- datos0[datos0$Year==2017, variables_tradicionales]
datos$Year <- NULL
datos <- datos[complete.cases(datos),]
datos <- droplevels( x = datos )
variables_numericas <- Filter( f = is.numeric, x = datos ) # is.numeric reconoce tanto numeric como integer
variables_numericas[is.na(variables_numericas)] <- 0 # NA = 0
variables_numericas <- scale(variables_numericas, center = T, scale = T)
##########################################################################################
# Validar con el indice de la bondad de la partición: 'IB'
##########################################################################################
IB <- vector()
for(k in 1:KMax){
  km <- kmeans(variables_numericas, k, nstart=100, iter.max=10000)
  IB[k] <- km$betweenss/km$totss
}
#
valores_IB <- cbind(k=1:length(IB), IB=round(IB, digits=6))
valores_IB
selectk <- NULL
for(k in 2:length(IB))
  selectk <- rbind(selectk, c(k, IB[k]-IB[k-1]))
colnames(selectk) <- c("k", "IB[k]-IB[k-1]")
selectk

plot(valores_IB, type = "b")
val_x <- seq(1.5,14.5,1)
val_y <- valores_IB[1:14,2]+selectk[,2]/2+0.02
text(val_x, val_y, labels = round(selectk[,2], 2), col = "red")

#
# Se observa por los saltos que un crecimiento regular, la mejor seleccion es 2-3 cluster
# A partir de 4 el umento es inferior a 0.1

##########################################################################################
# Estudiar cluster
##########################################################################################
# experimento 2.3
datos <- datos0[datos0$Year==2017, variables_tradicionales]
datos$Year <- NULL
datos <- datos[complete.cases(datos),]
datos <- droplevels( x = datos )
variables_numericas <- Filter( f = is.numeric, x = datos ) # is.numeric reconoce tanto numeric como integer
variables_numericas[is.na(variables_numericas)] <- 0 # NA = 0
variables_numericas <- scale(variables_numericas, center = T, scale = T)
datos[,colnames(variables_numericas)] <- scale(datos[,colnames(variables_numericas)])
##########################################################################################
# Mejores resultados K=2 -> why?
km <- kmeans(variables_numericas, 2, nstart=100, iter.max=10000)
table(km$cluster)
table(datos$Pos[km$cluster==1])
table(datos$Pos[km$cluster==2])
# distribuciones de posiciones muy uniforme
k1 <- variables_numericas[km$cluster==1,]
k2 <- variables_numericas[km$cluster==2,]

colMeans(k1)-colMeans(k2)
plot(colMeans(k1)-colMeans(k2), type = "n", ylab = "Media Cluster1 - Media Cluster2")
text(colMeans(k1)-colMeans(k2), labels = colnames(k2))
# Uno de los cluster agrupa "mejores" jugadores

# Interesante opcion K=7 -> why?
km <- kmeans(variables_numericas, 7, nstart=100, iter.max=10000)
table(km$cluster)
##########################################################################################
# KMeans cada vez etiqueta los cluster con diferente numero (aunque siempre tienen los mismos elementos)
# Revisar las condiciones de la grafica (ej: datos$Pos != "C") para que concuerden con el numero del cluster
##########################################################################################
miCluster <- 1
table(datos$Pos[km$cluster==miCluster])
c <- variables_numericas[km$cluster==miCluster,]
nc <- variables_numericas[km$cluster!=miCluster & datos$Pos != "C", ]
plot(colMeans(c)-colMeans(nc), type = "n", ylab = "Media Cluster - Media", main = paste("Diferencia de medias para cluster", miCluster))
text(colMeans(c)-colMeans(nc), labels = colnames(nc))
##########################################################################################
##########################################################################################
miCluster <- 2
table(datos$Pos[km$cluster==miCluster])
c <- variables_numericas[km$cluster==miCluster,]
nc <- variables_numericas[km$cluster!=miCluster, ]
plot(colMeans(c)-colMeans(nc), type = "n", ylab = "Media Cluster - Media", main = paste("Diferencia de medias para cluster", miCluster))
text(colMeans(c)-colMeans(nc), labels = colnames(nc))
##########################################################################################
##########################################################################################
miCluster <- 3
table(datos$Pos[km$cluster==miCluster])
c <- variables_numericas[km$cluster==miCluster,]
nc <- variables_numericas[km$cluster!=miCluster & datos$Pos == "C", ]
plot(colMeans(c)-colMeans(nc), type = "n", ylab = "Media Cluster - Media", main = paste("Diferencia de medias para cluster", miCluster))
text(colMeans(c)-colMeans(nc), labels = colnames(nc))
##########################################################################################
##########################################################################################
miCluster <- 4
table(datos$Pos[km$cluster==miCluster])
c <- variables_numericas[km$cluster==miCluster,]
nc <- variables_numericas[km$cluster!=miCluster & datos$Pos != "C" & datos$Pos != "PF", ]
plot(colMeans(c)-colMeans(nc), type = "n", ylab = "Media Cluster - Media", main = paste("Diferencia de medias para cluster", miCluster))
text(colMeans(c)-colMeans(nc), labels = colnames(nc))
##########################################################################################
##########################################################################################
miCluster <- 5
table(datos$Pos[km$cluster==miCluster])
c <- variables_numericas[km$cluster==miCluster,]
nc <- variables_numericas[km$cluster!=miCluster & datos$Pos != "PG" & datos$Pos!="SG", ]
plot(colMeans(c)-colMeans(nc), type = "n", ylab = "Media Cluster - Media", main = paste("Diferencia de medias para cluster", miCluster))
text(colMeans(c)-colMeans(nc), labels = colnames(nc))
##########################################################################################
##########################################################################################
miCluster <- 6
table(datos$Pos[km$cluster==miCluster])
c <- variables_numericas[km$cluster==miCluster,]
nc <- variables_numericas[km$cluster!=miCluster & datos$Pos == "PG", ]
plot(colMeans(c)-colMeans(nc), type = "n", ylab = "Media Cluster - Media", main = paste("Diferencia de medias para cluster", miCluster))
text(colMeans(c)-colMeans(nc), labels = colnames(nc))
##########################################################################################
miCluster <- 7
table(datos$Pos[km$cluster==miCluster])
c <- variables_numericas[km$cluster==miCluster,]
nc <- variables_numericas[km$cluster!=miCluster & datos$Pos != "C", ]
plot(colMeans(c)-colMeans(nc), type = "n", ylab = "Media Cluster - Media", main = paste("Diferencia de medias para cluster", miCluster))
text(colMeans(c)-colMeans(nc), labels = colnames(nc))
