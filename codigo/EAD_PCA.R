# PART 3: PCA

####################################
# Carga y limpieza datos0
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


####################################

experimento_pca <- function( pData, pTag){
  datos0 <- pData
  tag <- pTag
  
  print(paste("Experimento: ",tag))
  variables_numericas <- Filter( f = is.numeric, x = datos0 ) # is.numeric reconoce tanto numeric como integer
  variables_numericas[is.na(variables_numericas)] <- 0 # NA = 0

  print(paste(tag,": Normalizando y centrando")) # Discrimina mejor las posiciones
  mi_pca <- prcomp(variables_numericas, center = T, scale = T)
  print(summary(mi_pca))
  print(paste(tag,": Representación 2 primeras componentes"))
  componentes2 <- mi_pca$x[,1:2]
  plot(x = componentes2, col = datos0$Pos,
       main = paste(tag,": Representación 2 primeras componentes"))
  legend("topleft", bty = "n", pch = 1,
         legend=c("PG","SG","SF","PF","C"),
         col=c("green","cyan","blue","red","black"),text.col=c("green","cyan","blue","red","black"))
  print(paste(tag,": Representación 3 primeras componentes"))
  componentes3 <- mi_pca$x[,1:3]
  # add tag color for each Pos
  datos0$pcolor[datos0$Pos=="PG"] <- "red"
  datos0$pcolor[datos0$Pos=="SG"] <- "blue"
  datos0$pcolor[datos0$Pos=="SF"] <- "darkgreen"
  datos0$pcolor[datos0$Pos=="PF"] <- "yellow"
  datos0$pcolor[datos0$Pos=="C"] <- "black"
  for (i in seq(100,300,100)) {
    #scatterplot3d::scatterplot3d(componentes3, color = datos0$pcolor, angle = i,
    #                             main = paste(tag,": Representación 3 primeras componentes"))
  }
  return(mi_pca)
}

########################################################################
# Experimento 1: Todas las variables y todas las temporadas
########################################################################
datos0 <- datosSeasons
tag <- "TodasVariables_TodasTemporadas"
mi_pca <- experimento_pca(datos0, tag)
########################################################################
# Experimento 2: Todas las variables y temporadas modernas (A partir de 1980)
########################################################################
datos0 <- datosSeasons[datosSeasons$Year>1979,]
tag <- "TodasVariables_TodasModernas"
mi_pca <- experimento_pca(datos0, tag)
########################################################################
# Experimento 3: Todas las variables y ultimas temporadas (A partir de 2000)
########################################################################
datos0 <- datosSeasons[datosSeasons$Year>1999,]
tag <- "TodasVariables_UltimasTemporadas"
mi_pca <- experimento_pca(datos0, tag)
########################################################################
# Experimento 4: Variables tradicionales y todas las temporadas
########################################################################
variables_tradicionales <- c("Year", "Age", "G", "GS", "MP",
                             "FG", "FGA", "FG.",
                             "FT", "FTA", "FT.",
                             "X3P", "X3PA", "X3P.",
                             "X2P", "X2PA", "X2P.",
                             "ORB", "DRB", "TRB",
                             "AST", "STL", "BLK", "TOV", "PTS",
                             "PF", "height", "weight",
                             "Pos"
                             )
datos0 <- datosSeasons[,variables_tradicionales]
tag <- "VariablesTradicionales_TodasTemporadas"
mi_pca <- experimento_pca(datos0, tag)
########################################################################
# Experimento 5: Variables tradicionales y y temporadas modernas (A partir de 1980)
########################################################################
datos0 <- datosSeasons[datosSeasons$Year>1979,variables_tradicionales]
tag <- "VariablesTradicionales_TemporadasModernas"
mi_pca <- experimento_pca(datos0, tag)
########################################################################
# Experimento 6: Variables tradicionales y ultimas temporadas (A partir de 2000)
########################################################################
datos0 <- datosSeasons[datosSeasons$Year>1999,variables_tradicionales]
tag <- "VariablesTradicionales_UltimasTemporadas"
mi_pca <- experimento_pca(datos0, tag)
# me interesa las dos primeras componentes de este experimento
library(ggrepel)
library(ggplot2)
rotation_data_frame <- as.data.frame(mi_pca$rotation)
ggplot(rotation_data_frame, aes(PC1, PC2, label = rownames(rotation_data_frame))) +
  geom_text_repel() +
  geom_point(color = 'red') +
  theme_classic(base_size = 16)
########################################################################
# Experimento 7: Variables eta2 y todas las temporadas
########################################################################
#variables con eta2 > 0.1 (calculadas part1)
variables_eta2 <- c("TRB.","AST.","ORB.","DRB.","BLK.","X3PAr","ORB","BLK","AST","X3PA","DBPM","X3P","X3P.","FG.","TRB","Pos")
datos0 <- datosSeasons[,variables_eta2]
tag <- "VariablesEta2_TodasTemporadas"
mi_pca <- experimento_pca(datos0, tag)
########################################################################
# Experimento 8: Variables eta2 y temporadas modernas (A partir de 1980)
########################################################################
datos0 <- datosSeasons[datosSeasons$Year>1979,variables_eta2]
tag <- "VariablesEta2_TodasModernas"
mi_pca <- experimento_pca(datos0, tag)
########################################################################
# Experimento 9: Variables eta2 y ultimas temporadas (A partir de 2000)
########################################################################
datos0 <- datosSeasons[datosSeasons$Year>1999,variables_eta2]
tag <- "VariablesEta2_UltimasTemporadas"
mi_pca <- experimento_pca(datos0, tag)
##########################################################################################
# Experimento 10: Totales y ultimas temporadas (A partir de 2010) y mas de 20 partido
##########################################################################################
datos0 <- datosSeasons[datosSeasons$Year>2009 & datosSeasons$G>20,]
variables_suma <- c("G", "GS","MP","FT","FTA","FG","FGA","X3P","X3PA","X2P","X2PA","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS")
total_suma <- aggregate(datos0[ , variables_suma],
                        by=list(Player = datos0$Player), 
                        FUN=sum, na.rm = T)
variables_media <- c("PER","TS.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","OWS","DWS","WS","WS.48","OBPM","DBPM","BPM","VORP","FG.","X3P.","X2P.","eFG.","FT.")
total_media <- aggregate(datos0[ , variables_media],
                        by=list(Player = datos0$Player), 
                        FUN= mean, na.rm = T)
total_media[2:length(total_media)] <- round(total_media[2:length(total_media)],4)
variables_moda <- c("Pos","height","weight")
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
total_moda <- aggregate(datos0[ , variables_moda],
                         by=list(Player = datos0$Player), 
                         FUN= Mode)
datos0 <- merge(x = total_media, y = total_suma, by = "Player")
datos0 <- merge(x = datos0, y = total_moda, by = "Player")

tag <- "Totales_UltimasTemporadas"
mi_pca <- experimento_pca(datos0, tag)
