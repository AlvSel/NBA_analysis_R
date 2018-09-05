# Análisis datos
# PART 2: Datos según el tiempo

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
datosPlayers <- read.csv( file = "nba-players-stats-since-1950/Players.csv", header = T )
datosPlayers <- datosPlayers[, c("Player", "height", "weight")]
# Merge para tener en temporadas la altura y peso de cada jugador
datosSeasons <- merge( x = datosSeasons, y = datosPlayers, by = "Player" )

####################################
# Evolución estadísticas
####################################
stats <- datosSeasons[ , c("Year", "Pos", "G", "PTS", "X3P", "X3PA", "X2P", "X2PA", "AST", "TRB", "STL", "BLK", "MP", "height")]

####################################
# PTS, AST, TRB, STL, BLK
####################################
stats_temporada <- aggregate(stats[ , c("G", "PTS", "AST", "TRB", "STL", "BLK", "MP")],
                                 by=list(Year=stats$Year), 
                                 FUN=sum)
# Por partido
stats_temporada[,c("PTSG","ASTG","TRBG","STLG","BLKG","MPG")] <- round(stats_temporada[3:8] / stats_temporada$G, 4)
par(oma=c(0,0,2,0))
par(mfrow=c(2,3))
plot(stats_temporada$Year, stats_temporada$PTSG, type = "l", col = "red",
     main = "Puntos por partido",
     xlab = "Year", ylab = "Puntos", font.lab=2, cex.lab = 1.25)
plot(stats_temporada$Year, stats_temporada$TRBG, type = "l", col = "red",
     main = "Rebotes por partido",
     xlab = "Year", ylab = "Rebotes", font.lab=2, cex.lab = 1.25)
plot(stats_temporada$Year, stats_temporada$ASTG, type = "l", col = "red",
     main = "Asistencias por partido",
     xlab = "Year", ylab = "Asistencias", font.lab=2, cex.lab = 1.25)
plot(stats_temporada$Year, stats_temporada$STLG, type = "l", col = "red",
     main = "Robos por partido",
     xlab = "Year", ylab = "Robos", font.lab=2, cex.lab = 1.25)
plot(stats_temporada$Year, stats_temporada$BLKG, type = "l", col = "red",
     main = "Tapones por partido",
     xlab = "Year", ylab = "Tapones", font.lab=2, cex.lab = 1.25)
plot(stats_temporada$Year, stats_temporada$MPG, type = "l", col = "red",
     main = "Minutos por partido",
     xlab = "Year", ylab = "Minutos", font.lab=2, cex.lab = 1.25)
par(mfrow=c(1,1))
title(main="Estadísticas por jugador",outer=T, cex=2.0)
par(oma=c(0,0,0,0))

# Todas las variables se reducen a lo largo de los años => Los jugadores juegan menos tiempo
# Minutos por partido
MPG <- round(stats_temporada$MP / stats_temporada$G, 4)
# Temporadas ordenadas por minutos de media
datosSeasons$Year[sort(MPG, decreasing = T, index.return = T)$ix]
# Las últimas temporadas hay descenso de minutos por minuto (los entrenadores hacen mas cambios y rotan más)
# plot(stats_temporada$Year, MPG, type = "l", col = "red") #Desactivado para el mosaico de graficos

# Graficos anteriores ahora por minuto de juego
stats_temporada[,c("PTSMP","ASTMP","TRBMP","STLMP","BLKMP")] <- round(stats_temporada[3:7] / stats_temporada$MP, 4)
# Por minuto
par(oma=c(0,0,2,0))
par(mfrow=c(2,3))
plot(stats_temporada$Year, stats_temporada$PTSMP, type = "l", col = "red",
     main = "Puntos por minuto",
     xlab = "Year", ylab = "Puntos")
plot(stats_temporada$Year, stats_temporada$TRBMP, type = "l", col = "red",
     main = "Rebotes por minuto",
     xlab = "Year", ylab = "Rebotes")
plot(stats_temporada$Year, stats_temporada$ASTMP, type = "l", col = "red",
     main = "Asistencias por minuto",
     xlab = "Year", ylab = "Asistencias")
plot(stats_temporada$Year, stats_temporada$STLMP, type = "l", col = "red",
     main = "Robos por minuto",
     xlab = "Year", ylab = "Robos")
plot(stats_temporada$Year, stats_temporada$BLKMP, type = "l", col = "red",
     main = "Tapones por minuto",
     xlab = "Year", ylab = "Tapones")
par(mfrow=c(1,1))
title(main="Estadísticas por jugador",outer=T, cex=2.0)
par(oma=c(0,0,0,0))

####################################
# Tiros (T2 y T3)
####################################
tiros_temporada <- aggregate(stats[ , c("G", "X3P", "X3PA", "X2P", "X2PA", "MP")],
                                 by=list(Year=stats$Year), 
                                 FUN=sum, na.rm = T)
tiros_temporada <- tiros_temporada[tiros_temporada$Year>1979,]
tiros_temporada[,c("X3PG","X3PAG","X2PG","X2PAG")] <- round(tiros_temporada[3:6] / tiros_temporada$G, 4)

ymin <- min(tiros_temporada[,c("X3PG","X3PAG","X2PG","X2PAG")], na.rm = T)
ymax <- max(tiros_temporada[,c("X3PG","X3PAG","X2PG","X2PAG")], na.rm = T)
plot(tiros_temporada$Year, tiros_temporada$X3PG, type = "l", col = "green",
     main = "Tiros por partido",
     ylim = c(ymin, ymax),
     xlab = "Year", ylab = "Tiros por partido", lwd = 1.25, cex.lab = 1.25)
points(tiros_temporada$Year, tiros_temporada$X3PAG, type = "l", col = "red", lwd = 1.25)
points(tiros_temporada$Year, tiros_temporada$X2PG, type = "l", col = "green", lty = 2, lwd = 1.25)
points(tiros_temporada$Year, tiros_temporada$X2PAG, type = "l", col = "red", lty = 2, lwd = 1.25)
legend("topright", bty = "n",
       legend=c("T2 lanzados","T2 anotados","T3 lanzados","T3 anotados"),
       lty=c(2,2,1,1), col=c("red","green","red","green"), lwd = 1.25)

# Aumento significativo de los triples, descenso de los T2
# Triples intentados por posición
triples_temporada_posicion <- aggregate(stats[ , c("G", "X3PA")],
                                                by=list(Year=stats$Year,Pos=stats$Pos), 
                                                FUN=sum, na.rm = T)
triples_temporada_posicion <- triples_temporada_posicion[triples_temporada_posicion$Year > 1979,]
triples_temporada_posicion["X3PAG"] <- round(triples_temporada_posicion$X3PA / triples_temporada_posicion$G, 4)

PG_triples <- triples_temporada_posicion[triples_temporada_posicion$Pos=="PG",]$X3PAG
SG_triples <- triples_temporada_posicion[triples_temporada_posicion$Pos=="SG",]$X3PAG
SF_triples <- triples_temporada_posicion[triples_temporada_posicion$Pos=="SF",]$X3PAG
PF_triples <- triples_temporada_posicion[triples_temporada_posicion$Pos=="PF",]$X3PAG
C_triples <- triples_temporada_posicion[triples_temporada_posicion$Pos=="C",]$X3PAG

ymin <- min(PG_triples, SG_triples, SF_triples, PF_triples, C_triples)
ymax <- max(PG_triples, SG_triples, SF_triples, PF_triples, C_triples)
plot(1979:2016, PG_triples, type = "l", col = "blue",
     main = "Triples por partido por posicion",
     ylim = c(ymin,ymax),
     ylab = "Triples por partido", xlab = "Year", cex.lab = 1.25, lwd = 1.25)
points(1979:2016, SG_triples, type = "l", col = "green", lwd = 1.25)
points(1979:2016, SF_triples, type = "l", col = "violet", lwd = 1.25)
points(1979:2016, PF_triples, type = "l", col = "orange", lwd = 1.25)
points(1979:2016, C_triples, type = "l", col = "red", lwd = 1.25)
legend("topleft", bty="n",
       legend=c("PG","SG","SF","PF","C"), text.col=c("blue","green","violet","orange","red"),
       lty=c(1,1), col=c("blue","green","violet","orange","red"), lwd = 1.25)

# Todas las posiciones aumentan, los aumentos de los ala-pivots y pivots muy grande
####################################
# Stats por altura
####################################
stats_altura <- aggregate(stats[ , c("PTS", "AST", "TRB", "STL", "BLK", "X3P")],
                             by=list(height = stats$height), 
                             FUN=mean, na.rm = T)
stats_ultimos <- stats[stats$Year>2009, ]
stats_altura_ultimos <- aggregate(stats_ultimos[ , c("PTS", "AST", "TRB", "STL", "BLK", "X3P")],
                          by=list(height = stats_ultimos$height), 
                          FUN=mean, na.rm = T)
par(oma=c(0,0,2,0))
par(mfrow=c(2,3))
#Puntos
ymin <- min(stats_altura$PTS, stats_altura_ultimos$PTS)
ymax <- max(stats_altura$PTS, stats_altura_ultimos$PTS)
plot(stats_altura$height, stats_altura$PTS, col = "red", ylim = c(ymin,ymax),
     ylab = "Media Puntos", xlab = "Altura (cm)", cex.lab = 1.25)
points(stats_altura_ultimos$height, stats_altura_ultimos$PTS, col = "blue", pch = 4)
#Asistencias
ymin <- min(stats_altura$AST, stats_altura_ultimos$AST)
ymax <- max(stats_altura$AST, stats_altura_ultimos$AST)
plot(stats_altura$height, stats_altura$AST, col = "red", ylim = c(ymin,ymax),
     ylab = "Media Asistencias", xlab = "Altura (cm)", cex.lab = 1.25)
points(stats_altura_ultimos$height, stats_altura_ultimos$AST, col = "blue", pch = 4)
#Rebotes
ymin <- min(stats_altura$TRB, stats_altura_ultimos$TRB)
ymax <- max(stats_altura$TRB, stats_altura_ultimos$TRB)
plot(stats_altura$height, stats_altura$TRB, col = "red", ylim = c(ymin,ymax),
     ylab = "Media Rebotes", xlab = "Altura (cm)", cex.lab = 1.25)
points(stats_altura_ultimos$height, stats_altura_ultimos$TRB, col = "blue", pch = 4)
#Robos
ymin <- min(stats_altura$STL, stats_altura_ultimos$STL)
ymax <- max(stats_altura$STL, stats_altura_ultimos$STL)
plot(stats_altura$height, stats_altura$STL, col = "red", ylim = c(ymin,ymax),
     ylab = "Media Robos", xlab = "Altura (cm)", cex.lab = 1.25)
points(stats_altura_ultimos$height, stats_altura_ultimos$STL, col = "blue", pch = 4)
#Tapones
ymin <- min(stats_altura$BLK, stats_altura_ultimos$BLK)
ymax <- max(stats_altura$BLK, stats_altura_ultimos$BLK)
plot(stats_altura$height, stats_altura$BLK, col = "red", ylim = c(ymin,ymax),
     ylab = "Media Tapones", xlab = "Altura (cm)", cex.lab = 1.25)
points(stats_altura_ultimos$height, stats_altura_ultimos$BLK, col = "blue", pch = 4)
#Triples anotados
ymin <- min(stats_altura$X3P, stats_altura_ultimos$X3P)
ymax <- max(stats_altura$X3P, stats_altura_ultimos$X3P)
plot(stats_altura$height, stats_altura$X3P, col = "red", ylim = c(ymin,ymax),
     ylab = "Media Triples Anotados", xlab = "Altura (cm)", cex.lab = 1.25)
points(stats_altura_ultimos$height, stats_altura_ultimos$X3P, col = "blue", pch = 4)
par(mfrow=c(1,1))
title(main="Medias por altura",outer=T, cex=2.0)
legend("topleft", bty="n", horiz = T, xpd = T, inset = -0.27,
       legend=c("Todas las temporadas","A partir de 2010"), pch = c(1,4),
       col=c("red","blue"))
par(oma=c(0,0,0,0))
# Jugadores bajos más asistencias, robos y triples
# Jugadores altos más rebotes y tapones
# Puntos más o menos estable

#######################
# GIF TRIPLES (desactivado)
#######################
# triples_temporada_altura <- aggregate(stats[ , c("G", "X3PA")],
#                                         by=list(Year=stats$Year,height=stats$height), 
#                                         FUN=sum, na.rm = T)
# triples_temporada_altura <- triples_temporada_altura[triples_temporada_altura$Year > 1979,]
# triples_temporada_altura["X3PAG"] <- round(triples_temporada_altura$X3PA / triples_temporada_altura$G, 4)
# 
# xmax = max(triples_temporada_altura["height"])
# xmin = min(triples_temporada_altura["height"])
# ymax = max(triples_temporada_altura["X3PAG"])
# ymin = min(triples_temporada_altura["X3PAG"])
# for(i in unique(triples_temporada_altura$Year)){
#   png(paste0("NBA_Height_X3PAG_",i,".png"))
#   plot(triples_temporada_altura[triples_temporada_altura$Year==i, "height"], 
#        triples_temporada_altura[triples_temporada_altura$Year==i, "X3PAG"], 
#        col = "red", type = "h",
#        main = paste0("Triples lanzados por partido (Año ",i,")"),
#        xlim = c(xmin, xmax), xlab = "Altura (cm)",
#        ylim = c(ymin, ymax), ylab = "Triples")
#   dev.off()
# }
