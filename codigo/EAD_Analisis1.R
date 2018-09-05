# Visualización y estadísticos
# PART 1: Descripción 1 Variable

#############################
# Carga y limpieza datos
#############################
datosSeasons <- read.csv( file = "nba-players-stats-since-1950/Seasons_Stats.csv", header = T )
# Algunas instancias están vacias => borrar
instancias_vacias <- is.na( x = datosSeasons$Year )
datosSeasons <- datosSeasons[!instancias_vacias, ]

# Descripción de las variables una a una
dim( x = datosSeasons )
# 53 variables, 24624 instancias
sapply( X = datosSeasons, FUN = class )
table( sapply( X = datosSeasons, FUN = class ) )
# 3 variables factor, 23 integer, 25 numericas, 2 logical (estas 2 son siempre NA => elimino)
# delete "blanl" and "blank2" (empty columns)
datosSeasons <- subset( x = datosSeasons, select = -c( blanl , blank2 ) )

summary( object = datosSeasons )
# Existen variables con NA (en temporadas antiguas algunas estadísticas no se apuntaban)
# primera variable codigo identificador => elimino
datosSeasons <- subset( x = datosSeasons, select = -c(X) )

# Variables finales
table( sapply( X = datosSeasons, FUN = class ) )
# 3 variables factor, 22 integer, 25 numericas

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
table(datosSeasons$Pos)
# Posiciones muy distribuidas

#############################
# Estadisticos
#############################

datos0 <- datosSeasons

#VARIABLES NUMÉRICAS
variables_numericas <- Filter( f = is.numeric, x = datos0 ) # is.numeric reconoce tanto numeric como integer
estadisticos_variables_numericas <- cbind(mean   = apply( X = variables_numericas, MARGIN = 2, FUN = mean,   na.rm = T),
                                          median = apply( X = variables_numericas, MARGIN = 2, FUN = median, na.rm = T),
                                          var    = apply( X = variables_numericas, MARGIN = 2, FUN = var,    na.rm = T))
# Diferencia entre media y mediana
sort(estadisticos_variables_numericas[,"mean"]-estadisticos_variables_numericas[,"median"])
# Varianza
sort(estadisticos_variables_numericas[,"var"])
# Las variables con mas varianza son minutos por partido y puntos anotados
# Por lo general las variables tienen poca varianza.
t( x = apply( X = variables_numericas, MARGIN = 2, FUN = range , na.rm = T )) # traspuesta para visualizar mejor
# Varias variables tienen rango [0,1], son porcentajes o ratios

#VARIABLES CUALITATIVAS
variables_cualitativas <- subset(x = datos0, select = c(Year, Pos, Tm) ) # considero Year como cualitativa
apply( X = variables_cualitativas, MARGIN = 2, FUN = table )
# YEAR
plot( x = table( variables_cualitativas$Year ),type = "l", col = "red",
      xlab = "Year", ylab = "Número jugadores")
# Número de datos crece según los años, tiene sentido antes había menos equipos
# https://en.wikipedia.org/wiki/List_of_National_Basketball_Association_seasons#ref_Note4d


# PART 2: Descripción asociaciones de Variables
library(corrplot)
variables_tradicionales <- c("Year", "Age", "G", "GS", "MP","FG", "FGA", "FG.",
                             "FT", "FTA", "FT.","X3P", "X3PA", "X3P.","X2P", "X2PA", "X2P.",
                             "ORB", "DRB", "TRB","AST", "STL", "BLK", "TOV", "PTS","PF")
variables_numericas_reducidas <- variables_numericas[,variables_tradicionales]
# elimino algunas variables para cuadro más legible
variables_numericas_reducidas <- subset( x = variables_numericas_reducidas, select = -c(GS, FG., FT., X3P., X2P.) )
# jugadores con mas de 25 partidos
variables_numericas_reducidas <- variables_numericas_reducidas[variables_numericas_reducidas$G>25,]
correlaciones <- cor(variables_numericas_reducidas, use = "complete.obs" )
corrplot(correlaciones, method = "square", is.corr = F, tl.cex = 1, tl.col = "black", tl.offset = 0.25)

# Correlación con la clase (posición)
# Clase nominal => uso eta2 de apuntes
eta2 <- function(x, factor){
  niv <- levels(factor)
  numniv <- length(niv)
  SSB <- 0
  for(i in 1:numniv){
    xx <- x[factor==niv[i]]
    nxx <- length(xx)
    SSB <- SSB + nxx*(mean(xx)-mean(x))^2
  }
  SST <- (length(x)-1)*var(x)
  eta2value <- SSB/SST
  return(eta2value)
}
indices_eta2 <- apply( X = variables_numericas, MARGIN = 2, FUN = function(x){eta2(x[!is.na(x)],datos0$Pos[!is.na(x)])})
indices_eta2 <- sort(x = indices_eta2, decreasing = T)
indices_eta2

###########################################
# Naive clasificacion de Posiciones, mirando solo altura y peso del jugador
###########################################
# Dataset con alturas y pesos
datosPlayers <- read.csv( file = "nba-players-stats-since-1950/Players.csv", header = T )
datosPlayers <- datosPlayers[, c("Player", "height", "weight")]
nombre_pos <- subset( x = datos0, select = c("Player","Pos") )
# Merge para tener nombre, altura, peso y posicion
nombre_pos <- merge( x = datosPlayers, y = nombre_pos, by = "Player" )
# Elimino los repetidos (existen jugadores puede tener posiciones diferentes depende la temporada, estas las conservo)
nombre_pos <- unique( x = nombre_pos )

plot(x = nombre_pos$height, y = nombre_pos$weight,
     main = "Posiciones dependiendo altura y peso.",
     xlab = "Altura (cm)", ylab = "Peso (kg)",
     col = nombre_pos$Pos, 
     pch = c(16, 17, 18, 19, 20)[as.numeric( x = nombre_pos$Pos )])
legend( x = 160, y = 160,c("PG","SG","SF","PF","C"), 
        col = c("green", "cyan", "blue", "red", "black"), 
        pch = c(16, 17, 18, 19, 20))
# Los bases por lo general son los mas bajos y menos pesado, los pivots son el caso contrario.
# En la parte central hay mucho solapamiento de escoltas, aleros, ala-pivots.
# También se solapan los bases con los escoltas y los ala-pivots con los pivots.
