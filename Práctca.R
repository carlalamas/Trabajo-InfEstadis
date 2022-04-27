#' ---
#' title: "Trabajo páctico evaluable"
#' author: "Bruno Corucuera Sánchez"
#' date: "Curso 2021-2022"
#' output: html_document
#' ---

#' ## Trabajo Inferencia Estadística - GCID
#' ## Ejercicio 1

#' #### 1. Se carga el fichero de datos y se visualizan las primeras filas y el tamaño
#' #### del mismo
load('Zumo.Rdata')
print(head(Zumo,10))

# Numero de familias totales
N <- nrow(Zumo)
print(N)

#' #### 2. Calcula la media de *Gastos* de toda la población
mean(Zumo$Gastos)

#' #### 3. Calcula las medias de Gastos de las familias para cada uno de los valores de *UnidF*
#' Tenemos que calcular la media de gastos en base a los distintos valores de unidF qu son:
unidf <- unique(Zumo$UnidF)

#' Hacemos uso de la función *tapply* para calcular la media de cada *Unidf*
media_gasto <- tapply(Zumo$Gastos,Zumo$UnidF, mean)
df_media <- data.frame(unidf,media_gasto)

# Ordenamos el dataframe por la columna unidF
df_media <- df_media[order(df_media$unidf), ]
print(df_media)

#' #### 4. Realiza un muestreo polietápico considerando como estratos tanto UnidF 
#' #### como NivelEcon. Con afijación proporcional al $1%$


#' Con la función split, dividimos el dataframe de la población en base al nivel
#' económico y de los valores de UnidF
Zumo_split <- split(Zumo, list(Zumo$NivelEcon, Zumo$UnidF))

#' Obtenemos las muestras de $1%$ para cada valor de UnidF de cada nivel económico
#' Creamos la función *sample_df* en la que obtenemos una muestra de tamaño $1%$
#' para cada estrato.
# Se utiliza la sentencia "ceiling(length(x[['Id']])*0.01)" para calcular el tamaño
# de la muestra de ese estrato, contando el numero de familias, multiplicándolo
# por 0.01 y rodeondeando hacia arriba con "celling"
sample_df <- lapply(Zumo_split, function(x) x[sample(1:nrow(x), ceiling(length(x[['Id']])*0.01), TRUE),])
muestra_polietap <- do.call(rbind, sample_df)

# Dimensión de la muestra completa
dim(muestra_polietap)

#' Tamaño de la muestra para cada nivel económico es de:
table(muestra_polietap$NivelEcon)


#' tamaña de la muestra para cada valor de UnidF es de:
table(muestra_polietap$UnidF)

#' Tamaño muestras según numero de miembros familia y nivel económico 
table(muestra_polietap$NivelEcon,muestra_polietap$UnidF)

head(muestra_polietap)

#' #### 5. Calcula la media de Gastos de las familias de la muestra del apartado anterior
#' Media total de la muestra
mean(muestra_polietap$Gastos, na.rm=TRUE)

#' Utilizamos la función tapply, para calcular la media de cada valor de UnidF para cada
#' nivel económico. Se puede observar que hay medias con valores nulos ya que no existen familias
#' de nibel bajo con 9,10,11,12,13 UnidF por ejemplo.
tapply(muestra_polietap$Gastos,list(muestra_polietap$NivelEcon,muestra_polietap$UnidF),mean)


#' ## Ejercicio 2

#' #### 1. Calcula el valor de p
#' Creamos un nuevo dataframe *Zumo_2* que solo contiene familias de Región 2
#' y la unidad familiar consta de 1 o 2 miembros.
Zumo_2 <- Zumo[Zumo$Region==2 & Zumo$UnidF %in% c(1,2),]
head(Zumo_2,10)

#' La proporción entre familias de la Región 2 con 1 o 2 miembros 
p <- nrow(Zumo_2) / nrow(Zumo[Zumo$Region==2,])
print(p)  
  
  
#' #### 2. Utilizando las fórmulas de la teoría, calcula el tamaño muestral n 
#' #### necesario para estimar la proporción con un error máximo de 0.035
#' ####  y una confianza del 95 %. Por estudios previos, sospechas que p $\approx$ 0.3.
nivel_conf <- 0.95
error_max <- 0.035

#Creamos una función que nos permita obtener el tamaño mustral de una proporción
n_size <- function(nivel_conf,p,l) {
  alpha <- (1-nivel_conf)/2
  # tamaño muestral con p aproximado
  n <- (4*qnorm(alpha,lower.tail = F)**2)*p*(1-p) / l**2 
  
  # tamaño muestral en el peor caso
  n2 <- (qnorm(alpha/2,lower.tail = F)**2)/ l**2 
  
  df <-data.frame(n,n2)
  names(df) <- c('p aproximado','peor caso')
  return(df)
}
#' Tamaño muestral con $p \approx 0.3$ y peor caso $p = 1/2$
n_size(nivel_conf,0.3,error_max)

#' Tamaño muestral con $p=0.3686291$ (calulado en el apartado interior) y peor caso $p = 1/2$
n_size(nivel_conf,p,error_max)



