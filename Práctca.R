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

# Numero de familias
N <- nrow(Zumo)
print(N)

#' #### 2. Calcula la media de *Gastos*
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

#' #### 4. Realiza un muestreo polietápico considerando como estratos tanto UnidF como NivelEcon.
#' ####  Con afijación proporcional al 1%

#' Calculamos el tamaño de cada muestra, con la afijación al 0.01
nivel_afij <- 0.01

# Calculamos en número de categorías de cada estrato
n_NivelEcon <- length( unique(Zumo$NivelEcon))
n_unidf <- length( unique(Zumo$UnidF))

# calculamos el tamañod e cada muestra
n <- round((nivel_afij*N)/(n_NivelEcon*n_unidf))
print(n)

#' Con la función split, dividimos el dataframe en base al nivel económico y
#' de los valores de UnidF
sp <- split(Zumo, list(Zumo$NivelEcon, Zumo$UnidF))

#' Obtenemos las muestras de tamaño 253 para cada valor de UnidF de cada nivel económico
samples <- lapply(sp, function(x) x[sample(1:nrow(x), n, TRUE),])
muestra_polietap <- do.call(rbind, samples)
# Dimensión de la muestra completa
dim(muestra_polietap)

#' La cantidad de familias para cada nivel económico es de:
table(muestra_polietap$NivelEcon)

#' La cantidad de familias para valor de UnidF es de:
table(muestra_polietap$UnidF)


head(muestra_polietap)

#' #### 5. Calcula la media de Gastos de las familias de la muestra del apartado anterior
#' Utilizamos la función tapply, para calcular la media de cada valor de UnidF para cada
#' nivel económico
tapply(muestra_polietap$Gastos,list(muestra_polietap$NivelEcon,muestra_polietap$UnidF),mean)

