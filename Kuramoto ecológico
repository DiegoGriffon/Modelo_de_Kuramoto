# ===============================================================
# MODELO ECOLOGICO ESPACIAL DE KURAMOTO EN R
# ===============================================================
#
# SINCRONIZACION DE LUCIERNAGAS EN UN PAISAJE
#
# ---------------------------------------------------------------
# Este modelo incorpora:
#
# 1. Osciladores biológicos
# 2. Espacio explícito
# 3. Distancia entre individuos
# 4. Densidad local
# 5. Conectividad espacial
# 6. Vegetación / paisaje
# 7. Contaminación lumínica
# 8. Ruido ambiental
# 9. Sincronización parcial
# 10. Resiliencia ecológica
#
# ---------------------------------------------------------------
# IDEA GENERAL
# ---------------------------------------------------------------
#
# Cada luciérnaga:
#
# - posee un reloj interno
# - destella periódicamente
# - interactúa con individuos cercanos
# - percibe señales luminosas
# - responde al paisaje y al ambiente
#
# El objetivo es estudiar cómo emerge
# sincronización colectiva.
#
# ===============================================================



# ===============================================================
# 1. PARAMETROS MODIFICABLES 
# ===============================================================

# ---------------------------------------------------------------
# NUMERO DE LUCIERNAGAS
# ---------------------------------------------------------------

N <- 80


# ---------------------------------------------------------------
# TAMAÑO DEL PAISAJE
# ---------------------------------------------------------------
#
# El paisaje será un cuadrado:
#
# 0 ----- landscape.size
#
landscape.size <- 100


# ---------------------------------------------------------------
# FUERZA BASE DE ACOPLAMIENTO
# ---------------------------------------------------------------
#
# Controla cuánto influye una luciérnaga
# sobre otra.
#
# Valores bajos:
# -> poca sincronización
#
# Valores altos:
# -> fuerte sincronización
#
K <- 2


# ---------------------------------------------------------------
# ATENUACION ESPACIAL
# ---------------------------------------------------------------
#
# Controla cuánto disminuye la interacción
# con la distancia.
#
# alpha pequeño:
# -> interacción de largo alcance
#
# alpha grande:
# -> interacción muy local
#
alpha <- 0.05


# ---------------------------------------------------------------
# RADIO DE INTERACCION LOCAL
# ---------------------------------------------------------------
#
# Distancia máxima considerada
# para vecinos locales.
#
neighbor.radius <- 20


# ---------------------------------------------------------------
# EFECTO DE CONTAMINACION LUMINICA
# ---------------------------------------------------------------
#
# beta grande:
# -> la luz artificial reduce mucho
#    la sincronización
#
beta <- 2


# ---------------------------------------------------------------
# INTENSIDAD DEL RUIDO AMBIENTAL
# ---------------------------------------------------------------
#
# Representa:
#
# - viento
# - temperatura
# - interferencia ambiental
# - errores sensoriales
#
sigma <- 0.05


# ---------------------------------------------------------------
# PASO TEMPORAL
# ---------------------------------------------------------------

dt <- 0.05


# ---------------------------------------------------------------
# TIEMPO TOTAL DE SIMULACION
# ---------------------------------------------------------------

Tmax <- 100


# ---------------------------------------------------------------
# PASOS TOTALES
# ---------------------------------------------------------------

steps <- Tmax / dt



# ===============================================================
# 2. POSICION ESPACIAL DE LAS LUCIERNAGAS
# ===============================================================
#
# Cada luciérnaga tendrá coordenadas (x,y)
# dentro del paisaje.
#
set.seed(123)

x <- runif(N, 0, landscape.size)
y <- runif(N, 0, landscape.size)



# ===============================================================
# 3. FRECUENCIAS NATURALES
# ===============================================================
#
# Cada individuo posee una frecuencia
# de destello ligeramente distinta.
#
omega <- rnorm(N, mean = 1, sd = 0.1)



# ===============================================================
# 4. FASES INICIALES
# ===============================================================
#
# Cada luciérnaga inicia en una fase aleatoria.
#
theta <- runif(N, 0, 2*pi)



# ===============================================================
# 5. DISTANCIAS ENTRE INDIVIDUOS
# ===============================================================
#
# Calculamos matriz de distancias espaciales.
#
dist.mat <- as.matrix(dist(cbind(x,y)))



# ===============================================================
# 6. DENSIDAD LOCAL
# ===============================================================
#
# Calculamos cuántos vecinos tiene
# cada individuo.
#
rho <- rowSums(dist.mat < neighbor.radius)



# ===============================================================
# 7. VEGETACION / VISIBILIDAD
# ===============================================================
#
# Simulamos heterogeneidad del paisaje.
#
# Valores altos:
# -> mejor visibilidad
#
# Valores bajos:
# -> vegetación densa
#
visibility <- matrix(
  runif(N*N, 0.3, 1),
  nrow = N
)



# ===============================================================
# 8. CONTAMINACION LUMINICA
# ===============================================================
#
# Cada punto del paisaje tendrá
# un nivel de contaminación lumínica.
#
# 0 = oscuridad total
# 1 = iluminación máxima
#
light <- runif(N, 0, 1)



# ===============================================================
# 9. MATRIZ ECOLOGICA DE ACOPLAMIENTO
# ===============================================================
#
# Aquí combinamos:
#
# - distancia
# - vegetación
# - contaminación lumínica
# - densidad local
#
# para definir interacción ecológica.
#
Kij <- matrix(0, nrow = N, ncol = N)



for(i in 1:N){
  
  for(j in 1:N){
    
    # -----------------------------------------------------------
    # EFECTO DE DISTANCIA
    # -----------------------------------------------------------
    #
    # La interacción disminuye exponencialmente
    # con la distancia.
    #
    spatial.effect <- exp(-alpha * dist.mat[i,j])
    
    
    # -----------------------------------------------------------
    # EFECTO DE VEGETACION
    # -----------------------------------------------------------
    #
    vegetation.effect <- visibility[i,j]
    
    
    # -----------------------------------------------------------
    # EFECTO DE CONTAMINACION LUMINICA
    # -----------------------------------------------------------
    #
    # Mucha luz artificial disminuye
    # comunicación.
    #
    light.effect <- exp(
      -beta * mean(c(light[i], light[j]))
    )
    
    
    # -----------------------------------------------------------
    # EFECTO DE DENSIDAD LOCAL
    # -----------------------------------------------------------
    #
    density.effect <- rho[i] / max(rho)
    
    
    # -----------------------------------------------------------
    # ACOPLAMIENTO FINAL
    # -----------------------------------------------------------
    #
    Kij[i,j] <- K *
      spatial.effect *
      vegetation.effect *
      light.effect *
      density.effect
  }
}



# ===============================================================
# 10. MATRIZ PARA ALMACENAR RESULTADOS
# ===============================================================

theta.mat <- matrix(NA, nrow = steps, ncol = N)

R <- numeric(steps)



# ===============================================================
# 11. SIMULACION PRINCIPAL
# ===============================================================
#
# Aquí ocurre la dinámica ecológica.
#
for(t in 1:steps){
  
  theta.new <- theta
  
  
  # =============================================================
  # RECORREMOS CADA LUCIERNAGA
  # =============================================================
  #
  for(i in 1:N){
    
    # -----------------------------------------------------------
    # ACOPLAMIENTO COLECTIVO
    # -----------------------------------------------------------
    #
    coupling <- 0
    
    for(j in 1:N){
      
      coupling <- coupling +
        Kij[i,j] *
        sin(theta[j] - theta[i])
    }
    
    
    # -----------------------------------------------------------
    # RUIDO AMBIENTAL
    # -----------------------------------------------------------
    #
    noise <- rnorm(1, 0, sigma)
    
    
    # -----------------------------------------------------------
    # ECUACION ECOLOGICA DE KURAMOTO
    # -----------------------------------------------------------
    #
    dtheta <- omega[i] + coupling + noise
    
    
    # -----------------------------------------------------------
    # INTEGRACION NUMERICA
    # -----------------------------------------------------------
    #
    theta.new[i] <- theta[i] + dtheta * dt
  }
  
  
  # Actualizamos fases
  #
  theta <- theta.new
  
  
  # Guardamos resultados
  #
  theta.mat[t, ] <- theta
  
  
  # =============================================================
  # PARAMETRO DE SINCRONIZACION
  # =============================================================
  #
  z <- mean(exp(1i * theta))
  
  R[t] <- Mod(z)
}



# ===============================================================
# 12. GRAFICO DE SINCRONIZACION GLOBAL
# ===============================================================

plot(
  seq(1, Tmax, length.out = steps),
  R,
  type = "l",
  lwd = 3,
  xlab = "Tiempo",
  ylab = "Sincronización (R)",
  main = "Sincronización colectiva"
)

abline(h = 1, lty = 2)



# ===============================================================
# 13. VISUALIZACION ESPACIAL
# ===============================================================
#
# Mostramos posición espacial
# y fase final de cada luciérnaga.
#
phase.colors <- rainbow(N)

plot(
  x,
  y,
  pch = 19,
  cex = 2,
  col = phase.colors[
    rank(theta %% (2*pi))
  ],
  xlab = "X",
  ylab = "Y",
  main = "Distribución espacial de fases"
)



# ===============================================================
# 14. INTERPRETACION ECOLOGICA
# ===============================================================
#
# R cercano a 0:
#
# -> sistema desordenado
# -> destellos no sincronizados
#
#
# R intermedio:
#
# -> sincronización parcial
# -> clusters locales
#
#
# R cercano a 1:
#
# -> sincronización global
# -> orden colectivo
#
#
# ===============================================================
# FACTORES ECOLOGICOS REPRESENTADOS
# ===============================================================
#
# DISTANCIA
# -> interacción espacial
#
# VEGETACION
# -> bloqueo de señales
#
# CONTAMINACION LUMINICA
# -> interferencia antropogénica
#
# DENSIDAD
# -> cantidad de vecinos
#
# RUIDO
# -> variabilidad ambiental
#
# ===============================================================
# EXPERIMENTOS POSIBLES
# ===============================================================
#
# 1. Aumentar contaminación lumínica
#
# 2. Fragmentar el paisaje
#
# 3. Reducir densidad poblacional
#
# 4. Incrementar ruido ambiental
#
# 5. Alterar conectividad
#
# 6. Crear corredores ecológicos
#
# 7. Simular urbanización
#
# ===============================================================
