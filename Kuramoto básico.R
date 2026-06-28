# ==========================================================
# MODELO DE KURAMOTO EN R
# Sincronización de luciérnagas
# ==========================================================
#
# Este código implementa una versión básica del
# modelo de Kuramoto para estudiar cómo un grupo
# de luciérnagas puede sincronizar sus destellos.
#
# ----------------------------------------------------------
# IDEA BIOLOGICA
# ----------------------------------------------------------
#
# Cada luciérnaga:
#
# - posee un "reloj interno"
# - destella periódicamente
# - tiene una frecuencia natural propia
# - observa los destellos de otras luciérnagas
# - ajusta ligeramente el tiempo de su próximo destello
#
# Aunque inicialmente cada individuo destella de forma
# independiente, la interacción colectiva puede generar
# sincronización espontánea.
#
# ==========================================================



# ==========================================================
# 1. PARAMETROS DEL MODELO
# ==========================================================

# Número total de luciérnagas (N)
N <- 50

# Fuerza de acoplamiento entre individuos (K)
#
# Este parámetro controla cuánto influye
# una luciérnaga sobre otra.
#
# Valores pequeños:
# -> poca interacción
# -> comportamiento desordenado
#
# Valores grandes:
# -> fuerte influencia mutua
# -> sincronización colectiva
#
K <- 0.03

# Paso temporal de integración (dt)
#
# Representa el tamaño del intervalo de tiempo
# utilizado en la simulación numérica.
#
dt <- 0.05

# Tiempo total de simulación (Tmax)
Tmax <- 100

# Número total de pasos temporales (steps)
steps <- Tmax / dt



# ==========================================================
# 2. FRECUENCIAS NATURALES (omega)
# ==========================================================

# Se fija una semilla para reproducibilidad
#
# Esto asegura que los resultados sean
# siempre los mismos al ejecutar el código.
#
set.seed(123)

# Cada luciérnaga posee una frecuencia natural
# ligeramente distinta.
#
# Biológicamente esto representa variabilidad
# individual entre organismos.
#
# rnorm genera valores aleatorios siguiendo
# una distribución normal.
#
# mean = 1
# -> frecuencia promedio
#
# sd = 0.1
# -> variabilidad entre individuos
#
omega <- rnorm(N, mean = 1, sd = 0.1)



# ==========================================================
# 3. FASES INICIALES (theta)
# ==========================================================

# La fase representa la posición de cada
# luciérnaga dentro de su ciclo de destello.
#
# Una fase de:
#
# 0       -> inicio del ciclo
# pi      -> mitad del ciclo
# 2*pi    -> final del ciclo
#
# Inicialmente las fases son aleatorias.
#
theta <- runif(N, 0, 2*pi)



# ==========================================================
# 4. ALMACENAMIENTO DE RESULTADOS
# ==========================================================

# Creamos una matriz para guardar la evolución
# temporal de las fases.
#
# Filas:
# -> tiempo
#
# Columnas:
# -> individuos
#
theta.mat <- matrix(NA, nrow = steps, ncol = N)



# ==========================================================
# 5. SIMULACION DEL MODELO
# ==========================================================

# Dinámica principal del modelo.
#
# En cada paso temporal:
#
# 1. Cada luciérnaga observa las demás
# 2. Se calcula el efecto colectivo
# 3. Se ajusta ligeramente su fase
# 4. Se actualiza el sistema
#
for(t in 1:steps){
  
  # Se crea una copia temporal
  # para actualizar simultáneamente
  #
  theta.new <- theta
  
  
  # Se recorre cada luciérnaga
  #
  for(i in 1:N){
    
    # ======================================================
    # TERMINO DE ACOPLAMIENTO
    # ======================================================
    #
    # sum(sin(theta - theta[i]))
    #
    # Calcula la influencia colectiva
    # de todas las demás luciérnagas.
    #
    # Si otras luciérnagas están adelantadas
    # en fase:
    # -> empujan a la luciérnaga i hacia adelante
    #
    # Si están atrasadas:
    # -> la retrasan
    #
    coupling <- sum(sin(theta - theta[i]))
    
    
    # ======================================================
    # ECUACION DE KURAMOTO
    # ======================================================
    #
    # dtheta:
    # velocidad de cambio de la fase
    #
    # omega[i]
    # -> tendencia natural individual
    #
    # (K/N)*coupling
    # -> influencia social/colectiva
    #
    dtheta <- omega[i] + (K/N) * coupling
    
    
    # ======================================================
    # INTEGRACION NUMERICA
    # ======================================================
    #
    # Se actualiza la fase usando
    # el método de Euler.
    #
    theta.new[i] <- theta[i] + dtheta * dt
  }
  
  
  # Actualizamos todas las fases
  #
  theta <- theta.new
  
  
  # Guardamos resultados
  #
  theta.mat[t, ] <- theta
}



# ==========================================================
# 6. PARAMETRO DE SINCRONIZACION
# ==========================================================

# El parámetro R mide el grado
# de sincronización del sistema.
#
# R cercano a 0:
# -> desorden
#
# R cercano a 1:
# -> sincronización completa
#
R <- numeric(steps)



# Calculamos R para cada instante temporal
#
for(t in 1:steps){
  
  # ========================================================
  # REPRESENTACION COMPLEJA
  # ========================================================
  #
  # exp(i*theta)
  #
  # Convierte cada fase angular
  # en un punto del círculo unitario.
  #
  # Cuando las fases están dispersas:
  # -> los vectores se cancelan
  # -> R pequeño
  #
  # Cuando las fases están alineadas:
  # -> los vectores se refuerzan
  # -> R grande
  #
  z <- mean(exp(1i * theta.mat[t, ]))
  
  # Mod calcula el módulo
  # del número complejo.
  #
  R[t] <- Mod(z)
}



# ==========================================================
# 7. GRAFICO DE SINCRONIZACION
# ==========================================================

plot(
  seq(1, Tmax, length.out = steps),
  R,
  type = "l",
  lwd = 3,
  xlab = "Tiempo",
  ylab = "Sincronización (R)",
  main = "Modelo de Kuramoto - Luciérnagas"
)

# Línea de referencia
#
abline(h = 1, lty = 2)



# ==========================================================
# INTERPRETACION DEL RESULTADO
# ==========================================================
#
# Al inicio:
#
# - las fases son aleatorias
# - R es pequeño
# - los destellos están desordenados
#
# Con el tiempo:
#
# - las luciérnagas comienzan a ajustarse
# - emerge coordinación colectiva
# - R aumenta
#
# Finalmente:
#
# - muchas luciérnagas destellan juntas
# - R se aproxima a 1
#
# ==========================================================



# ==========================================================
# 8. VISUALIZACION DE LAS FASES
# ==========================================================
#
# Esta gráfica representa las fases
# como puntos sobre un círculo.
#
# Cuando hay desorden:
# -> puntos dispersos
#
# Cuando hay sincronización:
# -> puntos agrupados
#
plot(
  cos(theta),
  sin(theta),
  xlim = c(-1,1),
  ylim = c(-1,1),
  asp = 1,
  pch = 19,
  xlab = "cos(theta)",
  ylab = "sin(theta)",
  main = "Fases finales de las luciérnagas"
)


# ==========================================================
