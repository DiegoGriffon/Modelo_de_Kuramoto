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
K <- 2

# Paso temporal de integración (dt)
#
# Representa el tamaño del intervalo de tiempo
# utilizado en la simulación numérica.
#
dt <- 0.05

# Tiempo total de simulación
Tmax <- 100

# Número total de pasos temporales
Pasos <- Tmax / dt



# ==========================================================
# 2. FRECUENCIAS NATURALES (omega, ω)
# ==========================================================

# Cada luciérnaga posee una frecuencia natural
# ligeramente distinta.
#
# Biológicamente esto representa variabilidad
# individual entre organismos. Lo representamos 
# con el parametro omega
#
# Es frecuencia natural del oscilador. Es decir, la frecuencia 
# natural es el ritmo propio con el que un proceso biológico 
# (en este caso una luciérnaga) oscila cuando no está influenciado  
# por otros componentes del sistema
#
# Fijamos una semilla para reproducibilidad
#
# Esto asegura que los resultados sean
# siempre los mismos al ejecutar el código.
#
set.seed(123)

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
# 3. FASES INICIALES (Theta, θ)
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
Theta <- runif(N, 0, 2*pi)



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
Theta.mat <- matrix(NA, nrow = Pasos, ncol = N)



# ==========================================================
# 5. SIMULACION DEL MODELO
# ==========================================================

# Aquí ocurre la dinámica principal.
#
# En cada paso temporal:
#
# 1. Cada luciérnaga observa las demás
# 2. Se calcula el efecto colectivo
# 3. Se ajusta ligeramente su fase
# 4. Se actualiza el sistema
#
for(t in 1:Pasos){
  
  # Creamos una copia temporal
  # para actualizar simultáneamente
  #
  Theta.new <- Theta
  
  
  # Recorremos cada luciérnaga
  #
  for(i in 1:N){
    
    # ======================================================
    # TERMINO DE ACOPLAMIENTO
    # ======================================================
    #
    # sum(sin(Theta - Theta[i]))
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
    Acoplamiento <- sum(sin(Theta - Theta[i]))
    
    
    # ======================================================
    # ECUACION DE KURAMOTO
    # ======================================================
    #
    # dTheta:
    # velocidad de cambio de la fase
    #
    # omega[i]
    # -> tendencia natural individual
    #
    # (K/N)*Acoplamiento
    # -> influencia social/colectiva
    #
    dTheta <- omega[i] + (K/N) * Acoplamiento
    
    
    # ======================================================
    # INTEGRACION NUMERICA
    # ======================================================
    #
    # Actualizamos la fase usando
    # el método de Euler.
    #
    Theta.new[i] <- Theta[i] + dTheta * dt
  }
  
  
  # Actualizamos todas las fases
  #
  Theta <- Theta.new
  
  
  # Guardamos resultados
  #
  Theta.mat[t, ] <- Theta
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
R <- numeric(Pasos)



# Calculamos R para cada instante temporal
#
for(t in 1:Pasos){
  
  # ========================================================
  # REPRESENTACION COMPLEJA
  # ========================================================
  #
  # exp(i*Theta)
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
  z <- mean(exp(1i * Theta.mat[t, ]))
  
  # Mod calcula el módulo
  # del número complejo.
  #
  R[t] <- Mod(z)
}



# ==========================================================
# 7. GRAFICO DE SINCRONIZACION
# ==========================================================

plot(
  seq(1, Tmax, length.out = Pasos),
  R,
  type = "l",
  lwd = 3,
  xlab = "Tiempo",
  ylab = "Sincronización (R)",
  main = "Modelo de Kuramoto - Luciérnagas"
)

# Línea de referencia
#
abline(h = 1, lty = 2, col = "red")



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
  cos(Theta),
  sin(Theta),
  xlim = c(-1,1),
  ylim = c(-1,1),
  asp = 1,
  pch = 19,
  xlab = "cos(Theta)",
  ylab = "sin(Theta)",
  main = "Fases finales de las luciérnagas"
)


# ==========================================================
# Un buen video de introducción al modelo básico:
# https://www.youtube.com/watch?v=ywd7jVIubOo
# ==========================================================
