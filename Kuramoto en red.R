# ==========================================================
# MODELO DE KURAMOTO EN RED
# Sincronización de luciérnagas sobre una red 
# ==========================================================
#
# Este código implementa una versión del
# modelo de Kuramoto donde las luciérnagas
# interactúan a través de una red.
#
# Cada conexión posee un peso que representa
# la intensidad de interacción entre dos
# individuos.
#
# ==========================================================

# Instalar librería: igraph

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
# individual entre organismos.
#
set.seed(123)

omega <- rnorm(N, mean = 1, sd = 0.1)



# ==========================================================
# 2.5. ESTRUCTURA DE LA RED
# ==========================================================
#
# En el modelo clásico todas las luciérnagas
# interactúan con todas las demás.
#
# Aquí se incorpora una red de interacción.
#
# Cada elemento de la matriz A representa
# la intensidad de interacción entre dos
# luciérnagas.
#
# A[i,j] = 0
# -> no existe conexión
#
# A[i,j] > 0
# -> existe conexión
#
# Valores grandes indican una influencia
# más fuerte entre ambos individuos.
#


# Probabilidad de que exista una conexión
#
p <- 0.20


# Creamos una matriz inicialmente vacía
#
A <- matrix(0, N, N)


# Construimos una red no dirigida
#
for(i in 1:(N-1)){
  
  for(j in (i+1):N){
    
    # Decidimos aleatoriamente
    # si existe conexión
    #
    if(runif(1) < p){
      
      # Peso de la conexión
      #
      # Los pesos pueden interpretarse
      # como intensidad visual,
      # proximidad espacial,
      # calidad del hábitat,
      # etc.
      #
      peso <- runif(1, 0.3, 1)
      
      A[i,j] <- peso
      A[j,i] <- peso
      
    }
    
  }
  
}


# Se Eliminan las autoconexiones
#
diag(A) <- 0



# ==========================================================
# GRADO PONDERADO
# ==========================================================
#
# El grado ponderado representa
# la suma de todas las conexiones
# de una luciérnaga.
#
# Cuanto mayor sea este valor,
# mayor será el número e intensidad
# de vecinos con los que interactúa.
#
Grado <- rowSums(A)


# Evitamos divisiones entre cero
#
Grado[Grado == 0] <- 1



# ==========================================================
# 3. FASES INICIALES (Theta, θ)
# ==========================================================

Theta <- runif(N, 0, 2*pi)



# ==========================================================
# 4. ALMACENAMIENTO DE RESULTADOS
# ==========================================================

Theta.mat <- matrix(NA, nrow = Pasos, ncol = N)



# ==========================================================
# 5. SIMULACION DEL MODELO
# ==========================================================

for(t in 1:Pasos){
  
  # Copia temporal
  #
  Theta.new <- Theta
  
  
  # Recorremos cada luciérnaga
  #
  for(i in 1:N){
    
    
    # ======================================================
    # TERMINO DE ACOPLAMIENTO
    # ======================================================
    #
    # Cada luciérnaga solamente
    # recibe influencia de sus vecinos.
    #
    # La matriz de adyacencia A determina
    # quién interactúa con quién.
    #
    # Además, cada conexión posee un peso
    # diferente.
    #
    Acoplamiento <-
      sum(
        A[i, ] *
          sin(Theta - Theta[i])
      )
    
    
    # ======================================================
    # ECUACION DE KURAMOTO EN RED
    # ======================================================
    #
    # omega[i]
    # -> frecuencia natural
    #
    # (K/Grado[i])
    # -> intensidad del acoplamiento
    #
    # Acoplamiento
    # -> influencia de los vecinos
    #
    # La normalización por el grado
    # evita que los nodos con muchos
    # vecinos reciban automáticamente
    # una influencia excesiva.
    #
    dTheta <- omega[i] +
      (K/Grado[i]) * Acoplamiento
    
    
    # ======================================================
    # INTEGRACION NUMERICA
    # ======================================================
    #
    # Método de Euler
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

R <- numeric(Pasos)

for(t in 1:Pasos){
  
  # Representación compleja
  #
  z <- mean(exp(1i * Theta.mat[t, ]))
  
  # Magnitud del parámetro de orden
  #
  R[t] <- Mod(z)
  
}



# ==========================================================
# 7. GRAFICO DE SINCRONIZACION
# ==========================================================

par(mfrow = c(1, 1))

plot(
  seq(1, Tmax, length.out = Pasos),
  R,
  type = "l",
  lwd = 3,
  xlab = "Tiempo",
  ylab = "Sincronización (R)",
  main = "Modelo de Kuramoto sobre una red"
)

abline(h = 1, lty = 2, col = "red")



# ==========================================================
# INTERPRETACION DEL RESULTADO
# ==========================================================
#
# Al inicio:
#
# - las fases son aleatorias
# - R es pequeño
#
# Con el tiempo:
#
# - cada luciérnaga ajusta su fase
# únicamente con respecto a sus
# vecinos dentro de la red.
#
# Dependiendo de la estructura
# de la red y de la intensidad
# de las conexiones, puede emerger
# sincronización colectiva.
#
# Si la red está muy fragmentada,
# pueden aparecer grupos que se
# sincronizan entre sí sin alcanzar
# sincronización global.
#
# ==========================================================



# ==========================================================
# 8. VISUALIZACION DE LAS FASES
# ==========================================================

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
# 9. VISUALIZACION DE LA RED
# ==========================================================
#
# Esta figura muestra la estructura
# de la red utilizada en la simulación.
#
# Cada nodo representa una luciérnaga.
#
# Cada arista representa una interacción.
#
# El grosor de las líneas depende
# del peso de la conexión.
#
# Requiere el paquete igraph.
# ==========================================================



library(igraph)

g <- graph_from_adjacency_matrix(
  A,
  mode = "undirected",
  weighted = TRUE
)

plot(
  g,
  vertex.size = 8,
  vertex.color = "#C0FF3E",
  vertex.label = NA,
  edge.width = E(g)$weight * 3,
  layout = layout_with_fr(g),
  main = "Red de interacción entre luciérnagas"
)

