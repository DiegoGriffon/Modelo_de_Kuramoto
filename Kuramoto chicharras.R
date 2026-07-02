# ==========================================================
# SINCRONIZACIÓN DEL CANTO DE LA CHICHARRA (Quesada gigas)
# MODELO DE KURAMOTO CON FORZAMIENTO EXTERNO 
# ==========================================================

# Este código implementa una versión modelo de Kuramoto para   
# simular la sincronización del canto de la chicharra Quesada gigas.  

# Cada insecto se representa como un oscilador de fase que vibra 
# con una frecuencia natural ligeramente diferente. La interacción entre 
# individuos está mediada por un acoplamiento sinusoidal que tiende a 
# alinear sus fases. El parámetro R mide el grado de sincronización del 
# coro: valores cercanos a 1 indican que todos cantan al unísono, mientras 
# que valores cercanos a 0 indican desorden. Para replicar la sincronización 
# intermitente observada en la naturaleza, el código incluye un forzamiento 
# externo que se activa después de un cierto tiempo arbitrario, atrayendo 
# a cada chicharra hacia una fase objetivo diferente y rompiendo temporalmente 
# la cohesión del grupo. 

# La simulación genera gráficos de R contra el tiempo y de las fases finales, 
# permitiendo visualizar la dinámica de sincronización y desincronización del coro
# ==========================================================

# ==========================================================
# 1. PARAMETROS DEL MODELO
# ==========================================================

N <- 50
K <- 2
dt <- 0.05
Tmax <- 100
Pasos <- Tmax / dt

# Instante en el que comienza el forzamiento externo
T_forzamiento <- 1
paso_forzamiento <- T_forzamiento / dt

# Fuerza del forzamiento externo (gamma)
#
# gamma = 0.1  -> forzamiento débil, se mantiene sincronía
# gamma = 0.5  -> forzamiento intermedio
# gamma = 2.0  -> forzamiento fuerte, se pierde sincronía
#
gamma <- 1.9

# Gamma interesante: 1.9
# Produce oscilaciones sin un periodo evidente 

# ==========================================================
# 2. FRECUENCIAS NATURALES
# ==========================================================

set.seed(123)
omega <- rnorm(N, mean = 1, sd = 0.1)

# ==========================================================
# 3. FASES OBJETIVO (PHI_i)
# ==========================================================
#
# Cada luciérnaga tiene una fase objetivo diferente.
#
# Si las fases objetivo están distribuidas uniformemente
# alrededor del círculo, cada luciérnaga quiere estar
# en un lugar distinto.

set.seed(789)
phi <- runif(N, 0, 2*pi)

# ==========================================================
# 4. FASES INICIALES
# ==========================================================

set.seed(456)
Theta <- runif(N, 0, 2*pi)

# ==========================================================
# 5. ALMACENAMIENTO DE RESULTADOS
# ==========================================================

Theta.mat <- matrix(NA, nrow = Pasos, ncol = N)

# ==========================================================
# 6. SIMULACION DEL MODELO
# ==========================================================

for(t in 1:Pasos){
  
  Theta.new <- Theta
  
  for(i in 1:N){
    
    Acoplamiento <- sum(sin(Theta - Theta[i]))
    
    # ======================================================
    # TERMINO DE FORZAMIENTO EXTERNO
    # ======================================================
    #
    # Si t >= paso_forzamiento, la luciérnaga i es atraída
    # hacia su fase objetivo phi[i].
    #
    # El término es: gamma * sin(phi[i] - Theta[i])
    #
    # Si phi[i] está adelantada, acelera; si está atrasada, frena.
    #
    # Esto crea un conflicto: 
    # - El acoplamiento (K) quiere que todas vayan juntas.
    # - El forzamiento (gamma) quiere que cada una vaya a su sitio.
    
    if(t >= paso_forzamiento){
      forzamiento <- gamma * sin(phi[i] - Theta[i])
    } else {
      forzamiento <- 0
    }
    
    dTheta <- omega[i] + (K/N) * Acoplamiento + forzamiento
    
    Theta.new[i] <- Theta[i] + dTheta * dt
  }
  
  Theta <- Theta.new
  Theta.mat[t, ] <- Theta
}

# ==========================================================
# 7. PARAMETRO DE SINCRONIZACION
# ==========================================================

R <- numeric(Pasos)

for(t in 1:Pasos){
  z <- mean(exp(1i * Theta.mat[t, ]))
  R[t] <- Mod(z)
}

# ==========================================================
# 8. GRAFICO
# ==========================================================

par(mfrow = c(1, 1))

plot(
  seq(1, Tmax, length.out = Pasos),
  R,
  type = "l",
  lwd = 3,
  xlab = "Tiempo",
  ylab = "Sincronización (R)",
  main = paste("Kuramoto con forzamiento externo (gamma =", gamma, ")")
)

abline(v = T_forzamiento, lty = 2, col = "blue", lwd = 2)
abline(h = 1, lty = 2, col = "red")

# ==========================================================
# 9. FASES FINALES vs FASES OBJETIVO
# ==========================================================

# Este gráfico muestra cómo cada luciérnaga
# se ha desviado hacia su fase objetivo.

par(mfrow = c(1, 2))

# Fases finales (color azul) vs Fases objetivo (color rojo)
plot(
  cos(Theta),
  sin(Theta),
  xlim = c(-1,1),
  ylim = c(-1,1),
  asp = 1,
  pch = 19,
  col = "blue",
  xlab = "cos(Theta)",
  ylab = "sin(Theta)",
  main = paste("Fases finales (azul) y objetivo (rojo)")
)

points(cos(phi), sin(phi), col = "red", pch = 4, cex = 0.8)

# Añadimos círculo unitario
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lty = 2)

# Gráfico de R
plot(
  seq(1, Tmax, length.out = Pasos),
  R,
  type = "l",
  lwd = 3,
  xlab = "Tiempo",
  ylab = "Sincronización (R)",
  main = paste("R(t) con gamma =", gamma)
)

abline(v = T_forzamiento, lty = 2, col = "blue", lwd = 2)

# ==========================================================

# ==========================================================

# DESCRIPCIÓN:
  
# La chicharra Quesada gigas es una especie de cigarra emblemática
# de la ciudad de Caracas, Venezuela. Los machos adultos producen un canto
# ensordecedor que alcanza niveles de presión sonora entre 86 y 115 decibelios,
# convirtiéndola en una de las chicharras más ruidosas del mundo. Este potente
# zumbido es generado por la vibración rápida de los timbales, que son estructuras
# membranosas ubicadas en el abdomen del insecto. El sonido resulta de la
# deformación y recuperación elástica de estas membranas, accionadas por potentes
# músculos timbales que se contraen y relajan a frecuencias que pueden superar
# los 200 Hz.

# Una característica interesante del comportamiento acústico de Quesada gigas es la
# sincronización temporal de los cantos entre individuos cercanos. En ciertos
# momentos, múltiples machos emiten sus pulsos sonoros de manera casi simultánea,
# generando lo que se conoce como una "pared de sonido". Este fenómeno acústico
# crea una percepción auditiva de fuente sonora extendida que confunde a los
# depredadores y dificulta la localización de un insecto individual, ofreciendo
# una ventaja adaptativa al grupo. Sin embargo, esta sincronización no es
# permanente. El patrón acústico de Quesada gigas muestra ciclos en los que el
# canto sincronizado se mantiene durante breves intervalos, luego se desorganiza
# por un periodo, y posteriormente los individuos vuelven a sincronizarse. Este
# comportamiento de "sincronización intermitente" sugiere la presencia de
# mecanismos que promueven y desestabilizan la coordinación colectiva de manera
# alternante.

# El modelo de Kuramoto proporciona un marco matemático ideal para describir este
# fenómeno. Cada chicharra macho se representa como un oscilador de fase que
# gira en un ciclo de 0 a 2*pi radianes, donde una vuelta completa corresponde
# al ciclo completo de contracción y relajación del timbal. La fase theta_i(t)
# indica en qué punto del ciclo se encuentra el insecto i en el instante t.
# Cuando la fase es 0 o 2*pi, el insecto emite el pulso sonoro. La frecuencia
# natural omega_i representa la frecuencia de vibración del timbal cuando el
# insecto canta de manera aislada, sin interferencia de otros individuos. Estas
# frecuencias naturales varían ligeramente entre individuos debido a diferencias
# en tamaño, edad, temperatura corporal y otros factores fisiológicos y genéticos.

# La ecuación fundamental del modelo de Kuramoto para N chicharras es la siguiente:
  
# dtheta_i / dt = omega_i + (K / N) * sumatoria desde j=1 hasta N de seno(theta_j - theta_i)

# El primer término, omega_i, es la frecuencia natural del insecto i, que
# representa su ritmo intrínseco de vibración timbal. El segundo término es el
# acoplamiento social: cada insecto i escucha el canto de los demás individuos
# j y ajusta su propia fase en respuesta a las diferencias de fase. La constante
# K, conocida como fuerza de acoplamiento, cuantifica qué tan fuertemente los
# insectos influyen unos sobre otros. Cuando K es pequeña, cada chicharra canta a
# su propio ritmo y el coro suena desorganizado. Cuando K supera un valor crítico,
# las interacciones son lo suficientemente fuertes para vencer la heterogeneidad
# de las frecuencias naturales y los insectos comienzan a cantar al unísono.

# El parámetro de orden R se define como el módulo del vector promedio de todas
# las fases en el círculo complejo:
  
#  R * exp(i * Psi) = (1 / N) * sumatoria desde j=1 hasta N de exp(i * theta_j)

# R varía entre 0 y 1. Cuando R es cercano a 1, todas las chicharras tienen
# fases similares y cantan prácticamente al mismo tiempo, produciendo la pared
# de sonido. Cuando R es cercano a 0, las fases están distribuidas aleatoriamente
# y el canto es incoherente.  

# En el estado sincronizado, los insectos giran con una frecuencia común
# igual al promedio de omega_i, manteniendo diferencias de fase fijas 
# determinadas por la relación:
  
#  seno(Psi - theta_i) = (omega_i - omega_promedio) / (K * R)

# Esta ecuación muestra que solamente los insectos cuyas frecuencias naturales
# no difieren demasiado del promedio pueden permanecer sincronizados. Aquellos
# con frecuencias extremas nunca se sincronizan y cantan de manera independiente.

# Para simular el fenómeno de sincronización intermitente observado en Quesada
# gigas, el modelo debe incluir un mecanismo que desestabilice temporalmente la
# coordinación después de que la sincronización se ha establecido. Una forma de
# implementar esta dinámica es mediante la introducción de un forzamiento
# externo que compite con el acoplamiento natural entre los insectos. 

# Este forzamiento representa factores ambientales o internos que alteran el
# comportamiento de canto, tales como cambios en la temperatura, presencia de
# depredadores, competencia por parejas, un mecanismo endógeno de"cansancio"
# de los músculos timbales o El canto de otras chicharras lejanas que no están
# sincronizadas.

# La ecuación modificada que incorpora este forzamiento es la siguiente:
  
#  dtheta_i / dt = omega_i + (K / N) * sumatoria desde j=1 hasta N de seno(theta_j - theta_i) + gamma * seno(phi_i - theta_i)

# El nuevo término gamma * seno(phi_i - theta_i) representa una fuerza externa
# que atrae al insecto i hacia una fase objetivo phi_i. El parámetro gamma es
# la intensidad de esta fuerza. Cada insecto tiene su propio phi_i, que puede
# estar distribuido de manera uniforme alrededor del círculo. 

# Cuando gamma es pequeño comparado con el acoplamiento natural, el 
# grupo mantiene su cohesión.Sin embargo, cuando gamma supera un umbral 
# crítico, cada insecto comienza a seguir su propia fase objetivo, 
# abandonando el coro sincronizado.

# En el contexto del modelo, el forzamiento se activa en el instante T. 
# Esto representa el momento en que un factor desestabilizador comienza
# a operar. 

# A partir de ese instante,cada insecto es atraído hacia una fase diferente, 
# lo que provoca que los cantos se dispersen y el canto se vuelva incoherente, 
# replicando la pérdida de sincronización observada en la naturaleza. 

# Este modelo modificado captura la esencia del comportamiento acústico de
# Quesada gigas: la emergencia de una pared de sonido que confunde a los
# depredadores, la fragilidad de esta sincronización frente a perturbaciones,
# y la capacidad del sistema para reestablecer la coordinación cuando las
# condiciones lo permiten. La inclusión del forzamiento externo  que 
# tira de cada insecto hacia una fase diferente es una manera
# efectiva de simular la intermitencia característica del canto de esta
# especie, ofreciendo un modelo computacional que puede ser ajustado y
# explorado para comprender mejor los mecanismos subyacentes a este fenómeno.

# ==========================================================
# Antecedentes relevantes:

# Antonsen, T. M.  Faghih, R. T.  Girvan, M. Ott, E. J. Platig, J. 2008. External 
# periodic driving of large systems of globally coupled phase oscillators, Chaos 18, 037112.

# Childs, L. M., Strogatz, S. H. 2008. Stability diagram for the forced Kuramoto model. 
#Chaos: An Interdisciplinary Journal of Nonlinear Science, 18(4), 043128.

# Ott, E. Antonsen, T. M. 2008. Low dimensional behavior of large systems of globally coupled 
# oscillators, Chaos 18, 037113.

# Sakaguchi, H. 1988. Cooperative phenomena in coupled oscillator systems under external fields,
# Prog. Theor. Phys., 79, 39.

# ==========================================================
