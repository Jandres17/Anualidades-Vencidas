# Se presentan las funciones de interés simple
# Autor: Juan Andrés Pérez Raya
# V 1.0: 24 de octubre de 2024

# Valor futuro, conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

VFuturo = function(A, r, Tper){
  VF = A*((((1+r)^Tper)-1)/r)
  return(VF)
}

# Anualidad, conociendo valor futuro, tasa del periodo y número de pagos.

AFuturo = function(VF, r, Tper){
  A = (VF)/((((1+r)^Tper)-1)/r)
  return(A)
}

# Número de pagos o plazo, conociendo valor futuro, número de pagos y tasa del periodo.

Npagos = function(VF, A, r){
  Tper = log10(((VF*r)/A)+1)/log10(1+r)
  return(Tper)
}

# Tasa del periodo, conociendo valor futuro, número de pagos y monto de la anualidad.

tasaper = function(FV, A, Tper) {
  func = function(r) {
    (FV-A)*((((1+r)^Tper)-1)/r)
  }
  
  lower = 0.0001  
  upper = 1
  f.lower = func(lower)
  f.upper = func(upper)
  
  if (is.na(f.lower) || is.na(f.upper)) {
    stop("No se puede calcular en el intervalo dado.")
  }
  
  if (f.lower * f.upper > 0) {
    stop("La función no cambia de signo en el intervalo dado.")
  }
  
  r = uniroot(func, c(lower, upper))$root
  return(r)
}

# Valor actual, conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

Vactual = function(A, i, Tper) {
  VA = A*((1-(1 + r)^(-Tper))/r)
  return(VA)
}

# Anualidad, conociendo valor actual, tasa del periodo y número de pagos.

A_actual = function(VA, r, Tper) {
  A = VA/((1-(1+r)^(-Tper))/r)
  return(A)
}

# Número de pagos o plazo, conociendo valor actual, número de pagos y tasa del periodo.

Npagos_actual = function(VA, A, r) {
  n = -log10(1-((VA*r)/A))/log10(1+r)
  return(n)
}

# Tasa del periodo, conociendo valor actual, número de pagos y monto de la anualidad.

# Función auxiliar para expandir el intervalo de búsqueda
expandir_intervalo <- function(func, lower, upper, step = 0.1, max_iter = 10) {
  for (i in 1:max_iter) {
    f.lower <- func(lower)
    f.upper <- func(upper)
    
    if (!is.na(f.lower) && !is.na(f.upper) && f.lower * f.upper <= 0) {
      return(c(lower, upper))  # Si hay cambio de signo, devuelve el intervalo
    }
    
    # Expande el intervalo
    lower <- lower - step
    upper <- upper + step
  }
  stop("No se encontró un intervalo válido para el cambio de signo.")
}


tasa_actual <- function(VA, PMT, Tper) {
  func <- function(r) {
    VA - PMT * ((1 - (1 + r)^(-Tper)) / r)
  }
  
  # Establecer un intervalo inicial
  lower <- 0.0001
  upper <- 1
  
  # Intentar expandir el intervalo si no hay cambio de signo
  intervalo <- expandir_intervalo(func, lower, upper)
  
  # Resolver usando uniroot con el intervalo ajustado
  r <- uniroot(func, intervalo)$root
  return(r)
}

