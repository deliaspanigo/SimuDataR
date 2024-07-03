# Configuración inicial
#set.seed(123)  # Semilla para reproducibilidad

# Generar muestra inicial de una distribución normal
generador_base <- function(n_selected){

  n_selected <- 1000000
  vector_mod <- NA
  check_ok <- FALSE
  contador_externo <- 0

  while(!check_ok){


    contador_externo <- contador_externo + 1


    n_B <- n_selected
    n_A <- n_B - 1
    media_A <- 0  # Media de la distribución
    desvio_A <- 1  # Desviación estándar de la distribución
    datos_A <- rnorm(n_A, mean = media_A, sd = desvio_A)
    datos_A <- datos_A - mean(datos_A)
    vector_pos <- 1:(n_A-1)


    c_part01 <- n_B - 1
    c_part02 <- sum(datos_A^2)
    c_part03 <- ((sum(datos_A))^2)/n_B
    c_total <- -c_part01 + c_part02 - c_part03

    b_part01 <- (2*sum(datos_A))/n_B
    b_total <-  -b_part01

    a_part01 <- n_A/n_B
    a_total <- a_part01


    # Función para resolver una ecuación cuadrática
    resolver_parabola <- function(a, b, c) {
      # Calcular el discriminante
      discriminante <- b^2 - 4*a*c

      # Calcular las soluciones dependiendo del discriminante
      if (discriminante > 0) {
        x1 <- (-b + sqrt(discriminante)) / (2*a)
        x2 <- (-b - sqrt(discriminante)) / (2*a)
        cat("Las soluciones son x1 =", x1, "y x2 =", x2, "\n")
      } else if (discriminante == 0) {
        x <- -b / (2*a)
        cat("Hay una solución doble: x =", x, "\n")
      } else {
        parte_real <- -b / (2*a)
        parte_imaginaria <- sqrt(abs(discriminante)) / (2*a)
        cat("Las soluciones son complejas: x1 =", parte_real, "+", parte_imaginaria, "i y x2 =", parte_real, "-", parte_imaginaria, "i\n")
      }
      return(c(x1, x2))
    }

    # Ejemplo de uso
    a <- a_total
    b <- b_total
    c <- c_total

    discriminante <- -4*a*c
    check_soluciones_reales <-  discriminante >= 0

    print(paste0("a: ", a))
    print(paste0("b: ", b))
    print(paste0("c: ", c))
    print(paste0("soluciones reales: ", check_soluciones_reales))

    if(!check_soluciones_reales) next

    vector_soluciones <- resolver_parabola(a, b, c)

    vector_new_01 <- c(datos_A, vector_soluciones[1])
    vector_new_01 <- vector_new_01 - mean(vector_new_01)
    var_01 <- var(vector_new_01)
    range_01 <- max(vector_new_01) - min(vector_new_01)

    vector_new_02 <- c(datos_A, vector_soluciones[2])
    vector_new_02 <- vector_new_02 - mean(vector_new_02)
    var_02 <- var(vector_new_02)
    range_02 <- max(vector_new_02) - min(vector_new_02)

    vector_var <- c(var_01, var_02)
    print(vector_var)
    check_ok <- sum(unique(as.character(vector_var)) == "1") == 1
    print(check_ok)
    print("")
    selected_pos <- c()
    if(range_01 <= range_02) selected_pos <- 1 else selected_pos <- 2
    if(selected_pos == 1) vector_mod <- vector_new_01 else vector_mod <- vector_new_02

    print(var(vector_mod))
  }



  return(vector_mod)
}

vector_mod <- generador_base(n_selected = 100)

var_final  <- 16
sd_final <- sqrt(var_final)
mean_final <- 100
vector_final <- vector_mod*sd_final + mean_final

mean(vector_final)
var(vector_final)
