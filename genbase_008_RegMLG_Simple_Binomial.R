#set.seed(4)
# Librerias
library("openxlsx")


gen_special_standard_008_A <- function(selected_n){

  x_simu <- rnorm(n = selected_n, mean = 0, sd = 1)  # Regresora 1 con media 40 y desviación estándar 4
  x_simu <- x_simu - mean(x_simu)
  return(x_simu)

}

gen_special_standard_008_B <- function(selected_n){

  # N final
  n_final <- selected_n

  # Muestreo sub
  n_sub <- n_final - 1
  media_sub <- 0  # Media de la distribución
  desvio_sub <- 1  # Desviación estándar de la distribución
  datos_sub <- rnorm(n_sub, mean = media_sub, sd = desvio_sub)
  datos_sub[datos_sub >  3] <-  3
  datos_sub[datos_sub < -3] <- -3

  datos_sub <- datos_sub - mean(datos_sub)



  c_part01 <- n_final
  c_part02 <- (n_final*(n_final-2))/(n_final-1)*var(datos_sub)
  c_total <- -c_part01 + c_part02

  b_total <-  0

  a_total <- 1


  # Función para resolver una ecuación cuadrática
  resolver_parabola <- function(a, b, c) {
    # Calcular el discriminante
    discriminante <- b^2 - 4*a*c

    # Calcular las soluciones dependiendo del discriminante
    if (discriminante > 0) {
      x1 <- (-b + sqrt(discriminante)) / (2*a)
      x2 <- (-b - sqrt(discriminante)) / (2*a)
      #cat("Las soluciones son x1 =", x1, "y x2 =", x2, "\n")
    } else if (discriminante == 0) {
      x <- -b / (2*a)
      #cat("Hay una solución doble: x =", x, "\n")
    } else {
      parte_real <- -b / (2*a)
      parte_imaginaria <- sqrt(abs(discriminante)) / (2*a)
      #cat("Las soluciones son complejas: x1 =", parte_real, "+", parte_imaginaria, "i y x2 =", parte_real, "-", parte_imaginaria, "i\n")
    }
    return(c(x1, x2))
  }

  # Ejemplo de uso
  a <- a_total
  b <- b_total
  c <- c_total

  discriminante <- -4*a*c
  check_soluciones_reales <-  discriminante >= 0

  #print(paste0("a: ", a))
  #print(paste0("b: ", b))
  #print(paste0("c: ", c))
  cat(paste0("soluciones reales: ", check_soluciones_reales))
  cat("\n")

  if(!check_soluciones_reales) {
    cat("Soluciones no reales!\n")
    cat("Intenta de nuevo!\n")
    return(NULL)

  }

  vector_soluciones <- resolver_parabola(a, b, c)

  vector_new_01 <- c(datos_sub, vector_soluciones[1])
  vector_new_01 <- vector_new_01 - mean(vector_new_01)
  var_01 <- var(vector_new_01)
  range_01 <- max(vector_new_01) - min(vector_new_01)

  vector_new_02 <- c(datos_sub, vector_soluciones[2])
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

  return(vector_mod)
  #print(var(vector_mod))
}


genbase_008 <- function(selected_n, value_mean_REG, value_sd_REG,
                          value_slope, value_intercept, n_dec = 2, the_way = 2){

  if(the_way == 1) x_standard <- gen_special_standard_008_A(selected_n = selected_n)

  if(the_way == 2) x_standard <- gen_special_standard_008_B(selected_n = selected_n)

  x_simu <- x_standard*value_sd_REG + value_mean_REG

  # Ajustar intercepto y pendiente para logit_p razonable
  value_intercept <- - value_slope * value_mean_REG  # Intercepto ajustado para centrar logit_p alrededor de 0

  # Calcular logit_p
  vector_logit_p <- value_intercept + value_slope * x_simu
  vector_prob <- exp(vector_logit_p) / (1 + exp(vector_logit_p))  # Transformación logit a probabilidad

  # Generar la variable de respuesta binaria
  y_simu <- rbinom(selected_n, size = 1, prob = vector_prob)

  # Crear un dataframe
  vector_orden <- 1:length(x_simu)
  df_simu <- data.frame(vector_orden, y_simu, x_simu)
  colnames(df_simu) <- c("orden", "VR", "X")


  return(df_simu)

}


gen_and_save_008 <- function(selected_n, value_mean_REG, value_sd_REG,
                             value_slope, value_intercept, n_dec = 2, the_way = 1){





    df_simu <- genbase_008(selected_n, value_mean_REG, value_sd_REG,
                            value_slope, value_intercept, n_dec, the_way)



    # Ajustar el modelo binomial
    list_model <- glm(VR ~ X, family = binomial, data = df_simu)
    df_simu$predicted_prob <- predict(list_model, type = "response")


    # Resumen del modelo
    list_summary <- summary(list_model)
    list_summary

    cat("\n")
    print(list_summary)
    cat("\n")

    df_coef <- as.data.frame(list_summary$coefficients)
    value_obs_slope     <- df_coef[,1][2]
    value_obs_intercept <- df_coef[,1][1]


    df_summary_x <- data.frame(
      "Details" = c("Slope", "mean_x", "sd_x"),
      "Expected" = c(value_slope, mean(df_simu$X), sd(df_simu$X)),
      "Observed" = c(value_obs_slope, mean(df_simu$X), sd(df_simu$X))
    )

    cat("\n")
    print(df_summary_x)
    cat("\n")


    # Guardar base
    the_date <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
    pre_file_name <- "df_simu_RegMLG_Simple_Binomial"
    new_file_name <- paste0(pre_file_name, the_date, ".xlsx")

    openxlsx::write.xlsx(x = df_simu, file = new_file_name)

    cat(paste0("Archivo ", new_file_name, " guardado!"))
    cat("\n")


    # Crear el gráfico
    library(ggplot2)
    ggplot(df_simu, aes(x = X)) +
      geom_point(aes(y = VR), alpha = 0.5) +
      geom_line(aes(y = predicted_prob), color = "blue", size = 1) +
      labs(title = "Modelo Binomial con una Regresora",
           x = "X1",
           y = "Probabilidad de éxito (Y)") +
      theme_minimal()

}


selected_n  <- 400
value_slope <- 1.6
value_mean_REG <- 140
value_sd_REG <- 4
n_dec <- 2
the_way = 2


gen_and_save_008(selected_n, value_mean_REG, value_sd_REG,
                             value_slope, value_intercept, n_dec, the_way)
