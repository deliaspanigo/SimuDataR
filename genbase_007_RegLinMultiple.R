rm(list = ls())
library(MASS)
#Sys.sleep(1)


gen_special_standard_007_B <- function(selected_n){

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

# Generar regresoras
#set.seed(123)

genbase_007 <- function(value_n, value_amount_regs, vector_slopes,
                        vector_mean_REG, vector_sd_REG, value_adj_R2,
                        corr_matrix, vector_significative, n_dec = 2){

  matrix_X_standard <- MASS::mvrnorm(n = value_n,
                                     mu = rep(0, value_amount_regs),
                                     Sigma = corr_matrix)


  matrix_X_mod <- matrix_X_standard
  matrix_X_mod <- round(matrix_X_mod, n_dec)
  matrix_X_mod <- apply(matrix_X_mod, 2, function(x){x - mean(x)})

  matrix_X_mod <- sapply(1:ncol(matrix_X_mod), function(x){
    mod_reg <- matrix_X_mod[,x]
    mod_reg <- mod_reg*vector_sd_REG[x] + vector_mean_REG[x]
    mod_reg
  })

  Y_sin_error <- matrix_X_mod %*% (vector_slopes * vector_significative)
  value_var_Y_sin_error <- var(Y_sin_error)
  value_SCR <- value_var_Y_sin_error*(length(Y_sin_error)-1)

  # Calcular R2 a partir de R2 ajustado
  value_numerador_A   <- 1-value_adj_R2
  value_numerador_B   <- (value_n - value_amount_regs - 1)
  value_numerador_total <- value_numerador_A*value_numerador_B

  value_denominador <- (value_n - 1)
  value_cociente    <-  value_numerador_total/value_denominador

  value_R2 <- 1 - value_cociente

  value_SCT <- value_SCR/value_R2
  value_SCE <- value_SCT - value_SCR
  value_CME <- value_SCE/(value_n-1)
  value_sd_ERROR <- as.vector(sqrt(value_CME))

  #print(value_sd_ERROR)
  # Generar la variable dependiente con error
  # Desviación estándar del error
  vector_residuals <- gen_special_standard_007_B(selected_n = value_n)
  vector_residuals <- vector_residuals - mean(vector_residuals)
  vector_residuals <- vector_residuals*value_sd_ERROR

  Y_simu <- Y_sin_error + vector_residuals
  Y_simu <- round(Y_simu, n_dec)

  # Crear un dataframe
  df_simu <- data.frame(Y_simu, matrix_X_mod)
  colnames(df_simu) <- c("VR", paste0("X", 1:ncol(matrix_X_mod)))

  return(df_simu)
}


analisis_007 <- function(df_simu){

  # Ajustar el modelo de regresión lineal múltiple
  list_model <- lm(VR ~ ., data = df_simu)

  # Resumen del modelo
  list_summary <- summary(list_model)

  list_output <- list(list_model, list_summary)
  names(list_output) <- c("list_model", "list_summary")

  return(list_output)

}

control_007 <- function(value_n, value_amount_regs, vector_slopes,
                        vector_mean_REG, vector_sd_REG, value_adj_R2,
                        corr_matrix, vector_significative, n_dec, alpha_value = 0.05){


  df_simu <- genbase_007(value_n, value_amount_regs, vector_slopes,
                          vector_mean_REG, vector_sd_REG, value_adj_R2,
                          corr_matrix, vector_significative)

  vector_pos_reg <- 2:ncol(df_simu)

  list_analisis <- analisis_007(df_simu)

  vector_obs_slopes <- list_analisis$list_summary$coefficients[,1]
  vector_obs_slopes <- vector_obs_slopes[2:length(vector_obs_slopes)]
  value_obs_r2_ajust <- list_analisis$list_summary$adj.r.squared

  # Check normality
  residuals <- list_analisis$list_model$residuals
  list_normality <- shapiro.test(residuals)
  check_normality <- list_normality$p.value >= alpha_value

  df_normality <- data.frame(
    "test" = "normality_residuals",
    "p_value" = list_normality$p.value,
    "alpha_value" = alpha_value,
    "check_normality" = check_normality
  )

  print("")
  print(df_normality)
  print("")


  df_means_reg <- data.frame(
    "Details" = "means_REG",
    "Spected" = vector_mean_REG,
    "Observed" = apply(df_simu[,vector_pos_reg], 2, mean)
  )
  print("")
  print(df_means_reg)
  print("")


  df_sd_reg <- data.frame(
    "Details" = "sd_REG",
    "Spected" = vector_sd_REG,
    "Observed" = apply(df_simu[,vector_pos_reg], 2, sd)
  )
  print("")
  print(df_sd_reg)
  print("")

  df_slopes_reg <- data.frame(
    "Details" = "slopes_REG",
    "Spected" = vector_slopes,
    "Observed" = vector_obs_slopes
  )
  print("")
  print(df_slopes_reg)
  print("")


  df_R2Ajust_reg <- data.frame(
    "Details" = "R2_Ajust",
    "Spected" = value_adj_R2,
    "Observed" = value_obs_r2_ajust
  )
  print("")
  print(df_R2Ajust_reg)
  print("")


  list_output <- list(df_normality, df_means_reg, df_sd_reg,
                      df_slopes_reg, df_R2Ajust_reg)

  names(list_output) <- c("df_normality", "df_means_reg", "df_sd_reg",
                          "df_slopes_reg", "df_R2Ajust_reg")

  return(list_output)

}



gen_and_save_007 <- function(value_n, value_amount_regs, vector_slopes,
                             vector_mean_REG, vector_sd_REG, value_adj_R2,
                             corr_matrix, vector_significative, n_dec, alpha_value){



  df_simu <- genbase_007(value_n, value_amount_regs, vector_slopes,
                         vector_mean_REG, vector_sd_REG, value_adj_R2,
                         corr_matrix, vector_significative, n_dec)


  list_control <- control_007(value_n, value_amount_regs, vector_slopes,
                              vector_mean_REG, vector_sd_REG, value_adj_R2,
                              corr_matrix, vector_significative, n_dec, alpha_value)



  # Guardar base
  the_date <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  pre_file_name <- "df_simu_RLM_"
  new_file_name <- paste0(pre_file_name, the_date, ".xlsx")

  openxlsx::write.xlsx(x = df_simu, file = new_file_name)

  cat(paste0("Archivo ", new_file_name, " guardado!"))
  cat("\n")

}



# INPUTS!
# Son muchos!

  # Parámetros
  value_n <- 100  # Número de observaciones
  value_amount_regs <- 3  # Número de regresoras
  vector_slopes <- c(2, -1, 3)  # Pendientes
  vector_mean_REG <- c(10, 30, 50)
  vector_sd_REG <- c(2, 3, 4)
  value_adj_R2 <- 0.90
  n_dec <- 2
  alpha_value <- 0.05

  corr_matrix <- matrix(c(1, 0,   0,
                          0, 1  , 0,
                          0, 0  , 1  ), nrow = value_amount_regs)  # Matriz de correlación


  vector_significative <- c(TRUE, TRUE, TRUE)  # Significancia de cada regresora


# Generamos y guardamos!
gen_and_save_007(value_n, value_amount_regs, vector_slopes,
                             vector_mean_REG, vector_sd_REG, value_adj_R2,
                             corr_matrix, vector_significative, n_dec, alpha_value)

