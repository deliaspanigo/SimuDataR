

# Libreries
library("openxlsx")


gen_special_standard_002 <- function(selected_n){

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



  c_part01 <- n_sub
  c_part02 <- sum(datos_sub^2)
  c_part03 <- ((sum(datos_sub))^2)/n_final
  c_total <- -c_part01 + c_part02 - c_part03

  b_part01 <- (2*sum(datos_sub))/n_sub
  b_total <-  -b_part01

  a_part01 <- n_sub/n_final
  a_total <- a_part01


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


gen_special_standard_002_B <- function(selected_n){

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

genbase_002_A <- function(vector_n, vector_mean_VR_factor, vector_sd_ERROR_factor, n_dec = 2){


  vector_orden_levels <- 1:length(vector_n)
  vector_ref_levels <- LETTERS[vector_orden_levels]

  list_data <- sapply(vector_orden_levels, function(x){
    simu_g <- rnorm(n = vector_n[x], mean = vector_mean_VR_factor[x], sd = vector_sd_ERROR_factor[x])
    simu_g <  round(simu_g, n_dec)
    simu_g <- simu_g - mean(simu_g)
    simu_g <- simu_g + vector_mean_VR_factor[x]
    simu_g
  }, simplify = F)

  VR <- unlist(list_data)
  FACTOR <- rep(vector_ref_levels, vector_n)
  FACTOR <- as.factor(FACTOR)

  vector_orden_df <- 1:length(VR)

  df_simu <- cbind.data.frame(vector_orden_df, VR, FACTOR)
  colnames(df_simu) <- c("orden", "VR", "FACTOR")

  return(df_simu)

}



genbase_002_B <- function(vector_n, vector_mean_VR_factor, vector_sd_ERROR_factor, n_dec = 2){


  vector_orden_levels <- 1:length(vector_n)
  vector_ref_levels <- LETTERS[vector_orden_levels]

  list_data <- sapply(vector_orden_levels, function(x){

    super_ok <- FALSE
    while(!super_ok){
      simu_g <- gen_special_standard_002_B(selected_n = vector_n[x])


      if (length(simu_g) == vector_n[x]) {
        simu_g <- round(simu_g, n_dec)
        simu_g <- simu_g*vector_sd_ERROR_factor[x] + vector_mean_VR_factor[x]
        super_ok <- TRUE
      }
    }
    simu_g
  }, simplify = F)

  VR <- unlist(list_data)
  FACTOR <- rep(vector_ref_levels, vector_n)
  FACTOR <- as.factor(FACTOR)

  vector_orden_df <- 1:length(VR)

  df_simu <- cbind.data.frame(vector_orden_df, VR, FACTOR)
  colnames(df_simu) <- c("orden", "VR", "FACTOR")

  return(df_simu)

}


control_002 <- function(df_simu, alpha_value = 0.05){

  VR <- df_simu[,"VR"]
  FACTOR <- df_simu[,"FACTOR"]

  list_anova <- lm(VR ~ FACTOR)
  residuals  <- list_anova$residuals

  list_residuals_normality <- shapiro.test(residuals)
  list_residuals_homogeneity <- bartlett.test(residuals ~ FACTOR)

  vector_test <- c("Residuals Normality", "Residuals Homogeneity")

  vector_p_control <- c(list_residuals_normality$p.value,
                        list_residuals_homogeneity$p.value)

  vector_check_H0 <- vector_p_control >= alpha_value
  phrase_check_T <- "H0 Not Rejected"
  phrase_check_F <- "H0 Rejected"

  vector_decision <- ifelse(test = vector_check_H0,
                            yes = phrase_check_T,
                            no = phrase_check_F)


  df_control <- data.frame(
    "test" = vector_test,
    "p_value" = vector_p_control,
    "alpha_value" = alpha_value,
    "decision" = vector_decision,
    "vector_check" = vector_check_H0
  )

  return(df_control)
}

summary_002 <- function(df_simu){

  VR <- df_simu[,"VR"]
  FACTOR <- df_simu[,"FACTOR"]

  vector_orden_levels <- 1:nlevels(FACTOR)
  vector_ref_levels   <- levels(FACTOR)

  df_summary <- data.frame(
    "orden"    = vector_orden_levels,
    "level"    = vector_ref_levels,
    "n"        = tapply(VR, FACTOR, length),
    "mean"     = tapply(VR, FACTOR, mean),
    "variance" = tapply(VR, FACTOR, var)
  )

  return(df_summary)
}

general_vision_002 <- function(df_control){


  vector_check <- df_control$vector_check

  check_control_general <- sum(vector_check) == length(vector_check)


  phrase_T <- "All it's OK!!!!"
  phrase_F <- "Problems! Try again!"

  phrase_control_general <- ifelse(test = check_control_general,
                                   yes  = phrase_T,
                                   no   = phrase_F)

  cat("\n")
  cat(phrase_control_general)
  cat("\n")

  output_list <- list(check_control_general, phrase_control_general)
  names(output_list) <- c("check_control_general", "phrase_control_general")

  output_list
}

gen_and_save_002 <- function(vector_n, vector_mean_VR_factor, vector_sd_ERROR_factor, n_dec, the_way = 1){


  # Base simulada
  if(the_way == 1) df_simu <- genbase_002_A(vector_n, vector_mean_VR_factor, vector_sd_ERROR_factor, n_dec)
  if(the_way == 2) df_simu <- genbase_002_B(vector_n, vector_mean_VR_factor, vector_sd_ERROR_factor, n_dec)


  # DF del control
  df_control <- control_002(df_simu, alpha_value)
  cat("\n")
  print(df_control)
  cat("\n")

  # Medidas resumen
  df_summary <- summary_002(df_simu)
  cat("\n")
  print(df_summary)
  cat("\n")

  # Control
  list_control <- general_vision_002(df_control)

  # Check general
  check_ok <- list_control$check_control_general

  if(!check_ok) return(NULL)

  # Guardar base
  the_date <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  pre_file_name <- "df_simu_anova_1_way"
  new_file_name <- paste0(pre_file_name, the_date, ".xlsx")

  openxlsx::write.xlsx(x = df_simu, file = new_file_name)

  cat(paste0("Archivo ", new_file_name, " guardado!"))
  cat("\n")
}

######################################################################

# Generador!

# Inputs
vector_n        <- c(10, 10, 15)
vector_mean_VR_factor     <- c(20, 30, 42)
vector_sd_ERROR_factor       <- c(2, 3, 4)
alpha_value     <- 0.05
n_dec <- 4

gen_and_save_002(vector_n, vector_mean_VR_factor, vector_sd_ERROR_factor, n_dec, the_way = 2)



