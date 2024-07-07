
#set.seed(005)
# Libreries
library("openxlsx")


gen_special_standard_004_A <- function(selected_n){

  # N final
  n_final <- selected_n

  # Muestreo sub
  n_sub <- n_final - 1
  media_sub <- 0  # Media de la distribución
  desvio_sub <- 1  # Desviación estándar de la distribución
  datos_sub <- rnorm(n_sub, mean = media_sub, sd = desvio_sub)
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


gen_special_standard_004_B <- function(selected_n){

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


genbase_004_A <- function(vector_n, vector_mean_VR_factor, vector_mean_COV_factor,
                          vector_sd_COV, vector_sd_ERROR_factor, vector_slope_factor, n_dec = 2){


  vector_orden_levels <- 1:length(vector_n)
  vector_ref_levels <- LETTERS[vector_orden_levels]

  list_data <- sapply(vector_orden_levels, function(x){
    simu_g_cov <- rnorm(n = vector_n[x], mean = 0, sd = 1)
    simu_g_cov <- round(x = simu_g_cov, digits = 2)
    simu_g_cov <- simu_g_cov - mean(simu_g_cov)
    simu_g_cov <- simu_g_cov*vector_sd_COV[x] + vector_mean_COV_factor[x]

    simu_g_error <- rnorm(n = vector_n[x], mean = 0, sd = 1)
    simu_g_error <- round(simu_g_error, n_dec)
    simu_g_error <- simu_g_error - mean(simu_g_error)
    simu_g_error <- simu_g_error*vector_sd_ERROR_factor[x] + 0

    simu_g_vr <- vector_mean_VR_factor[x] + vector_slope_factor[x]*(simu_g_cov - mean(simu_g_cov)) + simu_g_error

    simu_g_factor <- rep(LETTERS[x], vector_n[x])
    data_g <- cbind.data.frame(simu_g_vr, simu_g_cov, simu_g_factor)
    data_g
  }, simplify = F)

  df_simu <- do.call(rbind.data.frame, list_data)
  vector_orden_df <- 1:nrow(df_simu)
  df_simu <- cbind.data.frame(vector_orden_df, df_simu)
  colnames(df_simu) <- c("orden", "VR", "COV", "FACTOR")
  df_simu[,"FACTOR"] <- as.factor(df_simu[,"FACTOR"])

  return(df_simu)

}



genbase_004_B <- function(vector_n, vector_mean_VR_factor, vector_mean_COV_factor,
                          vector_sd_COV, vector_sd_ERROR_factor, vector_slope_factor, n_dec = 2){


  vector_orden_levels <- 1:length(vector_n)
  vector_ref_levels <- LETTERS[vector_orden_levels]

  list_data <- sapply(vector_orden_levels, function(x){

    super_ok_cov <- FALSE
    while(!super_ok_cov){
      simu_g_cov <- gen_special_standard_004_B(selected_n = vector_n[x])


      if (length(simu_g_cov) == vector_n[x]) {
        simu_g_cov <- round(x = simu_g_cov, digits = 2)
        simu_g_cov <- simu_g_cov - mean(simu_g_cov)
        simu_g_cov <- simu_g_cov*vector_sd_COV[x] + vector_mean_COV_factor[x]
        super_ok_cov <- TRUE
      }
    }

    super_ok_error <- FALSE
    while(!super_ok_error){
      simu_g_error <- gen_special_standard_004_B(selected_n = vector_n[x])


      if (length(simu_g_error) == vector_n[x]) {
        simu_g_error <- round(x = simu_g_error, digits = 2)
        simu_g_error <- simu_g_error - mean(simu_g_error)
        simu_g_error <- simu_g_error*vector_sd_ERROR_factor[x] + 0
        super_ok_error <- TRUE
      }
    }

    simu_g_vr <- vector_mean_VR_factor[x] + vector_slope_factor[x]*(simu_g_cov - mean(simu_g_cov)) + simu_g_error
    simu_g_factor <- rep(LETTERS[x], vector_n[x])
    data_g <- cbind.data.frame(simu_g_vr, simu_g_cov, simu_g_factor)
    data_g

  }, simplify = F)

  df_simu <- do.call(rbind.data.frame, list_data)
  vector_orden_df <- 1:nrow(df_simu)
  df_simu <- cbind.data.frame(vector_orden_df, df_simu)
  colnames(df_simu) <- c("orden", "VR", "COV", "FACTOR")
  df_simu[,"FACTOR"] <- as.factor(df_simu[,"FACTOR"])

  return(df_simu)

}


control_004 <- function(df_simu, alpha_value = 0.05){

  VR     <- df_simu[,"VR"]
  FACTOR <- df_simu[,"FACTOR"]
  COV    <- df_simu[,"COV"]

  list_lm_ancova_with        <- lm(VR ~ COV + FACTOR + COV:FACTOR)
  list_aov_ancova_with       <- aov(list_lm_ancova_with)
  list_summary_ancova_with   <- summary(list_aov_ancova_with)
  df_aov_ancova_with         <- as.data.frame(list_summary_ancova_with[[1]])
  p_value_ancova_interaction <- df_aov_ancova_with$`Pr(>F)`[3]


  vector_obs_parts_slopes <- list_lm_ancova_with$coefficients
  amount_levels <- nlevels(FACTOR)
  amount_last <- amount_levels - 1
  pos_parts <- c(2, tail(1:length(vector_obs_parts_slopes), amount_last))
  new_parts <- vector_obs_parts_slopes[pos_parts]
  first_slope <- new_parts[1]
  mod_slopes <- new_parts
  mod_slopes[1] <- 0
  new_obs_slopes <- first_slope + mod_slopes



  RESIDUALS <- list_lm_ancova_with$residuals

  list_residuals_normality_with   <- shapiro.test(RESIDUALS)
  list_residuals_homogeneity_with <- bartlett.test(RESIDUALS ~ FACTOR)


  # Minibase mod
  minibase_mod <- cbind.data.frame(df_simu, RESIDUALS)
  colnames(minibase_mod)[ncol(minibase_mod)] <- "RESIDUALS"
  # Obtener el análisis de varianza


  vector_test <- c("Ancova interaction",
                   "Residuals Normality - Ancova With",
                   "Residuals Homogeneity - Ancova With")

  vector_p_control <- c(p_value_ancova_interaction,
                        list_residuals_normality_with$p.value,
                        list_residuals_homogeneity_with$p.value)

  vector_check_H0 <- vector_p_control >= alpha_value
  phrase_check_T <- "H0 Not Rejected"
  phrase_check_F <- "H0 Rejected"

  vector_decision <- ifelse(test = vector_check_H0,
                            yes = phrase_check_T,
                            no = phrase_check_F)


  vector_check_H0_mod <- vector_check_H0
  if(vector_check_H0_mod[1] == FALSE) vector_check_H0_mod[1] <- TRUE

  df_control <- data.frame(
    "test" = vector_test,
    "p_value" = vector_p_control,
    "alpha_value" = alpha_value,
    "decision" = vector_decision,
    "vector_check" = vector_check_H0_mod
  )

  df_slope <- data.frame(
    "level" = levels(FACTOR),
    "spected_slopes"  = vector_slope_factor,
    "observed_slopes" = new_obs_slopes
  )
  rownames(df_slope) <- 1:nrow(df_slope)

  df_mean_VR_factor <- data.frame(
    "level" = levels(FACTOR),
    "spected_mean_VR_factor"  = vector_mean_VR_factor,
    "observed_mean_VR_factor" = tapply(VR, FACTOR, mean)
  )
  rownames(df_mean_VR_factor) <- 1:nrow(df_mean_VR_factor)

  df_mean_COV_factor <- data.frame(
    "level" = levels(FACTOR),
    "spected_mean_COV_factor"  = vector_mean_COV_factor,
    "observed_mean_COV_factor" = tapply(COV, FACTOR, mean)
  )
  rownames(df_mean_COV_factor) <- 1:nrow(df_mean_COV_factor)

  df_sd_ERROR_factor <- data.frame(
    "level" = levels(FACTOR),
    "spected_sd_ERROR_factor"  = vector_sd_ERROR_factor,
    "observed_sd_ERROR_factor" = tapply(RESIDUALS, FACTOR, sd)
  )
  rownames(df_sd_ERROR_factor) <- 1:nrow(df_sd_ERROR_factor)


  output_list_control <- list(df_control, minibase_mod, df_slope,
                              df_mean_VR_factor, df_mean_COV_factor, df_sd_ERROR_factor)

  names(output_list_control) <- c("df_control", "minibase_mod", "df_slope",
                                  "df_mean_VR_factor", "df_mean_COV_factor", "df_sd_ERROR_factor")
  return(output_list_control)
}

summary_004 <- function(minibase_mod){

  VR <- minibase_mod[,"VR"]
  FACTOR <- minibase_mod[,"FACTOR"]
  COV <- minibase_mod[,"COV"]

  RESIDUALS <- minibase_mod[,"RESIDUALS"]

  vector_orden_levels <- 1:nlevels(FACTOR)
  vector_ref_levels   <- levels(FACTOR)

  df_summary_vr <- data.frame(
    "orden"    = vector_orden_levels,
    "level"    = vector_ref_levels,
    "n"        = tapply(VR, FACTOR, length),
    "min"     = tapply(VR, FACTOR, min),
    "max"     = tapply(VR, FACTOR, max),
    "mean"     = tapply(VR, FACTOR, mean),
    "variance" = tapply(VR, FACTOR, var)
  )


  df_summary_cov <- data.frame(
    "orden"    = vector_orden_levels,
    "level"    = vector_ref_levels,
    "n"        = tapply(COV, FACTOR, length),
    "min"      = tapply(COV, FACTOR, min),
    "max"      = tapply(COV, FACTOR, max),
    "mean"     = tapply(COV, FACTOR, mean),
    "variance" = tapply(COV, FACTOR, var)
  )


  df_summary_residuals <- data.frame(
    "orden"    = vector_orden_levels,
    "level"    = vector_ref_levels,
    "n"        = tapply(RESIDUALS, FACTOR, length),
    "min"      = tapply(RESIDUALS, FACTOR, min),
    "max"      = tapply(RESIDUALS, FACTOR, max),
    "mean"     = tapply(RESIDUALS, FACTOR, mean),
    "variance" = tapply(RESIDUALS, FACTOR, var)
  )

  output_list <- list(df_summary_vr, df_summary_cov, df_summary_residuals)
  names(output_list) <- c("df_summary_vr", "df_summary_cov", "df_summary_residuals")

  return(output_list)
}

general_vision_004 <- function(output_list_control){

  df_control <- output_list_control$"df_control"
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

  output_list_general <- list(check_control_general, phrase_control_general)
  names(output_list_general) <- c("check_control_general", "phrase_control_general")

  output_list_general
}

gen_and_save_004 <- function(vector_n, vector_mean_VR_factor, vector_mean_COV_factor,
                             vector_sd_COV, vector_sd_ERROR_factor, value_slope, n_dec, the_way = 1){


  # Base simulada
  if(the_way == 1) df_simu <- genbase_004_A(vector_n, vector_mean_VR_factor, vector_mean_COV_factor,
                                            vector_sd_COV, vector_sd_ERROR_factor, value_slope, n_dec)

  if(the_way == 2) df_simu <- genbase_004_B(vector_n, vector_mean_VR_factor, vector_mean_COV_factor,
                                            vector_sd_COV, vector_sd_ERROR_factor, value_slope, n_dec)


  # DF del control
  list_output_control <- control_004(df_simu, alpha_value)
  df_control <- list_output_control$"df_control"
  cat("\n")
  print(df_control)
  cat("\n")

  df_slope <- list_output_control$"df_slope"
  cat("\n")
  print(df_slope)
  cat("\n")


  df_mean_VR_factor <- list_output_control$"df_mean_VR_factor"
  cat("\n")
  print(df_mean_VR_factor)
  cat("\n")

  df_mean_COV_factor <- list_output_control$"df_mean_COV_factor"
  cat("\n")
  print(df_mean_COV_factor)
  cat("\n")


  df_sd_ERROR_factor <- list_output_control$"df_sd_ERROR_factor"
  cat("\n")
  print(df_sd_ERROR_factor)
  cat("\n")

  # Medidas resumen
  minibase_mod <- list_output_control$"minibase_mod"
  df_summary <- summary_004(minibase_mod)
  cat("\n")
  print(df_summary)
  cat("\n")

  # Control
  list_control <- general_vision_004(list_output_control)

  # Check general
  check_ok <- list_control$check_control_general

  if(!check_ok) return(NULL)

  # Guardar base
  the_date <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  pre_file_name <- "df_simu_ancova_without"
  new_file_name <- paste0(pre_file_name, the_date, ".xlsx")

  openxlsx::write.xlsx(x = df_simu, file = new_file_name)

  cat(paste0("Archivo ", new_file_name, " guardado!"))
  cat("\n")
}



# Generador!

# Inputs
vector_n <- c(10, 14, 20)
vector_mean_VR_factor <- c(30, 40, 60)
vector_mean_COV_factor <- c(50, 50, 50)
vector_sd_COV <- c(2, 2, 2)
vector_sd_ERROR_factor <- c(3, 3, 3)
vector_slope_factor <- c(10, 15, 40)

n_dec <- 2

alpha_value <- 0.05



gen_and_save_004(vector_n, vector_mean_VR_factor, vector_mean_COV_factor,
                 vector_sd_COV, vector_sd_ERROR_factor, vector_slope_factor, n_dec, the_way = 2)



