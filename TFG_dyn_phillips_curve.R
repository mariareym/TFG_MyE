
## María Rey
## Trabajo de Fin de Grado: Matemáticas y Estadística
## Estimación de parámetros en DLMs por técnicas MCMC


## Librerías

 library(readxl)
 library(dplyr)
 library(tidyr)
 library(ggplot2)
 library(dlm)
 library(stringr)
 library(zoo)
 library(stargazer)
 

## Parte 1: Importar y construir base de datos

 jst_data <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Matemáticas y Estadística/Datos/JSTdatasetR6.xlsx"

 JST <- read_excel(jst_data)
 jst <- data.frame(JST)

 jst <- jst %>%
  arrange(ifs, year) %>%
   select(year, country, ifs, cpi, unemp, wage) %>%
   arrange(ifs, year) %>%
   group_by(ifs) %>%
   mutate(
     cpi_lag = lag(cpi),
     wage_lag = lag(wage),
     price_infl = ((cpi - cpi_lag)/cpi_lag)*100,
     wage_infl = ((wage - wage_lag)/wage_lag)*100,
     lagged_price_infl = lag(price_infl)
     ) %>%
   ungroup()
 
## Parte 3: Países a analizar
 
 us_data <- jst %>%
   filter(country == "USA") %>%
   na.omit(uk_data) %>%
   select(
    year, wage_infl, unemp, lagged_price_infl, price_infl 
   )
   
 us.ts <- ts(us_data, start=c(1872), frequency = 1)
 
 
 ## Parte 2: Análisis descriptivo 
 
 ggplot(us_data, aes(x = year)) +
   geom_line(aes(y = unemp, color = "Tasa de desempleo"), size = 0.5) +
   geom_line(aes(y = wage_infl, color = "Inflación salarial"), size = 0.5) +
   theme_minimal() + 
   theme(legend.position = "bottom", 
         axis.title = element_text(size = 8), 
         axis.title.x = element_text(margin = margin(t = 6)),
         axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
   labs(
     colour = NULL,
     x = "",
     y = "%") +
   scale_color_manual(values = c("Tasa de desempleo" = "orchid4", "Inflación salarial" = "navy"))
 
 us_pc <- lm(wage_infl ~ unemp + lagged_price_infl, data = us.ts)

 
 ## Parte 3: Modelizar DLM 
 
 #### Modelo #### 

 build_dpc <- function(w) {
     FF = matrix(c(1, 0, 0), nr=1)
     V = exp(w[1])
     GG = diag(3)
     W = diag(exp(w[2:4]))
     m0 = rep(0,3)
     C0 = 2*diag(3)
     JFF = matrix(c(0, 1, 1), nrow=1)
     X = as.matrix(us.ts[, c("unemp", "lagged_price_infl")])
     dpc <- dlm(FF=FF, V=V, GG=GG, W=W, m0=m0, C0=C0, JFF=JFF, X=X)
    return(dpc)
 }
 
 ###### MLE ########
 
 # Definir los 30 vectores de parámetros
 param0 <- c(0, 0, 0, 0)
 param1 <- c(0.1, 2, 0.05, 0.2)
 param2 <- c(15, 10, 25, 5)
 param3 <- c(1, 3, 2, 4)
 param4 <- c(0.3, 0.5, 0.8, 0.2)
 param5 <- c(2, 1, 3, 4)
 param6 <- c(0.05, 0.1, 0.15, 0.2)
 param7 <- c(1.5, 2.5, 3.5, 1.5)
 param8 <- c(3, 4, 5, 2)
 param9 <- c(3, 6, 9, 12)
 param10 <- c(5, 15, 10, 20)
 param11 <- c(2.5, 3.5, 2, 4)
 param12 <- c(2, 1.5, 3, 2.5)
 param13 <- c(2.7, 3.2, 3.5, 3.8)
 param14 <- c(1.2, 1.5, 1.8, 2)
 param15 <- c(5, 4.5, 5.5, 6)
 param16 <- c(1, 0.7, 1.3, 1.5)
 param17 <- c(1, 1.5, 1.8, 1.2)
 param18 <- c(1.5, 2, 2.5, 3)
 param19 <- c(0.5, 1.5, 2, 2.5)
 param20 <- c(4, 3, 5, 6)
 param21 <- c(2.3, 2.8, 3.2, 3.6)
 param22 <- c(1.5, 2, 2.5, 3)
 param23 <- c(4.5, 5, 5.5, 6)
 param24 <- c(1.5, 2.5, 1, 4)
 param25 <- c(2.5, 3.5, 1.5, 4.5)
 param26 <- c(0.7, 0.9, 1.1, 1.3)
 param27 <- c(1.2, 1.5, 1.8, 2)
 param28 <- c(3.2, 3.5, 3.8, 4)
 param29 <- c(2, 2.5, 3, 3.5)
 param30 <- c(1.5, 2, 2.5, 3)
 
 # Crear una lista de vectores de parámetros
 params_list <- list(param0, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10,
                     param11, param12, param13, param14, param15, param16, param17, param18, param19, param20,
                     param21, param22, param23, param24, param25, param26, param27, param28, param29, param30)
 
 # Definir la función para calcular MLE, dpc y mostrar resultados
 calculate_mle <- function(parms) {
   outMLE <- dlmMLE(us.ts[,"wage_infl"], parm = parms, build_dpc)
   convergence <- outMLE$convergence
   if (convergence == 0) {
     dpcMLE <- build_dpc(outMLE$par)
     W_value <- W(dpcMLE)
     V_value <- V(dpcMLE)
     return(list(convergence = convergence, W = W_value, V = V_value))
   } else {
     return(list(convergence = convergence, W = NA, V = NA))
   }
 }
 
 # Ejecutar el bucle para cada vector de parámetros y guardar resultados
 results <- lapply(params_list, calculate_mle)
 
 # Mostrar resultados
 for (i in seq_along(results)) {
   cat("Iteración", i, "\n")
   cat("Convergencia:", results[[i]]$convergence, "\n")
   if (results[[i]]$convergence == 0) {
     cat("Valor de W:", results[[i]]$W, "\n")
     cat("Valor de V:", results[[i]]$V, "\n")
   } else {
     cat("No se alcanzó la convergencia\n")
   }
   cat("\n")
 }
 
 dpcMLE <- build_dpc(c(.00001854, 7.0786, 0.04183, 0.04183))
 
 smoothMLE <- dlmSmooth(us.ts[,"wage_infl"], dpcMLE)
 filterMLE <- dlmFilter(us.ts[,"wage_infl"], dpcMLE)
 
 onesa_forecastMLE <- filterMLE$f
 dpcMLE_perf <- smoothMLE$s[,1] + smoothMLE$s[,2]*us.ts[,3] + smoothMLE$s[,3]*us.ts[,4]
 
 # Análisis

 year <- 1890:(1890 + length(us.ts[,"year"]) -1)
 year1 <- 1890:(1890 + length(us.ts[,"year"]))
 
 # calcular métricas de ajuste del modelo y hacer una comparativa con MCMC
 
 # MAE - mean absolute error 
 
 plot(year, abs(us.ts[, "wage_infl"] - onesa_forecastMLE), xlab="", ylab="", type = "l")
 mean(abs(us.ts[, "wage_infl"] - onesa_forecastMLE))
 
 plot(year, abs(us.ts[, "wage_infl"] - dpcMLE_perf), xlab="", ylab="", type = "l")
 mean(abs(us.ts[, "wage_infl"] - dpcMLE_perf))
 
 # MSE - mean square error
 
 plot(year, (us.ts[, "wage_infl"] - onesa_forecastMLE)^2, xlab="", ylab="", type = "l")
 mean((us.ts[, "wage_infl"] - onesa_forecastMLE)^2)
 
 plot(year, (us.ts[, "wage_infl"] - dpcMLE_perf)^2, xlab="", ylab="", type = "l")
 mean((us.ts[, "wage_infl"] - dpcMLE_perf)^2)
 
 # RMSE - Root mean square error
 
 mean(sqrt((us.ts[, "wage_infl"] - onesa_forecastMLE)^2))
 mean(sqrt((us.ts[, "wage_infl"] - dpcMLE_perf)^2))

 
 ########### MCMC #############

 set.seed(6385)
 MCMC <- 12000
 
 # parámetros iniciales 
 
 dpc_mod <- dlm(FF = matrix(c(1, 0, 0), nr = 1),
                V = exp(param1[1]),
                GG = diag(3),
                W = diag(exp(param1[2:4])),
                m0 = rep(0, 3),
                C0 = 2 * diag(3),
                JFF = matrix(c(0, 1, 1), nrow = 1),
                X = as.matrix(us.ts[, c("unemp", "lagged_price_infl")]))
 
 #ajustar parámetros!
 
 gibbsOut1 <- dlmGibbsDIG(y = us.ts[, "wage_infl"], 
                         mod = dpc_mod, 
                         a.y = 53932, b.y = 0.5,
                         a.theta = c(0.14127, 23.906, 23.906), b.theta = c(0.5, 0.01, 0.01),
                         thin=5,
                         n.sample = MCMC,
                         save.states = TRUE)
 
 
 gibbsOut2 <- dlmGibbsDIG(y = us.ts[, "wage_infl"], 
                          mod = dpc_mod, 
                          a.y = 53932, b.y = 0.5,
                          a.theta = c(0.14127, 23.906, 23.906), b.theta = c(0.5, 0.01, 0.01),
                          thin=5,
                          n.sample = MCMC,
                          save.states = TRUE)
 
 
 gibbsOut3 <- dlmGibbsDIG(y = us.ts[, "wage_infl"], 
                          mod = dpc_mod, 
                          a.y = 53932, b.y = 0.5,
                          a.theta = c(0.14127, 23.906, 23.906), b.theta = c(0.5, 0.01, 0.01),  
                          n.sample = MCMC,
                          thin=5,
                          save.states = TRUE)
 
 
 # medias ergódicas
 
 ergmeansdV <- data.frame(ergMean(gibbsOut1$dV), ergMean(gibbsOut2$dV), ergMean(gibbsOut3$dV))
 ergmeansdW1 <- data.frame(ergMean(gibbsOut1$dW[,1]), ergMean(gibbsOut2$dW[,1]), ergMean(gibbsOut3$dW[,1]))
 ergmeansdW2 <- data.frame(ergMean(gibbsOut1$dW[,2]), ergMean(gibbsOut2$dW[,2]), ergMean(gibbsOut3$dW[,2]))
 ergmeansdW3 <- data.frame(ergMean(gibbsOut1$dW[,3]), ergMean(gibbsOut2$dW[,3]), ergMean(gibbsOut3$dW[,3]))
 
 matplot(ergmeansdV, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab = "")
 mtext(expression(sigma[v]^2), side = 2, line = 2.5)
 legend("bottomright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")
 
 matplot(ergmeansdW1, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab = "")
 mtext(expression(sigma[0]^2), side = 2, line = 2.5)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")
 
 matplot(ergmeansdW2, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab="")
 mtext(expression(sigma[1]^2), side = 2, line = 2.5)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")
 
 matplot(ergmeansdW3, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab = "")
 mtext(expression(sigma[2]^2), side = 2, line = 2.5)
 legend("bottomright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")

 burn <- 4000
 attach(gibbsOut1)
 dV1 <- dV[-(1:burn)]
 dW1 <- dW[-(1:burn),]
 detach()

 attach(gibbsOut2)
 dV2 <- dV[-(1:burn)]
 dW2 <- dW[-(1:burn),]
 detach()
 
 attach(gibbsOut3)
 dV3 <- dV[-(1:burn)]
 dW3 <- dW[-(1:burn),]
 detach()
 
 # desviaciones estándar de quitando el burn in 
 sd(dV1)
 sd(dV2)
 sd(dV3)
 
 sd(dW1[,1])
 sd(dW2[,1])
 sd(dW3[,1])
 
 sd(dW1[,2])
 sd(dW2[,2])
 sd(dW3[,2])
 
 sd(dW1[,3])
 sd(dW2[,3])
 sd(dW3[,3])
  
# simulaciones
 
 simulationsdV <- data.frame(gibbsOut1$dV, gibbsOut2$dV, gibbsOut3$dV)
 simulationsdW1 <- data.frame(gibbsOut1$dW[,1], gibbsOut2$dW[,1], gibbsOut3$dW[,1])
 simulationsdW2 <- data.frame(gibbsOut1$dW[,2], gibbsOut2$dW[,2], gibbsOut3$dW[,2])
 simulationsdW3 <- data.frame(gibbsOut1$dW[,3], gibbsOut2$dW[,3], gibbsOut3$dW[,3])
 
 matplot(simulationsdV, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab = "")
 mtext(expression(sigma[v]^2), side = 2, line = 2.5)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")
 
 matplot(simulationsdW1, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab = "")
 mtext(expression(sigma[0]^2), side = 2, line = 2.5)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")
 
 matplot(simulationsdW2, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab = "")
 mtext(expression(sigma[1]^2), side = 2, line = 2.5)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")
 
 matplot(simulationsdW3, type = "l", lty = 1, col = c("steelblue1", "steelblue", "navy"), 
         xlab = "Iteraciones", ylab = "")
 mtext(expression(sigma[2]^2), side = 2, line = 2.5)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lty = 1, bty = "n")
 
 
 # posterior means of the unknown variances
 mcmcMean(cbind(dV1[-(1:burn)], dW1[-(1:burn),]))
 mcmcMean(cbind(dV2[-(1:burn)], dW2[-(1:burn),]))
 mcmcMean(cbind(dV3[-(1:burn)], dW3[-(1:burn),]))
 
 dV_interval <- quantile(dV2, probs = c(0.025, 0.975))
 dW_intervals <- apply(dW2, 2, quantile, probs = c(0.025, 0.975))
 
 # Mostrar resultados
 print("95% confidence interval for dV:")
 print(dV_interval)
 
 print("95% confidence intervals for dW1:")
 print(dW_intervals)

 posterior_means <- c(1.90e-05,  7.21e+00, 4.18e-02, 4.18e-02)
 
 dpcMCMC <- build_dpc(posterior_means)
 filterMCMC <- dlmFilter(us.ts[,2], dpcMCMC)
 smoothMCMC <- dlmSmooth(us.ts[,2], dpcMCMC)
 
 onesa_forecastMCMC <- filterMCMC$f
 dpcMCMC_perf <- smoothMCMC$s[,1] + smoothMCMC$s[,2]*us.ts[,3] + smoothMCMC$s[,3]*us.ts[,4]

 set.seed(6385)
 dpc_FFBS <- dlmBSample(filterMCMC)
 
 
 # Densidad posterior de dV (varianza del ruido de observación)
 
 densitydV1 <- density(dV1)
 densitydV2 <- density(dV2)
 densitydV3 <- density(dV3)
 
 densitydW11 <- density(dW1[,1])
 densitydW12<- density(dW2[,1])
 densitydW13 <- density(dW3[,1])
 
 densitydW21 <- density(dW1[,2])
 densitydW22<- density(dW2[,2])
 densitydW23 <- density(dW3[,2])
 
 densitydW31 <- density(dW1[,3])
 densitydW32<- density(dW2[,3])
 densitydW33 <- density(dW3[,3])
 
 plot(densitydV1, col = "steelblue1", lwd = 2, main = "", xlab = expression(sigma[v]^2), ylab = "Densidad")
 lines(densitydV2, col = "steelblue", lwd = 2)
 lines(densitydV3, col = "navy", lwd = 2)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lwd = 2, bty = "n")
 
 plot(densitydW11, col = "steelblue1", lwd = 2, main = "", xlab = expression(sigma[0]^2), ylab = "Densidad")
 lines(densitydW12, col = "steelblue", lwd = 2)
 lines(densitydW13, col = "navy", lwd = 2)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lwd = 2, bty = "n")
 
 
 plot(densitydW21, col = "steelblue1", lwd = 2, main = "", xlab = expression(sigma[1]^2), ylab = "Densidad")
 lines(densitydW22, col = "steelblue", lwd = 2)
 lines(densitydW23, col = "navy", lwd = 2)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lwd = 2, bty = "n")
 
 plot(densitydW31, col = "steelblue1", lwd = 2, main = "", xlab = expression(sigma[2]^2), ylab = "Densidad")
 lines(densitydW32, col = "steelblue", lwd = 2)
 lines(densitydW33, col = "navy", lwd = 2)
 legend("topright", legend = c("Simulación 1", "Simulación 2", "Simulación 3"), 
        col = c("steelblue1", "steelblue", "navy"), lwd = 2, bty = "n")
 
 acf(dV1, main = "")
 acf(dV2)
 acf(dV3)
 
 acf(dW1[,1], main = "")
 acf(dW2[,1])
 acf(dW3[,1])
 
 acf(dW1[,2], main = "")
 acf(dW2[,2])
 acf(dW3[,2])
 
 acf(dW1[,3], main = "")
 acf(dW2[,3])
 acf(dW3[,3])
 
  ### análisis y comparativa del modelo 
 
 # predicciones a un paso de las observaciones
 
 ylimosa <- range(c(filterMCMC$f, filterMLE$f))
 plot(year, filterMCMC$y, col = "grey45", type = "l", lwd = 1, lty=2, main = "", xlab = "", ylab = "inflación salarial (%)", ylim=ylimosa)
 lines(year, filterMCMC$f, col = "navy", type = "l", lwd = 1)
 lines(year, filterMLE$f, col = "orchid3", type = "l", lwd = 1)
 legend("topright", legend = c("EMV", "MCMC"), col = c("orchid3", "navy"), lwd = 1, bty = "n")
 
 # Calcula los intervalos de confianza para las predicciones a un paso
 filterMCMC_var_pred <- sapply(dlmSvd2var(filterMCMC$U.R, filterMCMC$D.R), function(x) x[1, 1])
 filterMLE_var_pred <- sapply(dlmSvd2var(filterMLE$U.R, filterMLE$D.R), function(x) x[1, 1])
 filterMCMC_lower <- filterMCMC$f - qnorm(0.975) * sqrt(filterMCMC_var_pred)/sqrt(132)
 filterMCMC_upper <- filterMCMC$f + qnorm(0.975) * sqrt(filterMCMC_var_pred)/sqrt(132)
 filterMLE_lower <- filterMLE$f - qnorm(0.975) * sqrt(filterMLE_var_pred)/sqrt(132)
 filterMLE_upper <- filterMLE$f + qnorm(0.975) * sqrt(filterMLE_var_pred)/sqrt(132)
 ylimosa <- range(c(filterMCMC$f, filterMLE$f, filterMCMC_lower, filterMCMC_upper, filterMLE_lower, filterMLE_upper))
 plot(year, filterMCMC$y, col = "grey45", type = "l", lwd = 1, lty=2, main = "", xlab = "", ylab = "Inflación salarial (%)", ylim=ylimosa)
 lines(year, filterMCMC$f, col = "navy", type = "l", lwd = 1)
 lines(year, filterMLE$f, col = "orchid3", type = "l", lwd = 1)
 polygon(c(year, rev(year)), c(filterMCMC_lower, rev(filterMCMC_upper)), col = adjustcolor("skyblue", alpha.f = 0.2), border = NA)
 polygon(c(year, rev(year)), c(filterMLE_lower, rev(filterMLE_upper)), col = adjustcolor("plum", alpha.f = 0.2), border = NA)
 legend("topright", legend = c("EMV", "MCMC", "IC 95% EMV", "IC 95% MCMC"), col = c("plum", "navy", adjustcolor("plum", alpha.f = 0.2), adjustcolor("skyblue", alpha.f = 0.2)), lwd = 1, bty = "n")
 
 
 
 ggplot(us_data, aes(x = year)) +
   geom_line(aes(y = filterMCMC$y, color = "Observaciones"), linetype="dashed", size = 0.5) +
   geom_line(aes(y = filterMCMC$f, color = "MCMC"), linetype="solid", size = 0.5) +
   geom_line(aes(y = filterMLE$f, color = "MLE"), linetype="solid", size = 0.5) +
   theme_minimal() + 
   theme(legend.position = "bottom", 
         axis.title = element_text(size = 8), 
         axis.title.x = element_text(margin = margin(t = 6)),
         axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
   labs(
     color = NULL,
     x = "Año",
     y = "Tantos por uno") +
   scale_color_manual(values = c("Observaciones" = "grey45", "MCMC" = "steelblue", "MLE" = "steelblue1"))

 
 # performance del modelo
 
 ylimosa <- range(c(filterMCMC$y, dpcMCMC_perf))
 plot(year, filterMCMC$y, col = "grey45", type = "l", lwd = 1, lty=2, main = "", xlab = "", ylab = "inflación salarial (%)", ylim=ylimosa)
 lines(year, dpcMCMC_perf, col = "navy", type = "l", lwd = 1)
 lines(year, dpcMLE_perf, col = "orchid3", type = "l", lwd = 1)
 legend("topright", legend = c("EMV", "MCMC"), col = c("orchid3", "navy"), lwd = 1, bty = "n")
 
 ggplot(us_data, aes(x = year)) +
   geom_line(aes(y = filterMCMC$y, color = "Observaciones"), linetype="dashed", size = 0.5) +
   geom_line(aes(y = dpcMCMC_perf, color = "MCMC"), linetype="solid", size = 0.5) +
   geom_line(aes(y = dpcMLE_perf, color = "MLE"), linetype="solid", size = 0.5) +
   theme_minimal() + 
   theme(legend.position = "bottom", 
         axis.title = element_text(size = 8), 
         axis.title.x = element_text(margin = margin(t = 6)),
         axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
   labs(
     color = NULL,
     x = "Año",
     y = "Tantos por uno") +
   scale_color_manual(values = c("Observaciones" = "grey45", "MCMC" = "steelblue", "MLE" = "steelblue1"))
 
 # estados estimados (smoothing estimates)
 
 smoothMCMC_var <- dlmSvd2var(smoothMCMC$U.S, smoothMCMC$D.S)
 smoothMLE_var <- dlmSvd2var(smoothMLE$U.S, smoothMLE$D.S)
 smoothMCMC_var_diag <- sapply(smoothMCMC_var, function(x) x[1, 1])
 smoothMLE_var_diag <- sapply(smoothMLE_var, function(x) x[1, 1])
 smoothMCMC_lower <- smoothMCMC$s[,1] -qnorm(0.025, lower=FALSE) * sqrt(smoothMCMC_var_diag)/sqrt(132)
 smoothMCMC_upper <- smoothMCMC$s[,1] + qnorm(0.025, lower=FALSE)* sqrt(smoothMCMC_var_diag)/sqrt(132)
 smoothMLE_lower <- smoothMLE$s[,1] -qnorm(0.025, lower=FALSE) * sqrt(smoothMLE_var_diag)/sqrt(132)
 smoothMLE_upper <- smoothMLE$s[,1] + qnorm(0.025, lower=FALSE)* sqrt(smoothMLE_var_diag)/sqrt(132)
 ylim0 <- range(c(smoothMLE$s[,1], smoothMCMC$s[,2], smoothMCMC_lower, smoothMCMC_upper, smoothMLE_lower, smoothMLE_upper))
 plot(year1, smoothMLE$s[,1], col = "steelblue1", type = "l", lwd = 1, main = "", xlab = "", ylab = "", ylim = ylim0)
 lines(year1, smoothMCMC$s[,1], col = "steelblue", type = "l", lwd = 1)
 polygon(c(year1, rev(year1)), c(smoothMLE_lower, rev(smoothMLE_upper)), col = adjustcolor("steelblue1", alpha.f = 0.2), border = NA)
 polygon(c(year1, rev(year1)), c(smoothMCMC_lower, rev(smoothMCMC_upper)), col = adjustcolor("steelblue", alpha.f = 0.2), border = NA)
 mtext(expression(beta[0]), side = 2, line = 2.5)
 legend("topright", legend = c("EMV", "MCMC", "Intervalo de Confianza 95% (EMV)", "Intervalo de Confianza 95% (MCMC)"), col = c("steelblue1", "steelblue", NA, NA), lwd = c(1, 1, NA, NA), lty = c(1, 1, NA, NA), fill = c(NA, NA, adjustcolor("steelblue1", alpha.f = 0.2), adjustcolor("steelblue", alpha.f = 0.2)), bty = "n")
 
 
 smoothMCMC_var <- dlmSvd2var(smoothMCMC$U.S, smoothMCMC$D.S)
 smoothMLE_var <- dlmSvd2var(smoothMLE$U.S, smoothMLE$D.S)
 smoothMCMC_var_diag <- sapply(smoothMCMC_var, function(x) x[2, 2])
 smoothMLE_var_diag <- sapply(smoothMLE_var, function(x) x[2, 2])
 smoothMCMC_lower <- smoothMCMC$s[,2] -qnorm(0.025, lower=FALSE) * sqrt(smoothMCMC_var_diag)/sqrt(132)
 smoothMCMC_upper <- smoothMCMC$s[,2] + qnorm(0.025, lower=FALSE)* sqrt(smoothMCMC_var_diag)/sqrt(132)
 smoothMLE_lower <- smoothMLE$s[,2] -qnorm(0.025, lower=FALSE) * sqrt(smoothMLE_var_diag)/sqrt(132)
 smoothMLE_upper <- smoothMLE$s[,2] + qnorm(0.025, lower=FALSE)* sqrt(smoothMLE_var_diag)/sqrt(132)
 ylim0 <- range(c(smoothMLE$s[,2], smoothMCMC$s[,2], smoothMCMC_lower, smoothMCMC_upper, smoothMLE_lower, smoothMLE_upper))
 plot(year1, smoothMLE$s[,2], col = "steelblue1", type = "l", lwd = 1, main = "", xlab = "", ylab = "", ylim = ylim0)
 lines(year1, smoothMCMC$s[,2], col = "steelblue", type = "l", lwd = 1)
 polygon(c(year1, rev(year1)), c(smoothMLE_lower, rev(smoothMLE_upper)), col = adjustcolor("steelblue1", alpha.f = 0.2), border = NA)
 polygon(c(year1, rev(year1)), c(smoothMCMC_lower, rev(smoothMCMC_upper)), col = adjustcolor("steelblue", alpha.f = 0.2), border = NA)
 mtext(expression(beta[1]), side = 2, line = 2.5)
 legend("topright", legend = c("EMV", "MCMC", "Intervalo de Confianza 95% (EMV)", "Intervalo de Confianza 95% (MCMC)"), col = c("steelblue1", "steelblue", NA, NA), lwd = c(1, 1, NA, NA), lty = c(1, 1, NA, NA), fill = c(NA, NA, adjustcolor("steelblue1", alpha.f = 0.2), adjustcolor("steelblue", alpha.f = 0.2)), bty = "n")
 
 smoothMCMC_var <- dlmSvd2var(smoothMCMC$U.S, smoothMCMC$D.S)
 smoothMLE_var <- dlmSvd2var(smoothMLE$U.S, smoothMLE$D.S)
 smoothMCMC_var_diag <- sapply(smoothMCMC_var, function(x) x[3, 3])
 smoothMLE_var_diag <- sapply(smoothMLE_var, function(x) x[3, 3])
 smoothMCMC_lower <- smoothMCMC$s[,3] - qnorm(0.025, lower=FALSE) * sqrt(smoothMCMC_var_diag)/sqrt(132)
 smoothMCMC_upper <- smoothMCMC$s[,3] + qnorm(0.025, lower=FALSE) * sqrt(smoothMCMC_var_diag)/sqrt(132)
 smoothMLE_lower <- smoothMLE$s[,3] - qnorm(0.025, lower=FALSE) * sqrt(smoothMLE_var_diag)/sqrt(132)
 smoothMLE_upper <- smoothMLE$s[,3] + qnorm(0.025, lower=FALSE) * sqrt(smoothMLE_var_diag)/sqrt(132)
 ylim0 <- range(c(smoothMLE$s[,3], smoothMCMC$s[,3], smoothMCMC_lower, smoothMCMC_upper, smoothMLE_lower, smoothMLE_upper))
 plot(year1, smoothMLE$s[,3], col = "steelblue1", type = "l", lwd = 1, main = "", xlab = "", ylab = "", ylim = ylim0)
 lines(year1, smoothMCMC$s[,3], col = "steelblue", type = "l", lwd = 1)
 polygon(c(year1, rev(year1)), c(smoothMLE_lower, rev(smoothMLE_upper)), col = adjustcolor("steelblue1", alpha.f = 0.2), border = NA)
 polygon(c(year1, rev(year1)), c(smoothMCMC_lower, rev(smoothMCMC_upper)), col = adjustcolor("steelblue", alpha.f = 0.2), border = NA)
 mtext(expression(beta[2]), side = 2, line = 2.5)
 legend("topright", legend = c("EMV", "MCMC", "Intervalo de Confianza 95% (EMV)", "Intervalo de Confianza 95% (MCMC)"), col = c("steelblue1", "steelblue", NA, NA), lwd = c(1, 1, NA, NA), lty = c(1, 1, NA, NA), fill = c(NA, NA, adjustcolor("steelblue1", alpha.f = 0.2), adjustcolor("steelblue", alpha.f = 0.2)), bty = "n")
 
 plot(year1, smoothMLE$s[,1], col = "steelblue1", type="l", lwd = 1, main = "", xlab = "", ylab = "")
 lines(year1, smoothMCMC$s[,1], col = "steelblue",  type="l", lwd = 1)
 mtext(expression(beta[0]), side = 2, line = 2.5)
 legend("bottomright", legend = c("EMV", "MCMC"), col = c("steelblue1", "steelblue"), lwd = 1, bty = "n")
 
 plot(year1, smoothMLE$s[,2], col = "steelblue1", type="l", lwd = 1, main = "", xlab = "", ylab = "")
 lines(year1, smoothMCMC$s[,2], col = "steelblue",  type="l", lwd = 1)
 mtext(expression(beta[1]), side = 2, line = 2.5)
 legend("bottomright", legend = c("EMV", "MCMC"), col = c("steelblue1", "steelblue"), lwd = 1, bty = "n")
 
 ylim2 <- range(c(smoothMLE$s[,3], smoothMCMC$s[,3]))
 plot(year1, smoothMLE$s[,3], col = "steelblue1",  type="l", lwd = 1, main = "", xlab = "", ylab = "", ylim = ylim2)
 lines(year1, smoothMCMC$s[,3], col = "steelblue",  type="l", lwd = 1)
 mtext(expression(beta[2]), side = 2, line = 2.5)
 legend("bottomright", legend = c("EMV", "MCMC"), col = c("steelblue1", "steelblue"), lwd = 1, bty = "n")
 

 #estados filtrados (filtered estimates)
 
 filterMCMC_var <- dlmSvd2var(filterMCMC$U.C, filterMCMC$D.C)
 filterMLE_var <- dlmSvd2var(filterMLE$U.C,filterMLE$D.C)
 filterMCMC_var_diag <- sapply(filterMCMC_var, function(x) x[1, 1])
 filterMLE_var_diag <- sapply(filterMLE_var, function(x) x[1, 1])
 filterMCMC_lower <- filterMCMC$m[,1] -qnorm(0.025, lower=FALSE)* sqrt(filterMCMC_var_diag)/sqrt(132)
 filterMCMC_upper <- filterMCMC$m[,1] + qnorm(0.025, lower=FALSE) * sqrt(filterMCMC_var_diag)/sqrt(132)
 filterMLE_lower <- filterMLE$m[,1] - qnorm(0.025, lower=FALSE)* sqrt(filterMLE_var_diag)/sqrt(132)
 filterMLE_upper <- filterMLE$m[,1] + qnorm(0.025, lower=FALSE) * sqrt(filterMLE_var_diag)/sqrt(132)
 ylim0 <- range(c(filterMLE$m[,1], filterMCMC$m[,1], filterMCMC_lower, filterMCMC_upper, filterMLE_lower, filterMLE_upper))
 plot(year1, filterMLE$m[,1], col = "steelblue1", type = "l", lwd = 1, main = "", xlab = "", ylab = "", ylim = ylim0)
 lines(year1, filterMCMC$m[,1], col = "steelblue", type = "l", lwd = 1)
 polygon(c(year1, rev(year1)), c(filterMLE_lower, rev(filterMLE_upper)), col = adjustcolor("steelblue1", alpha.f = 0.2), border = NA)
 polygon(c(year1, rev(year1)), c(filterMCMC_lower, rev(filterMCMC_upper)), col = adjustcolor("steelblue", alpha.f = 0.2), border = NA)
 mtext(expression(beta[0]), side = 2, line = 2.5)
 legend("topright", legend = c("EMV", "MCMC", "Intervalo de Confianza 95% (EMV)", "Intervalo de Confianza 95% (MCMC)"), col = c("steelblue1", "steelblue", NA, NA), lwd = c(1, 1, NA, NA), lty = c(1, 1, NA, NA), fill = c(NA, NA, adjustcolor("steelblue1", alpha.f = 0.2), adjustcolor("steelblue", alpha.f = 0.2)), bty = "n")
 
 filterMCMC_var <- dlmSvd2var(filterMCMC$U.C, filterMCMC$D.C)
 filterMLE_var <- dlmSvd2var(filterMLE$U.C,filterMLE$D.C)
 filterMCMC_var_diag <- sapply(filterMCMC_var, function(x) x[2, 2])
 filterMLE_var_diag <- sapply(filterMLE_var, function(x) x[2, 2])
 filterMCMC_lower <- filterMCMC$m[,2] - qnorm(0.025, lower=FALSE) * sqrt(filterMCMC_var_diag)/sqrt(132)
 filterMCMC_upper <- filterMCMC$m[,2] + qnorm(0.025, lower=FALSE) * sqrt(filterMCMC_var_diag)/sqrt(132)
 filterMLE_lower <- filterMLE$m[,2] - qnorm(0.025, lower=FALSE) * sqrt(filterMLE_var_diag)/sqrt(132)
 filterMLE_upper <- filterMLE$m[,2] + qnorm(0.025, lower=FALSE) * sqrt(filterMLE_var_diag)/sqrt(132)
 ylim0 <- range(c(filterMLE$m[,2], filterMCMC$m[,2], filterMCMC_lower, filterMCMC_upper, filterMLE_lower, filterMLE_upper))
 plot(year1, filterMLE$m[,2], col = "steelblue1", type = "l", lwd = 1, main = "", xlab = "", ylab = "", ylim = ylim0)
 lines(year1, filterMCMC$m[,2], col = "steelblue", type = "l", lwd = 1)
 polygon(c(year1, rev(year1)), c(filterMLE_lower, rev(filterMLE_upper)), col = adjustcolor("steelblue1", alpha.f = 0.2), border = NA)
 polygon(c(year1, rev(year1)), c(filterMCMC_lower, rev(filterMCMC_upper)), col = adjustcolor("steelblue", alpha.f = 0.2), border = NA)
 mtext(expression(beta[1]), side = 2, line = 2.5)
 legend("topright", legend = c("EMV", "MCMC", "Intervalo de Confianza 95% (EMV)", "Intervalo de Confianza 95% (MCMC)"), col = c("steelblue1", "steelblue", NA, NA), lwd = c(1, 1, NA, NA), lty = c(1, 1, NA, NA), fill = c(NA, NA, adjustcolor("steelblue1", alpha.f = 0.2), adjustcolor("steelblue", alpha.f = 0.2)), bty = "n")
 
 filterMCMC_var <- dlmSvd2var(filterMCMC$U.C, filterMCMC$D.C)
 filterMLE_var <- dlmSvd2var(filterMLE$U.C,filterMLE$D.C)
 filterMCMC_var_diag <- sapply(filterMCMC_var, function(x) x[3, 3])
 filterMLE_var_diag <- sapply(filterMLE_var, function(x) x[3, 3])
 filterMCMC_lower <- filterMCMC$m[,3] -qnorm(0.025, lower=FALSE) * sqrt(filterMCMC_var_diag)/sqrt(132)
 filterMCMC_upper <- filterMCMC$m[,3] + qnorm(0.025, lower=FALSE)* sqrt(filterMCMC_var_diag)/sqrt(132)
 filterMLE_lower <- filterMLE$m[,3] - qnorm(0.025, lower=FALSE)* sqrt(filterMLE_var_diag)/sqrt(132)
 filterMLE_upper <- filterMLE$m[,3] + qnorm(0.025, lower=FALSE)* sqrt(filterMLE_var_diag)/sqrt(132)
 ylim0 <- range(c(filterMLE$m[,3], filterMCMC$m[,3], filterMCMC_lower, filterMCMC_upper, filterMLE_lower, filterMLE_upper))
 plot(year1, filterMLE$m[,3], col = "steelblue1", type = "l", lwd = 1, main = "", xlab = "", ylab = "", ylim = ylim0)
 lines(year1, filterMCMC$m[,3], col = "steelblue", type = "l", lwd = 1)
 polygon(c(year1, rev(year1)), c(filterMLE_lower, rev(filterMLE_upper)), col = adjustcolor("steelblue1", alpha.f = 0.2), border = NA)
 polygon(c(year1, rev(year1)), c(filterMCMC_lower, rev(filterMCMC_upper)), col = adjustcolor("steelblue", alpha.f = 0.2), border = NA)
 mtext(expression(beta[2]), side = 2, line = 2.5)
 legend("topright", legend = c("EMV", "MCMC", "Intervalo de Confianza 95% (EMV)", "Intervalo de Confianza 95% (MCMC)"), col = c("steelblue1", "steelblue", NA, NA), lwd = c(1, 1, NA, NA), lty = c(1, 1, NA, NA), fill = c(NA, NA, adjustcolor("steelblue1", alpha.f = 0.2), adjustcolor("steelblue", alpha.f = 0.2)), bty = "n")
 
 plot(year1, filterMLE$m[,1], col = "steelblue1",type = "l", lwd = 1, main = "", xlab = "", ylab = "")
 lines(year1, filterMCMC$m[,1], col = "steelblue",type = "l", lwd = 1)
 mtext(expression(beta[0]), side = 2, line = 2.5)
 legend("bottomright", legend = c("EMV", "MCMC"), col = c("steelblue1", "steelblue"), lwd = 1, bty = "n")
 
 plot(year1, filterMLE$m[,2], col = "steelblue1",type = "l", lwd = 1, main = "", xlab = "", ylab = "")
 lines(year1, filterMCMC$m[,2], col = "steelblue",type = "l", lwd = 1)
 mtext(expression(beta[1]), side = 2, line = 2.5)
 legend("bottomright", legend = c("EMV", "MCMC"), col = c("steelblue1", "steelblue"), lwd = 1, bty = "n")

 plot(year1, filterMLE$m[,3], col = "steelblue1",type = "l", lwd = 1, main = "", xlab = "", ylab = "")
 lines(year1, filterMCMC$m[,3], col = "steelblue",type = "l", lwd = 1)
 mtext(expression(beta[2]), side = 2, line = 2.5)
 legend("bottomright", legend = c("EMV", "MCMC"), col = c("steelblue1", "steelblue"), lwd = 1, bty = "n")
 
 # estados FFBS
 plot(year1, dpc_FFBS[,1], col = "orchid3", type = "l", lwd = 1, main = "", xlab = "", ylab = "")
 mtext(expression(beta[0]), side = 2, line = 2.5)
 
 plot(year1, dpc_FFBS[,2], col = "orchid3", type = "l", lwd = 1, main = "", xlab = "", ylab = "")
 mtext(expression(beta[1]), side = 2, line = 2.5)
 
 plot(year1, dpc_FFBS[,3], col = "orchid3", type = "l", lwd = 1, main = "", xlab = "", ylab = "")
 mtext(expression(beta[2]), side = 2, line = 2.5)

 
    ## gráfico replicado del paper!
 
 y_range <- range(c(us.ts[, "price_infl"], dpc_FFBS[,2]))
 
 plot(year1,dpc_FFBS[,2], type = "l", col = "orchid4", xlab = "", ylab = "", ylim = y_range)
 par(new = TRUE)
 plot(us.ts[, "price_infl"], type = "l", col = "navy",  axes = FALSE, xlab = "", ylab = "%", ylim = y_range)
 abline(h = 0, col = "grey46", lty = 2)
 axis(side = 4, at = pretty(range(dpc_FFBS[,2])))
 mtext(expression(beta[1]), side = 4, line = 3)
 legend("topright", legend = c("Inflación de precios", expression(beta[1])), col = c("navy", "orchid4"), lty = 1, bty = "n")
 

 qqnorm(residuals(filterMCMC)$res)
 qqline(residuals(filterMCMC)$res, col = "red")
 
 qqnorm(residuals(filterMLE)$res)
 qqline(residuals(filterMLE)$res, col = "red")
 
 shapiro.test(residuals(filterMCMC)$res)
 shapiro.test(residuals(filterMLE)$res)
 
 tsdiag(filterMLE)
 tsdiag(filterMCMC)
 
 # métricas de error
 
 # MAE - mean absolute error 
 
 plot(year, abs(us.ts[, "wage_infl"] - onesa_forecastMCMC) ,  type = "l", xlab="", ylab="")
 mean(abs(us.ts[, "wage_infl"] - onesa_forecastMCMC))
 
 plot(year, abs(us.ts[, "wage_infl"] - dpcMCMC_perf),  type = "l", xlab="", ylab="")
 mean(abs(us.ts[, "wage_infl"] - dpcMCMC_perf))
 
 # MSE - mean square error
 
 plot(year, (us.ts[, "wage_infl"] - onesa_forecastMCMC)^2,  type = "l", xlab="", ylab="")
 mean((us.ts[, "wage_infl"] - onesa_forecastMCMC)^2)
 
 plot(year,  (us.ts[, "wage_infl"] - dpcMCMC_perf)^2, type = "l", xlab="", ylab="")
 mean((us.ts[, "wage_infl"] - dpcMCMC_perf)^2)
 
 # RMSE - Root mean square error
 
 mean(sqrt((us.ts[, "wage_infl"] - onesa_forecastMCMC)^2))
 mean(sqrt((us.ts[, "wage_infl"] - dpcMCMC_perf)^2))

