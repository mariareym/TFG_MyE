
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
 library(tseries)
 library(forecast)
 
 ## Parte 1: Importar y construir base de datos
 
 hicpindex_data <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Matemáticas y Estadística/Datos/prc_hicp_midx_page_spreadsheet.xlsx"
 hicprate_data <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Matemáticas y Estadística/Datos/prc_hicp_manr_page_spreadsheet.xlsx"
 
 EUhipc_index <-read_excel(hicpindex_data, sheet = "Sheet 1", skip = 8)
 EUhicp_rate <- read_excel(hicprate_data, sheet = "Sheet 1", skip = 8)
 
 hipc_index <- data.frame(EUhipc_index)
 hipc_rate <- data.frame(EUhicp_rate)
 
 ## Limpiar y consolidar base de datos
 
 hipc_index <- hipc_index %>%
   slice(2:4) %>%
   mutate(across(-1, as.character))
 
 hipc_index <- pivot_longer(hipc_index, cols = -1, names_to = "date", values_to = "hcpi_index")
 hipc_index <- na.omit(hipc_index)
 
 hipc_rate <- hipc_rate %>%
   slice(2:3) %>%
   mutate(across(-1, as.character))
 
 hipc_rate <- pivot_longer(hipc_rate, cols = -1, names_to = "date", values_to = "hcpi_rate")
 hipc_rate <- na.omit(hipc_rate)
 
 base <- merge(hipc_rate, hipc_index, by = c("date", "TIME"), all.x = TRUE, all.y = FALSE)
 
 base <- base %>%
   rename(
     ue_group = "TIME"
   ) %>%
   mutate(hcpi_rate=as.numeric(hcpi_rate),
          hcpi_index=as.numeric(hcpi_index))
 
 base <- base %>%
   mutate(
     date = as.numeric(str_remove_all(date, "[^0-9.]")), 
     date = as.Date(paste0(date, ".01"), format = "%Y.%m.%d")
   )
 
 base <- base %>%
   mutate(
     group = case_when(ue_group == "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015, EA20-2023)" ~ 1,
                       ue_group == "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)" ~ 2)
   )
 
 ea <- base %>%
   filter(group == 1) %>%
   mutate(log_hcpi_index = log(hcpi_index))
 
 # declaramos panel de datos
 
 ea.ts <- ts(ea, start = c(1997, 1), frequency = 12)
 
 ## Parte 2: Análisis descriptivo 
 
 ggplot(ea, aes(x = date)) +
   geom_line(aes(y = hcpi_rate), color = "orchid4") +
   theme_minimal() + 
   theme(legend.position ="none", 
         axis.title = element_text(size = 8), 
         axis.title.x = element_text(margin = margin(t = 6)),
         axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
   labs(
     colour = NULL,
     x = "Año",
     y = "IPC") +
   ggtitle("IPC rate")
 
 ggplot(data = ea, aes(x = date)) +
   geom_line(aes(y = log_hcpi_index), color = "orchid4") +
   theme_minimal() + 
   theme(legend.position ="none", 
         axis.title = element_text(size = 8), 
         axis.title.x = element_text(margin = margin(t = 6)),
         axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
   labs(
     colour = NULL,
     x = "Año",
     y = "") +
   ggtitle("Log del Índice de Precios al Consumo Armonizado - HCPI")

 ## Parte 3: Modelizar DLM

 Lhcpi_index <- ea.ts[, "log_hcpi_index"]
 
 level0 <- Lhcpi_index[1]
 slope0 <- mean(diff(Lhcpi_index))

 ts.plot(diff(Lhcpi_index), main="", xlab="", ylab="", col="navy")
 
 diff_series <- diff(Lhcpi_index)
 diff_series_ts <- ts(diff_series, frequency = 12)  
 stl_decomp <- stl(diff_series_ts, s.window = "periodic")
 seasonal_adjusted <- seasadj(stl_decomp)
 ts.plot(seasonal_adjusted, main = "", ylab = "",xlab="", col = "navy")
 
 modelo_ar1 <- arima(seasonal_adjusted , order = c(1,0,0))
 modelo_arma <- arima(diff(Lhcpi_index), order = c(12,0,0))
 summary(modelo_ar1)
 summary(modelo_arma)
 acf(seasonal_adjusted, main="")
 pacf(diff(Lhcpi_index), main="")
 
 build_infl_gap0 <- function(u) {
   trend <- dlmModPoly(order = 2, dV = 20, dW = exp(u[1:2]), 
                       m0 = c(level0, slope0),
                       C0 = 2*diag(2))
   gap <- dlmModARMA(ar = u[3], sigma2 = exp(u[4]))
   seas <- dlmModSeas(frequency = 12, dV = exp(u[5]))
   return(trend+gap+seas)
 }
 
 build_infl_gap <- function(u) {
   trend <- dlmModPoly(order = 2, dV = 20, dW = exp(u[1:2]), 
                       m0 = c(level0, slope0),
                       C0 = 2*diag(2))
   seas <- dlmModSeas(frequency = 12, dV = exp(u[3]))
   return(trend+seas)
 }

 # EMV
 
 init <- c(0.2, 0.2, 0.1)
 init1 <- c(1, 1, 1)
 init0 <- c(0.5, 0.5, 0.5, 0.5, 0.5) #cómo se escogen los parámetros iniciales, con qué criterios?
 init01 <- c(1, 1, 1, 1, 1)
 
 outMLE <- dlmMLE(Lhcpi_index, init1, build_infl_gap)
 dlmGAP <- build_infl_gap(outMLE$par)
 
 outMLE$convergence #se debe comprobar siempre
 
 FF(dlmGAP)
 GG(dlmGAP)
 V(dlmGAP)
 W(dlmGAP)
 
 inflSmooth <- dlmSmooth(Lhcpi_index, dlmGAP)
 inflFilter <- dlmFilter(Lhcpi_index, dlmGAP)
 
 plot(cbind(inflFilter$y, inflFilter$s[,1]),
      xlab="", ylab="Log HCPI index", lty = c("longdash", "solid"),
      col = c("darkgrey", "navy"), plot.type = "single")
 
 
 plot(cbind(inflFilter$y, inflFilter$f),
      xlab="", ylab="Log HCPI index", lty = c("longdash", "solid"),
      col = c("darkgrey", "navy"), plot.type = "single")

 plot(inflSmooth$s[,1:4], col=c("navy"), ann = F, yax.flip = TRUE)
 
 series_names <- c("Bloque polinomial: estado 1", "Bloque polinomial: estado 2", "Bloque estacional: estado 3")
 
 # Configurar los márgenes exteriores: c(bottom, left, top, right)
 par(oma = c(5, 4, 2, 2))  # Márgenes exteriores
 par(mfrow = c(1, 3), mar = c(5, 4, 2, 1) + 0.1)  # Configurar el layout para tres gráficos horizontales y márgenes
 
 # Graficar cada serie individualmente y añadir etiquetas
 for (i in 1:4) {
   plot(inflSmooth$s[,i], type = "l", col = "navy", ylab = "", xlab = "", main = series_names[i])
 }

 
