# ------------------------------------------------------------
# Simulación de "muchos estudios" con efecto verdadero nulo (ρ = 0)
# Autor: Juan David Leongómez Peña
# Proyecto: MetaCiencia
#
# Descripción:
#   Este script genera un GIF con dos paneles que ilustran la variación muestral
#   cuando el efecto verdadero es exactamente cero (correlación ρ = 0).
#
#   - Panel A: en cada "estudio" se toma una submuestra aleatoria de una
#     población sin relación (x ⫫ y), se ajusta un modelo lineal y se dibuja
#     la recta estimada. Se colorea por "significativo" si |z| > 1.96.
#   - Panel B: se construye un histograma acumulado de los z (t) observados a
#     través de los estudios, mostrando cuántos caen en las colas |z| > 1.96.
#
#   El objetivo es mostrar que, incluso con ρ = 0, algunas submuestras producirán
#   estimaciones "significativas" por puro azar, y cómo esa proporción converge
#   hacia el nivel nominal a medida que crece el número de estudios.
#
# Requisitos:
#   - tidyverse (ggplot2, dplyr, tibble, purrr, readr, etc.)
#   - gganimate
#   - glue
#   - gifski (para renderizar el GIF)
#
# Salida:
#   - simulacion_estudios_fast.gif
#
# Notas:
#   - El estadístico 't' del coeficiente de x (en lm) se aproxima a normal(0,1)
#     para n suficientemente grande; aquí lo referimos como "z" por brevedad.
#   - Ajusta n_estudios, n_muestra, y los ejes/colores según tu preferencia.
# ------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(glue)

set.seed(1234)

# Parámetros principales ----
n_poblacion <- 7000   # Tamaño de la población sintética sin relación entre x e y
n_estudios  <- 1000   # Número de "estudios" (submuestras) a simular
n_muestra   <- 100    # Tamaño muestral por estudio (por submuestra)
fps_out     <- 20     # Cuadros por segundo del GIF resultante

# Etiquetas (ejemplo con variables absurdas para enfatizar ρ = 0) ----
x_lab <- "Minutos de pataleta del gato por día"
y_lab <- "Número de empanadas soñadas por semana"

# Población sin relación (ρ = 0) ----
# x ~ N(30, 10^2), y ~ N(0, 1^2), independientes.
pobl <- tibble(
  id = 1:n_poblacion,
  x  = rnorm(n_poblacion, mean = 30, sd = 10),
  y  = rnorm(n_poblacion, mean = 0,  sd = 1)
)

# Índices de submuestras por estudio ----
# Para cada estudio se muestrean n_muestra individuos de la población sin reemplazo.
idx_list <- replicate(n_estudios, sample.int(n_poblacion, n_muestra), simplify = FALSE)

# Función para calcular estadísticas por submuestra ----
# Devuelve: z (t del coeficiente de x), r (correlación muestral), b1 (pendiente), b0 (intercepto).
calc_stats <- function(ix) {
  d <- pobl[ix, ]
  fit <- lm(y ~ x, data = d)
  coefs <- summary(fit)$coefficients
  list(
    z   = unname(coefs["x","t value"]),  # t del coeficiente ~ z
    r   = cor(d$x, d$y),
    b1  = coef(fit)[2],
    b0  = coef(fit)[1]
  )
}

# Ejecutar todos los "estudios" y recogemos estadísticas ----
stats_list <- lapply(seq_len(n_estudios), \(k) calc_stats(idx_list[[k]]))
z_vec  <- map_dbl(stats_list, "z")
r_vec  <- map_dbl(stats_list, "r")
b1_vec <- map_dbl(stats_list, "b1")
b0_vec <- map_dbl(stats_list, "b0")

# Significancia (umbral |z| > 1.96) ----
sig_vec <- abs(z_vec) > 1.96

# Datos Panel A: puntos de la submuestra y rectas por frame ----
muestras_df <- map2_dfr(idx_list, seq_along(idx_list), ~{
  d <- pobl[.x, c("x","y")]
  d$frame <- .y
  d
}) %>%
  left_join(tibble(frame = 1:n_estudios, sig = sig_vec), by = "frame") %>%
  mutate(panel = "A")

rectas_df <- tibble(
  frame = 1:n_estudios,
  slope = b1_vec, intercept = b0_vec,
  r = r_vec, z = z_vec, sig = sig_vec,
  panel = "A"
)

# Etiqueta dinámica (estática en esquina del Panel A) ----
# Usamos percentiles de la población para colocar el recuadro dentro del plano.
x_ann <- quantile(pobl$x, 0.02)
y_ann <- quantile(pobl$y, 0.98)

anotA <- rectas_df %>%
  transmute(frame, panel,
            x = x_ann, y = y_ann,
            label = glue("Estudio {frame} | r={round(r,2)}  z={round(z,2)}  |  |z|>1.96: {ifelse(sig,'Sí','No')}"))

# Panel B: histograma acumulado de z ----
# Definimos bins fijos para todos los frames y acumulamos conteos progresivamente.
breaks <- seq(-4, 4, by = 0.2); binw <- diff(breaks)[1]
mids   <- head(breaks, -1) + binw/2

hist_acum_df <- map_dfr(1:n_estudios, function(k) {
  h <- hist(z_vec[1:k], breaks = breaks, plot = FALSE)
  tibble(
    frame = k, panel = "B",
    bin = mids, count = as.numeric(h$counts),
    prop_sig = mean(abs(z_vec[1:k]) > 1.96),
    n_pos = sum(z_vec[1:k] >  1.96),
    n_neg = sum(z_vec[1:k] < -1.96)
  )
})

# Etiqueta dinámica del Panel B con resumen acumulado ----
anotB <- hist_acum_df %>%
  group_by(frame, panel) %>%
  summarise(prop_sig = first(prop_sig), n_pos = first(n_pos), n_neg = first(n_neg), .groups = "drop") %>%
  mutate(label = glue("Estudios: {frame} | P(|z|>1.96) ≈ {scales::percent(prop_sig, 0.1)} | +:{n_pos}  -:{n_neg}"))

# Colores de significancia para la submuestra (Panel A) ----
col_sig <- c(`FALSE` = "black", `TRUE` = "#d400aa")

# Plot combinado con facetas (Panel A y Panel B) ----
p_comb <- ggplot() +
  facet_wrap(~panel, ncol = 2, scales = "free") +
  
  # Panel A: población completa (capa estática en todos los frames)
  geom_point(
    data = pobl %>% mutate(panel = "A"),
    aes(x, y),
    colour = "grey75",
    size = 0.6, alpha = 0.7,
    inherit.aes = FALSE
  ) +
  
  # Panel A: submuestra por frame (coloreada por significancia)
  geom_point(
    data = muestras_df,
    aes(x, y, colour = as.character(sig)),
    size = 1.6, alpha = 0.95, show.legend = FALSE
  ) +
  
  # Panel A: recta de regresión por frame
  geom_abline(
    data = rectas_df,
    aes(slope = slope, intercept = intercept, colour = as.character(sig)),
    linewidth = 0.9, show.legend = FALSE
  ) +
  
  # Panel A: etiqueta informativa (r, z, y significancia) en la esquina
  geom_label(
    data = anotA,
    aes(x = x, y = y, label = label),
    size = 3.6, label.size = 0, alpha = 0.85
  ) +
  
  # Panel B: histograma acumulado de z
  geom_col(
    data = hist_acum_df %>% mutate(sig_bin = abs(bin) >= 1.96 - binw/2),
    aes(x = bin, y = count, group = bin, fill = sig_bin),
    width = binw, colour = "grey25"
  ) +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "#d400aa"), guide = "none") +
  
  # Panel B: líneas de corte ±1.96
  geom_vline(data = data.frame(panel = "B"), aes(xintercept = -1.96),
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = data.frame(panel = "B"), aes(xintercept =  1.96),
             linetype = "dashed", linewidth = 0.8) +
  
  # Panel B: etiqueta acumulada (arriba-derecha)
  geom_label(
    data = anotB,
    aes(x = max(breaks) - 0.2, y = Inf, label = label),
    size = 3.4, vjust = 1.5, hjust = 1.0, label.size = 0, inherit.aes = FALSE
  ) +
  
  # Escalas y tema
  scale_colour_manual(values = col_sig) +
  labs(
    title    = "Variación muestral con efecto verdadero = 0",
    subtitle = "Panel A: población (ρ = 0) y submuestra | Panel B: histograma acumulado de z",
    x = x_lab, y = y_lab
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +
  
  # Animación: el estado es el índice del estudio (frame)
  transition_states(
    frame,
    transition_length = 0,  # sin transición entre estados (salto inmediato)
    state_length      = 1,  # 1 frame por estado
    wrap = FALSE
  )

# Render de la animación ----
# end_pause agrega pausa al final; renderer(loop = FALSE) evita que se repita en bucle.
anim <- animate(
  p_comb,
  nframes   = n_estudios,
  fps       = fps_out,
  width     = 1200, height = 600,
  end_pause = 3*fps_out,                          # ~3 s de pausa final (ajústalo si deseas)
  renderer  = gifski_renderer(loop = FALSE)
)

# Guardar a disco ----
anim_save("simulacion_estudios_fast.gif", animation = anim)
