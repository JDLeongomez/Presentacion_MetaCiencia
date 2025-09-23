# Paquetes
library(tidyverse)

# --- Usa los mismos parámetros que en tu animación ---
set.seed(1234)          # <- igual que antes
n_poblacion <- 7000
# Etiquetas (ajústalas a las que usaste en la animación)
x_lab <- "Minutos de pataleta del gato por día"
y_lab <- "Número de empanadas soñadas por semana"

# Distribución de X y Y (ρ = 0), igual que en la animación
pobl <- tibble(
  id = 1:n_poblacion,
  x  = rnorm(n_poblacion, 30, 10),
  y  = rnorm(n_poblacion,  0,  1)
)

# Definición de bins para el histograma (igual que en la animación)
breaks <- seq(-4, 4, by = 0.2)
binw   <- diff(breaks)[1]
mids   <- head(breaks, -1) + binw/2

# Histograma "vacío": conteos = 0 en todos los bins
hist_vacio <- tibble(
  panel = "B",
  bin   = mids,
  count = 0
)

# Para que haya espacio para la etiqueta, forzamos un rango Y razonable en el panel B
# (no dibuja nada, solo fija el rango)
rango_y_b <- tibble(panel = "B", bin = 0, count = 20)  # ajusta 20 si quieres más/menos altura

# Etiqueta del panel B
etiqueta_B <- tibble(
  panel = "B",
  x = max(breaks) - 0.2,
  y = 19,  # cerca de la parte alta del rango
  label = "Estudios: 0 | P(|z|>1.96) ≈ 0% | +:0  -:0"
)

# Plot combinado (facetas A y B)
p_static <- ggplot() +
  facet_wrap(~panel, ncol = 2, scales = "free") +

  # Panel A: población (sin submuestra)
  geom_point(
    data = pobl %>% mutate(panel = "A"),
    aes(x, y),
    colour = "grey75", size = 0.6, alpha = 0.7, inherit.aes = FALSE
  ) +

  # Panel B: histograma vacío
  geom_col(
    data = hist_vacio,
    aes(x = bin, y = count, group = bin),
    width = binw, fill = "grey70", colour = "grey25"
  ) +
  # Forzar rango Y del panel B sin dibujar nada
  geom_blank(data = rango_y_b, aes(x = bin, y = count)) +
  # Líneas de significancia
  geom_vline(
    data = data.frame(panel = "B"),
    aes(xintercept = -1.96), linetype = "dashed", linewidth = 0.8
  ) +
  geom_vline(
    data = data.frame(panel = "B"),
    aes(xintercept =  1.96), linetype = "dashed", linewidth = 0.8
  ) +
  # Etiqueta superior derecha del panel B
  geom_label(
    data = etiqueta_B,
    aes(x = x, y = y, label = label),
    size = 3.6, vjust = 1.0, hjust = 1.0, label.size = 0, alpha = 0.9
  ) +

  # Estilo y textos (coherentes con tu animación)
  labs(
    title = "Variación muestral con efecto verdadero = 0",
    subtitle = "Panel A: población (ρ = 0) y submuestra | Panel B: histograma acumulado de z",
    x = x_lab, y = y_lab
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

dpi_match <- 72          # o 96 si tu GIF se ve más “pequeño”
ggsave(
  "variacion_muestral_base_estatica.png",
  p_static,
  width  = 1200 / dpi_match,   # pulgadas
  height =  600 / dpi_match,   # pulgadas
  units  = "in",
  dpi    = dpi_match
)
