# ------------------------------------------------------------
# Distribución de valores p a medida que aumenta el poder
# Autor: Juan David Leongómez Peña
# Proyecto: MetaCiencia
#
# Descripción:
#   Genera una animación (GIF) de histogramas de valores p para una prueba t
#   de dos muestras con varianzas iguales, manteniendo n fijo y aumentando
#   el poder objetivo. Para el primer estado (poder = 0.05) se fuerza δ = 0
#   para representar la hipótesis nula verdadera y obtener distribución
#   uniforme de p (en [0, 1]).
#
# Requisitos:
#   - R >= 4.x
#   - tidyverse (ggplot2, dplyr, tibble, purrr, etc.)
#   - gganimate
#   - gifski (renderer por defecto para animaciones en GIF)
#
# Salida:
#   - distribucion_pvals_poder.gif (en el directorio de trabajo)
# ------------------------------------------------------------

# Paquetes ----
library(tidyverse)  # incluye ggplot2, dplyr, tibble, purrr, etc.
library(gganimate)
library(gifski)

# Semilla reproducible (afecta las simulaciones) ----
set.seed(2025)

# Parámetros ----
alpha        <- 0.05   # Nivel de significancia. Valores p < alpha se consideran "significativos".
n_por_grupo  <- 50     # Tamaño de muestra por grupo en cada simulación.
iter_por_nvl <- 50000  # Nº de simulaciones por nivel de poder (grano fino para histogramas suaves).
poderes_obj  <- c(alpha, seq(0.10, 1.00, by = 0.05))
# Valores objetivo de poder a representar.
# El primer estado corresponde a poder = 0.05 (con δ = 0, hipótesis nula verdadera).

# Colores ----
col_signif <- "#d400aa"  # MetaCiencia (magenta) para "Significativo"
col_nsign  <- "#3b3b3b"  # Gris oscuro para "No significativo"

# δ para un poder dado (prueba t de dos colas, dos muestras, varianzas iguales) ----
# Nota: bajo alternativa, power.t.test() devuelve el tamaño de efecto (delta)
# necesario para alcanzar el "power" deseado con n, alpha y sd = 1.
delta_para_poder <- function(power, n, alpha = 0.05) {
  power.t.test(
    n         = n,
    power     = power,
    sd        = 1,
    sig.level = alpha,
    type      = "two.sample",
    alternative = "two.sided"
  )$delta
}

# Tabla de estados (poder y δ). Forzamos δ = 0 cuando poder = alpha ----
estados <- tibble(
  poder = poderes_obj,
  delta = c(
    0,  # δ = 0 → H0 verdadera → p ~ U(0,1) → poder ≈ alpha
    sapply(poderes_obj[-1], function(p) delta_para_poder(p, n_por_grupo, alpha))
  )
)

# Función de simulación de p-values para un δ dado ----
# Genera dos muestras normales independientes con sd = 1 y diferencia de medias δ.
# Realiza una prueba t (dos muestras, var.equal = TRUE) y devuelve un tibble con p y etiqueta.
simular_pvals <- function(delta, n, iter = 2000, alpha = 0.05) {
  pvals <- replicate(iter, {
    a <- rnorm(n, mean = 0, sd = 1)
    b <- rnorm(n, mean = delta, sd = 1)
    t.test(a, b, var.equal = TRUE)$p.value
  })
  tibble(
    p = pvals,
    significancia = factor(
      ifelse(p < alpha, "Significativo", "No significativo"),
      levels = c("No significativo", "Significativo")  # ordena leyenda
    )
  )
}

# Construcción del data frame para la animación (concatena todos los estados) ----
# Para cada par (poder, δ) simulamos 'iter_por_nvl' p-values y etiquetamos el estado.
df_anim <- purrr::pmap_dfr(estados, function(poder, delta) {
  sim <- simular_pvals(delta, n_por_grupo, iter_por_nvl, alpha)
  sim$poder <- poder
  sim
})

# Gráfico base + animación ----
# - Histograma de p con densidad relativa (suma a 1 por estado).
# - Escala X en [0,1]; eje Y en proporción (formato %).
# - Eje Y "libre" por fotograma con view_follow().
g <- ggplot(df_anim, aes(x = p, fill = significancia)) +
  geom_histogram(
    bins = 100, boundary = 0, closed = "left",
    aes(y = after_stat(count / sum(count)))
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_fill_manual(
    name   = "Significancia",
    values = c("No significativo" = col_nsign, "Significativo" = col_signif),
    breaks = c("No significativo", "Significativo")
  ) +
  labs(
    x = "valor p",
    y = "Probabilidad",
    title = "Poder = {sprintf('%.2f', as.numeric(closest_state))}"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) +
  transition_states(poder, transition_length = 3, state_length = 8) +
  view_follow(fixed_x = TRUE, fixed_y = FALSE) +  # permite variar el eje Y por estado
  ease_aes("linear")

# Exportar GIF ----
# Ajusta 'nframes' y 'fps' si quieres animación más fluida o más corta.
fps_ <- 12
animate(
  g,
  nframes = nrow(estados) * 8,
  fps     = fps_,
  width   = 800,
  height  = 600,
  end_pause = 2 * fps_,
  renderer = gifski_renderer("distribucion_pvals_poder.gif")
)
