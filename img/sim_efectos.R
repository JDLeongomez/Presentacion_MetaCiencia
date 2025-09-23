library(tidyverse)
library(gganimate)
library(glue)

set.seed(1234)

# --- Parámetros ---
n_poblacion <- 7000
n_estudios  <- 1000
n_muestra   <- 100
fps_out     <- 20

# Correlación absurda (ρ = 0):
x_lab <- "Minutos de pataleta del gato por día"
y_lab <- "Número de empanadas soñadas por semana"

# --- Población sin relación ---
pobl <- tibble(
  id = 1:n_poblacion,
  x  = rnorm(n_poblacion, 30, 10),
  y  = rnorm(n_poblacion,  0,  1)
)

# --- Submuestras por estudio ---
idx_list <- replicate(n_estudios, sample.int(n_poblacion, n_muestra), simplify = FALSE)

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

stats_list <- lapply(seq_len(n_estudios), \(k) calc_stats(idx_list[[k]]))
z_vec  <- map_dbl(stats_list, "z")
r_vec  <- map_dbl(stats_list, "r")
b1_vec <- map_dbl(stats_list, "b1")
b0_vec <- map_dbl(stats_list, "b0")
sig_vec <- abs(z_vec) > 1.96

# Datos Panel A: muestras y rectas
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

# Etiqueta dinámica Panel A (estática en esquina)
x_ann <- quantile(pobl$x, 0.02); y_ann <- quantile(pobl$y, 0.98)
anotA <- rectas_df %>%
  transmute(frame, panel,
            x = x_ann, y = y_ann,
            label = glue("Estudio {frame} | r={round(r,2)}  z={round(z,2)}  |  |z|>1.96: {ifelse(sig,'Sí','No')}"))

# Panel B: histograma acumulado
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

anotB <- hist_acum_df %>%
  group_by(frame, panel) %>%
  summarise(prop_sig = first(prop_sig), n_pos = first(n_pos), n_neg = first(n_neg), .groups = "drop") %>%
  mutate(label = glue("Estudios: {frame} | P(|z|>1.96) ≈ {scales::percent(prop_sig, 0.1)} | +:{n_pos}  -:{n_neg}"))

# Colores de significancia para la submuestra
col_sig <- c(`FALSE` = "#1b9e77", `TRUE` = "#e76f51")

# --- Un solo plot con facetas (A y B) ---
p_comb <- ggplot() +
  facet_wrap(~panel, ncol = 2, scales = "free") +
  
  # Panel A: población (capa estática en todos los frames)
  geom_point(data = pobl %>% mutate(panel = "A"),
             aes(x, y), colour = "grey75", size = 0.6, alpha = 0.7, inherit.aes = FALSE) +
  # Panel A: submuestra y recta por frame
  geom_point(data = muestras_df, aes(x, y, colour = as.character(sig)),
             size = 1.6, alpha = 0.95, show.legend = FALSE) +
  geom_abline(data = rectas_df, aes(slope = slope, intercept = intercept, colour = as.character(sig)),
              linewidth = 0.9, show.legend = FALSE) +
  geom_label(data = anotA, aes(x = x, y = y, label = label),
             size = 3.6, label.size = 0, alpha = 0.85) +
  
  # Panel B: histograma acumulado de z
  geom_col(
    data = hist_acum_df %>% mutate(sig_bin = abs(bin) >= 1.96 - binw/2),
    aes(x = bin, y = count, group = bin, fill = sig_bin),
    width = binw, colour = "grey25"
  ) +
  scale_fill_manual(values = c("FALSE"="#1b9e77","TRUE"="#e76f51"), guide = "none") +
  geom_vline(data = data.frame(panel = "B"), aes(xintercept = -1.96), linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = data.frame(panel = "B"), aes(xintercept =  1.96), linetype = "dashed", linewidth = 0.8) +
  geom_label(data = anotB, aes(x = max(breaks) - 0.2, y = Inf, label = label),
             size = 3.4, vjust = 1.5, hjust = 1.0, label.size = 0, inherit.aes = FALSE) +
  
  scale_colour_manual(values = col_sig) +
  labs(
    title = "Variación muestral con efecto verdadero = 0",
    subtitle = "Panel A: población (ρ = 0) y submuestra | Panel B: histograma acumulado de z",
    x = x_lab, y = y_lab
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")) +
  transition_states(
    frame,                # <- este es tu estado
    transition_length = 0,
    state_length = 1,
    wrap = FALSE
  )

anim <- animate(
  p_comb,
  nframes = n_estudios,
  fps = fps_out,
  width = 1200, height = 600,
  end_pause = 60,                      # ~2–3 s de pausa final (ajústalo)
  renderer = gifski_renderer(loop = FALSE)  # no repetir
)
anim_save("simulacion_estudios_fast.gif", anim)
