# ------------------------------------------------------------
# Producción científica por país (2024): per cápita e índice H
# Autor: Juan David Leongómez Peña
# Proyecto: MetaCiencia
#
# Descripción:
#   - Descarga SJR (países) directamente desde la web (parseo HTML).
#   - Carga población 2023 (OWID), normaliza nombres y mapea ISO3.
#   - Une con geometrías (rnaturalearth) usando ISO3 "limpio".
#   - Genera dos mapas:
#       (1) Publicaciones por millón (log, gris→magenta).
#       (2) Índice H (log, gris→magenta).
#   - Ensambla ambos en una figura con título y fuente.
#
# Requisitos:
#   tidyverse, readr, rvest, janitor, scales, countrycode, sf, rnaturalearth,
#   stringi, ggpubr
#
# Salida:
#   Objeto 'map.fin'. Guarda con ggsave() si quieres.
# ------------------------------------------------------------

# Paquetes ----
library(tidyverse)
library(readr)
library(rvest)
library(janitor)
library(scales)
library(countrycode)
library(sf)
library(rnaturalearth)
library(stringi)
library(ggpubr)   # ggarrange(), annotate_figure(), text_grob()

# ---- Función: leer SJR (tabla HTML) --------------------------------------
# La URL 'out=csv/xls' a veces devuelve HTML; por eso parseamos la tabla.
# Devuelve columnas con nombres esperados para el pipeline:
#   Country, Documents, H index, (y otras si están)
leer_sjr_html <- function(year = 2024) {
  url <- sprintf("https://www.scimagojr.com/countryrank.php?year=%d", year)
  doc <- read_html(url)
  tb  <- html_table(doc, fill = TRUE)[[1]] %>%  # primera tabla grande
    remove_empty("cols") %>%
    clean_names() %>%
    rename(
      Country                     = any_of("country"),
      Documents                   = any_of("documents"),
      `Citable documents`         = any_of("citable_documents"),
      Citations                   = any_of("citations"),
      `Self-citations`            = any_of("self_citations"),
      `Citations per document`    = any_of("citations_per_document"),
      `H index`                   = any_of("h_index")
    ) %>%
    mutate(
      across(c(Documents, `Citable documents`, Citations, `Self-citations`,
               `Citations per document`, `H index`),
             ~ suppressWarnings(readr::parse_number(as.character(.x))))
    )
  tb
}

# --- SJR + ISO3 ------------------------------------------------------------
# Descarga SJR (países) para 2024
dat.sjr <- leer_sjr_html(2024)

# Normaliza nombres (quita acentos/espacios) y mapea a ISO3.
# Se añaden 'custom_match' para casos frecuentes/conflictivos.
dat.sjr_iso <- dat.sjr |>
  mutate(
    Country_norm = trimws(stri_trans_general(Country, "Latin-ASCII")),
    CODE = countrycode(
      Country_norm, "country.name", "iso3c",
      custom_match = c(
        "Russian Federation" = "RUS", "Czech Republic" = "CZE",
        "Congo, Dem. Rep." = "COD", "Congo, Rep." = "COG",
        "Viet Nam" = "VNM", "Ivory Coast" = "CIV",
        "Tanzania" = "TZA", "Korea, Rep." = "KOR",
        "Korea, South" = "KOR", "Korea, North" = "PRK",
        "Hong Kong" = "HKG", "Macao" = "MAC",
        "Haiti" = "HTI",
        "Saint Martin (French)" = "MAF",
        "Saint Martin (Dutch)"  = "SXM",
        # variantes a veces presentes
        "France" = "FRA", "Norway" = "NOR",
        "United Kingdom" = "GBR", "United States" = "USA"
      ),
      warn = TRUE
    )
  )

# --- Población 2023 (millones) --------------------------------------------
# Fuente: Our World in Data (UN WPP 2024), año 2023.
pop_owid <- read_csv(
  "https://ourworldindata.org/grapher/population.csv",
  show_col_types = FALSE
) |>
  filter(Year == 2023, nchar(Code) == 3) |>
  transmute(CODE = Code, pop_2023 = `Population (historical)` / 1e6)

# Publicaciones por millón de habitantes
dat.ppm <- dat.sjr_iso |>
  left_join(pop_owid, by = "CODE") |>
  mutate(`Publicaciones por millón` = Documents / pop_2023)

# --- Geometrías con ISO3 "limpio" -----------------------------------------
# En rnaturalearth algunos registros traen iso_a3 = "-99".
# Construimos 'iso3_clean' priorizando iso_a3, luego iso_a3_eh, y si no adm0_a3.
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(4326) |>
  mutate(
    iso3_clean = dplyr::case_when(
      iso_a3    != "-99" ~ iso_a3,
      iso_a3_eh != "-99" ~ iso_a3_eh,
      TRUE               ~ adm0_a3
    )
  ) |>
  select(name_long, iso3_clean, geometry)

# Une SJR+población al mapa
map_sf <- world_sf |>
  left_join(
    dat.ppm |> select(CODE, `Publicaciones por millón`, `H index`, Documents),
    by = c("iso3_clean" = "CODE")
  ) |>
  # 0 → NA (usaremos trans = "log10")
  mutate(ppm = na_if(`Publicaciones por millón`, 0))

# --- Escalas logarítmicas y puntos de control -----------------------------
# Construimos límites, cortes y "stops" (values) *por variable* para no mezclar.

# 1) Publicaciones por millón
lims_ppm  <- c(1, max(map_sf$ppm, na.rm = TRUE))
brks_all  <- c(1, 10, 100, 1000, 10000)
brks_ppm  <- brks_all[brks_all >= lims_ppm[1] & brks_all <= lims_ppm[2]]
# Mantén gris hasta ~100, empieza a teñir a ~1500, magenta fuerte > 2000
vals_num_ppm <- c(lims_ppm[1], 100, 1500, 2000, lims_ppm[2])
vals_ppm <- rescale(log10(vals_num_ppm), to = c(0, 1), from = log10(lims_ppm))

# 2) Índice H (si hay ceros de H, se hacen NA para la escala log)
map_sf <- map_sf |>
  mutate(h_log = ifelse(`H index` <= 0 | is.na(`H index`), NA_real_, `H index`))
lims_h   <- c(1, max(map_sf$h_log, na.rm = TRUE))
brks_h   <- brks_all[brks_all >= lims_h[1] & brks_all <= lims_h[2]]
vals_num_h <- c(lims_h[1], 100, 800, 2000, lims_h[2])
vals_h <- rescale(log10(vals_num_h), to = c(0, 1), from = log10(lims_h))

# Paleta (gris → magenta MetaCiencia)
pal_mc <- c("#3b3b3b", "#3b3b3b", "#7a5b73", "#b03fa0", "#d400aa")

# --- Mapas individuales ----------------------------------------------------
# (A) Índice H
p1 <- ggplot(map_sf) +
  geom_sf(aes(fill = h_log), color = "grey30", linewidth = 0.1) +
  scale_fill_gradientn(
    colours = pal_mc, values = vals_h,
    trans = "log10", limits = lims_h,
    breaks = brks_h, labels = comma,
    na.value = "grey80", name = "Índice H",
    oob = squish
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(angle = 45, hjust = 1)
  )

# (B) Publicaciones por millón
p2 <- ggplot(map_sf) +
  geom_sf(aes(fill = ppm), color = "grey30", linewidth = 0.1) +
  scale_fill_gradientn(
    colours = pal_mc, values = vals_ppm,
    trans = "log10", limits = lims_ppm,
    breaks = brks_ppm, labels = comma,
    na.value = "grey80", name = "Publicaciones por millón\nde habitantes",
    oob = squish
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(angle = 45, hjust = 1)
  )

# --- Figura final (dos paneles + título y fuente) --------------------------
map.fin <- annotate_figure(
  ggarrange(p2, p1, ncol = 2),
  top    = text_grob("Producción científica - 2024", face = "bold", size = 14),
  bottom = text_grob("Fuentes: Scimago Journal & Country Rank", hjust = 1.1, x = 1, size = 10)
)

# Visualiza en el visor
map.fin

# (Opcional) Guardar en alta resolución:
ggsave("produccion_cientifica_2024.png", map.fin, width = 12, height = 5, dpi = 300)
