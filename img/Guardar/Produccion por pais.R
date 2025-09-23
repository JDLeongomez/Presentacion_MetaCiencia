library(tidyverse)
library(viridis)
library(readxl)
library(scales)
library(ggpubr)

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") %>% 
  mutate(Country = COUNTRY)

dat.sjr <- read_excel("scimagojr country rank 2020.xlsx") %>%
  mutate(Country = recode(Country, "Russian Federation" = "Russia"))

dat.sjr <- left_join(x = dat.sjr, y= df, by = "Country")

dat.ppc <- read_excel("prod.pc.xlsx") %>% 
  mutate(Publicaciones = `Number of scientific publications (2020)`) %>% 
  mutate(`Publicaciones per capita` = `Scientific publications per capita (in ppm)`)

dat.ppc <- left_join(x = dat.ppc, y= df, by = "Country")

world <- map_data(map = "world") %>% 
  rename(Country = region)

p <- world %>% 
  merge(dat.ppc, by.x = "Country", all.x = TRUE) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = `Publicaciones per capita`)) + 
  geom_polygon() +
  scale_fill_continuous(labels = comma, 
                        type = "viridis",
                        name = "Publicaciones\nper capita") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(angle = 45, hjust = 1))

p2 <- world %>% 
  merge(dat.sjr, by.x = "Country", all.x = TRUE) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = `H index`)) + 
  geom_polygon() +
  scale_fill_continuous(labels = comma, 
                        type = "viridis",
                        name = "Índice H") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(angle = 45, hjust = 1))

map.fin <- annotate_figure(ggarrange(p,p2, 
                          ncol = 2),
                top = text_grob("Producción científica - 2020",
                                face = "bold", size = 14),
                bottom = text_grob("Fuentes: Scimago Journal & Country Rank",
                                   hjust = 1.1, x = 1, size = 10))
library(plotly)

pl1 <- plot_ly(dat.ppc, 
        type='choropleth', 
        locations=dat.ppc$CODE, 
        z=dat.ppc$`Publicaciones per capita`)%>%
  layout(title = 'Publicaciones (por millón de habitantes)')

pl2 <- plot_ly(dat.sjr, 
               type='choropleth', 
               locations=dat.sjr$CODE, 
               z=dat.sjr$`H index`)%>%
  layout(title = 'Índice H')

subplot(pl1, pl2)

plot_ly(data = dat.ppc, x = ~dat.ppc$GDP..BILLIONS., 
        y = ~dat.ppc$`Number of scientific publications (2020)`, 
        type = 'scatter', 
        mode = 'markers',
        marker = list(size = 7, 
                      color = "#FF9999", 
                      line = list(color = "#CC0000", width = 2))) %>%
  layout(title = "<b> Correlation Scatter Plot", xaxis = list(title = "blablabla"), 
         yaxis = list(title = "blablabla"), showlegend = FALSE)
