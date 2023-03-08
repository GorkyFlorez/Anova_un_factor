# crear un data frame con los datos
datos <- data.frame(
  grupo = rep(c("testigo", "tratamiento1", "tratamiento2", "tratamiento4"), each = 4),
  porcentaje = c(80, 73.3, 82.1, 72.9, 97.8, 96.1, 96.7, 96.8, 96.1, 95.1, 95.4, 98.3, 78.3, 82, 87.1, 85.1)
)
datos
# realizar el análisis de varianza
modelo <- aov(porcentaje ~ grupo, data = datos)
summary(modelo)

# ajustar el modelo
modelo <- aov(porcentaje ~ grupo, data = datos)

# gráfico de qq-plot
par(mfrow=c(1,1))
qqnorm(resid(modelo), main = "Gráfico de qq-plot")
qqline(resid(modelo), col = 2)

# test de Shapiro-Wilk
shapiro.test(resid(modelo))

# gráfico de dispersión
plot(fitted(modelo), resid(modelo), main = "Gráfico de dispersión")

# test de Bartlett
bartlett.test(resid(modelo) ~ grupo, data = datos)


# gráfico de caja y bigotes
boxplot(porcentaje ~ grupo, data = datos, main = "Gráfico de caja y bigotes")


## Load the required libraries
library("ggplot2") 
library("tidyr") 
library("dplyr") 
library("cowplot") 
library("RColorBrewer") 
library("ggpubr") 
source("halfViolinPlots.R") 


datos <- data.frame(
  grupo = rep(c("testigo", "Trat 1", "Trat 2", "Trat 3"), each = 4),
  porcentaje = rnorm(80, mean = 68.5, sd = 14.5)
)

# realizar el análisis de varianza
modelo <- aov(porcentaje ~ grupo, data = datos)
summary(modelo)

library(agricolae)
A=HSD.test(modelo, "grup", group = TRUE)
A
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")
library(ggplot2)
## And plot the data
Expo= ggplot(datos, aes(x = grupo, y = porcentaje, fill = grupo, color = grupo)) +
  ggtitle("") +
  ylab("Crecimiento longitudinal (cm)") +
  xlab("Tratamiento") +
  theme_cowplot() +
  scale_shape_identity() +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  geom_point(position = position_jitter(0.15), 
             size = 1.5, 
             alpha = 0.6, 
             aes(shape = 15)) +
  geom_flat_violin(position = position_nudge(x = 0.25, y = 0),
                   adjust = 2,
                   alpha = 0.6, 
                   trim = TRUE, 
                   scale = "width") +
  geom_boxplot(
              
               width = 0.3, 
               varwidth = TRUE, 
               outlier.shape = NA, 
               alpha = 0.35, 
               colour = "black", 
               show.legend = FALSE) +
  stat_compare_means(aes(label = ..p.adj..),size = 2,
                     method = "wilcox.test", 
                     comparisons = list(c("testigo","Trat 1"), c("Trat 2","testigo"), c("Trat 2","Trat 3"))) +
  stat_compare_means(method = "anova", 
                     label.y = 12,size = 3)+
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5),
        axis.text.y  = element_text(color="black",
                                    family="serif",size=7),
        axis.text.x  = element_text(color="black",
                                    family="serif",size=7),
        axis.title = element_text(size = 9,family="serif",face="bold"),
        legend.position = "none",
        legend.text=element_text(size=6, family="serif"),
        legend.title = element_text(size=6, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.2,"cm"), #ancho de cuadrados de referencia 
        plot.caption = element_text(size = 8, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = margin(t = 15)),
        plot.background = element_rect(fill = "white", color=NA),
        
  )
Expo

ggsave(plot=Expo ,"Grafico de anova.png",units = "cm",width = 10, #ancho
       height = 10, #alargo
       dpi=1200)


