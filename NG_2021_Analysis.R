# 2021 Nutritional Geometry Data Analysis - Santiago Meneses.

# Loading necessary graphing packages (ggplot2).
library(tidyverse)
library(interp)

# set working directory
baseDir <- "~/Documents/Research/Nutritional_Specialization/Summer2021_NG_Data"

# Importing dataset of A. picea Nutritional Geometry.
apheno <- read_csv(paste0(baseDir,"/Rudis_Two-Choice.csv"))

# Separate species data in two with dplyr.
picea_total <- dplyr::filter(apheno, Species == "A. picea")
rudis_total <-dplyr::filter(apheno, Species =="A. rudis")

# Interpolate data with interp::interp. 
picea_int <- interp::interp(picea_total$PercentageHarvestedProtein, 
                            picea_total$PercentageHarvestedCarbohydrate,
                            picea_total$TotalHarvestedNutrients,
                            xo = seq(min(picea_total$PercentageHarvestedProtein),
                                     max(picea_total$PercentageHarvestedProtein),
                                     lenght = 12),
                            yo = seq(min(picea_total$PercentageHarvestedCarbohydrate),
                                     max(picea_total$PercentageHarvestedCarbohydrate),
                                     length = 12),
                            method = "linear",
                            input = "points",
                            duplicate = "mean")
# Make interpolated matrix with function interp2xyz.

# Make a surface plot with geom_contour_filled

# Make a point/scatter plot
aphaeno_point <- 
  ggplot(apheno, 
         aes(HarvestedProtein, 
         HarvestedCarbohydrate)) +
         geom_abline(intercept = 0, slope = 6, alpha = 1/2) +
         geom_abline(intercept = 0, slope = 3, alpha = 1/2) +
         geom_abline(intercept = 0, slope = 0.3333, alpha = 1/2) +
         geom_abline(intercept = 0, slope = 0.16667, alpha = 1/2) +
         annotate("text", x = 22, y = 150, angle = 80, label = "1:6")+
         annotate("text", x = 47, y = 150, angle = 70,label = "1:3")+
         annotate("text", x = 150, y = 54, angle = 17,label = "3:1")+
         annotate("text", x = 150, y = 29, angle = 12,label = "6:1")+
         geom_point(aes(color = Species,
                        fill = Species,
                 size = TotalHarvestedNutrients,
                 shape = Diet)) +
         scale_color_manual(values = c("#c51b7d", "#4d9221")) +
         theme_bw() + 
         theme (
           plot.title = element_text(face = "bold"),
           axis.ticks = element_line(color = "grey70", size = 0.2),
           axis.text.x = element_text(face = "bold"),
           axis.text.y = element_text(face = "bold"),
           axis.title.x = element_text(face = "bold"),
           axis.title.y = element_text(face = "bold"),
           legend.title = element_text(face = "bold"),
           legend.text = element_text(face = "bold"),
           panel.grid.major = element_line(color = "grey70", size = 0.2),
           panel.grid.minor = element_blank(),
           legend.position = c(0.8, 0.8),
           legend.background = element_rect(fill = "grey",
                                            size = 0.5,
                                            linetype = "solid",
                                            color = "black")
         )  +
       scale_size_continuous(guide = "none")+
       ylim(0,150)+
       xlim(0,150)+
       ggtitle("Total Nutrient Harvest by Species") +
       ylab("Harvested Carbohydrate (mg)") +
       xlab("Harvested Protein (mg)")

aphaeno_point
# Save plot
ggsave("Total_Nutrient_Harvest.svg", width = 6, height = 4)

