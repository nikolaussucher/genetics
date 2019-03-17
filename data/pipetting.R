library(tidyverse)
library(printr)
setwd("~/Dropbox/R/genetics-master")


# Load data ---------------------------------------------------------------

data <-  as.tibble(read.csv("data/pipetting.csv"))


# Add error column --------------------------------------------------------

data <- data %>% mutate(error = (actual_volume-target_volume)/target_volume*100, fractional_volume = target_volume/(pipette_size*10^(-3))*100)

# Plot data ---------------------------------------------------------------

# order the pipette names for plotting by pipette size

data$pipette_name <- factor(data$pipette_name, levels = c("1 ml", "5 ml", "10 ml", "20 ml","P10","P20","P200","P1000"))

ggplot(data=data) +
  geom_point(mapping = aes(x = fractional_volume, y = error, shape = student, color = pipette_name),
             size = 4, show.legend = TRUE) +
  scale_color_discrete(name = "Pipette size") +
  scale_shape_discrete(name = "Student") +
  geom_smooth(mapping = aes(x = fractional_volume, y = error),
              show.legend = FALSE, size = 2) +
  labs(y = "Error (%)", x = "Fraction of maximal pipette volume (%)") +
  theme(axis.text.x=element_text(family="sans"),axis.text.y=element_text(family="sans"))

#Violin plot
ggplot(data = data) +
  geom_violin(mapping = aes(x = pipette_name, y = error, fill = student), 
              draw_quantiles = c(0.25, 0.5, 0.75), 
              adjust = .5, show.legend = TRUE, scale = "width") +
  labs(y = "Error (%)", x = "Pipette size") +
  scale_fill_discrete(name = "Student")

#Box plot
ggplot(data = data) +
  geom_boxplot(mapping = aes(x = pipette_name, y = error, fill = student),
               show.legend = TRUE) +
  labs(y = "Error (%)", x = "Pipette size") +
  scale_fill_discrete(name = "Student")


