library(tidyverse)
library(printr)
setwd("~/Dropbox/R/genetics-master")


# Load data ---------------------------------------------------------------

data <-  as.tibble(read.csv("data/pipetting.csv"))


# Add error column --------------------------------------------------------

data <- data %>% mutate(error = (actual_volume-target_volume)/target_volume*100, fractional_volume = target_volume/(pipette_size*10^(-3))*100)

# order the pipette names for plotting by pipette size

data$pipette_name <- factor(data$pipette_name, levels = c("1 ml", "5 ml", "10 ml", "20 ml","P10","P20","P200","P1000"))

by_pipette <- data %>% group_by(pipette_name)

by_pipette %>% summarise(mean(error),sd(error))

# Plot data ---------------------------------------------------------------

# Error vs. fractional volume
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


#Violin plot grouped by pipette size
ggplot(data = by_pipette) +
  geom_violin(mapping = aes(x = pipette_name, y = error, fill = pipette_name), 
              draw_quantiles = c(0.25, 0.5, 0.75), 
              adjust = .5, show.legend = FALSE, scale = "width") +
  labs(y = "Error (%)", x = "Pipette size") 



#Box plot
ggplot(data = by_pipette) +
  geom_boxplot(mapping = aes(x = pipette_name, y = error, fill = pipette_name),
               show.legend = TRUE) +
  labs(y = "Error (%)", x = "Pipette size") +
  scale_fill_discrete(name = "Student")

ggplot(data = data) +
  geom_count(mapping = aes(x = pipette_name, y = error, color = student),
               show.legend = TRUE) +
  labs(y = "Error (%)", x = "Pipette size") +
  scale_fill_discrete(name = "Student")

# Histogram overlaid with kernel density curve
ggplot(data = data) + 
  geom_histogram(mapping = aes(x = error, y=..density..,fill=pipette_name,color=pipette_name),
                 binwidth=2, position = "identity", alpha=0.4) +
  geom_density(aes(x = error,fill=pipette_name,color=pipette_name),alpha=0.7) +
  stat_function(fun=dnorm,
                color="red", alpha=0.4,
                args=list(mean=mean(data$error), 
                          sd=sd(data$error)))
  
