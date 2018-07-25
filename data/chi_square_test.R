library(tidyverse)
library(printr)
setwd("~/Dropbox/R/genetics-master")

# Mono hybrid cross -------------------------------------------------------

#load and sum data by category
mono_data <- read.csv("data/monohybrid_cross.csv")
observed_mono <- c(sum(mono_data$Purple),sum(mono_data$Yellow))

knitr::kable(mono_data, digits = 2, caption = "Monohybrid cross data for Pearson's chi-squared test.")

# chi squared by hand
expected_mono <- c(0.75*sum(mono_data), 0.25*sum(mono_data))
chi_stat_mono <- sum((observed_mono-expected_mono)^2/expected_mono)
p_value_mono <- 1-pchisq(chi_stat_mono,1)

# chi squared using R function
p_null_mono <- c(0.75,0.25)
chisqu_mono <- chisq.test(observed_mono,p=p_null_mono)


data_mono = tibble(Color=c(rep("Purple",length(mono_data$Purple)),
               rep("Yellow",length(mono_data$Yellow))), Frequency=c(mono_data$Purple, mono_data$Yellow))
               
ggplot(data=data_mono) +
  geom_col(mapping = aes(x = Color, y=Frequency, fill = Color),
           show.legend = FALSE) +
           scale_fill_manual("legend",
           values = c("Purple" = "#42165b", "Yellow" = "gold"))

# Dihybrid cross ----------------------------------------------------------

#load and sum data by category
di_data <- read.csv("data/dihybrid_cross.csv")
observed_di <- c(sum(di_data$PurpleSmooth),sum(di_data$PurpleWrinkled),
                 sum(di_data$YellowSmooth),sum(di_data$YellowWrinkled))

# chi squared by hand
expected_di <- c(9/16*sum(di_data), 3/16*sum(di_data),
                  3/16*sum(di_data), 1/16*sum(di_data))
chi_stat_di <- sum((observed_di-expected_di)^2/expected_di)
p_value_di <- 1-pchisq(chi_stat_di,1)

# chi squared using R function
p_null_di <- c(9/16,3/16,3/16,1/16)
chisqu_di <- chisq.test(observed_di,p=p_null_di)

#test for color only

obs_di <- c(sum(di_data$PurpleSmooth) + sum(di_data$PurpleWrinkled),
            sum(di_data$YellowSmooth) + sum(di_data$YellowWrinkled))
chisqu_di <- chisq.test(obs_di,p=p_null_mono)


# plot chi squared denisity function
#monohybrid
curve( dchisq(x, df=1), col='red', main = "Chi-Square Density Graph", from=0,to=60)
xvec <- seq(2.5,60,length=101)
pvec <- dchisq(xvec,df=1)
polygon(c(xvec,rev(xvec)),c(pvec,rep(0,length(pvec))), col=adjustcolor("black",alpha=0.3))

# dihybrid
curve( dchisq(x, df=3), col='red', main = "Chi-Square Density Graph", from=0,to=60)
xvec <- seq(35.6,60,length=101)
pvec <- dchisq(xvec,df=3)
polygon(c(xvec,rev(xvec)),c(pvec,rep(0,length(pvec))), col=adjustcolor("black",alpha=0.3))


#get cut-off values for significance 1-p, i.e. p<0.05 -> 1 -0.05 = 0.95
qchisq(0.95, df=1)


# same using ggplot
#function to add area under the curve
funcShaded_1 <- function(x) {
  y <- dchisq(x, df=1)
  y[x < 3.841459 ] <- NA
  return(y)
}

funcShaded_3 <- function(x) {
  y <- dchisq(x, df=3)
  y[x < 7.814728 ] <- NA
  return(y)
}


ggplot(data.frame(x = c(0, 15)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = 1), aes(color = "k=1"), size = 1.5) +
  stat_function(fun = dchisq, args = list(df = 3), aes(color = "k=3"), size = 1.5) +
  scale_x_continuous(name = "chi-squared", breaks = seq(0, 40, 1)) +
  scale_y_continuous(name = "Probability") +
  ggtitle("Chi-squared distribution") +
  scale_colour_manual("Degrees of freedom", values = c("blue", "red")) +
  # theme(axis.line = element_line(size=1, colour = "black"),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       plot.title=element_text(size = 20, family="xkcd-Regular"),
  #       text=element_text(size = 16, family="xkcd-Regular"),
  #       axis.text.x=element_text(colour="black", size = 12),
  #       axis.text.y=element_text(colour="black", size = 12)) +
  stat_function(fun=funcShaded_1, geom="area", fill="blue", alpha=0.2) +
  stat_function(fun=funcShaded_3, geom="area", fill="red", alpha=0.2)
