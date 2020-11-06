install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
install.packages("Hmisc")

rm(pH_series)

is.data.frame(pH_series)
print(pH_series)
str(pH_series)




pH_series <- as_tibble(pH_series)

pH_series$pH <- as.factor(pH_series$pH)
is.factor(pH_series$pH)
pH_series$pH <- as.character(pH_series$pH)

ggplot(pH_series, aes(pH, diff)) +
  geom_jitter(color = "firebrick", size = 2, width = 0.15, pch = 1) +
  labs(x = "pH", y = "% change") + #labels axes
  theme_classic() +  #takes out background
  stat_summary(
    fun.data = mean_sdl, geom = "errorbar", width = 0.2, fun.args = list(mult=1)) +
  stat_summary(
    fun = mean, geom = "point", 
    size = 3)


stripchart(diff~pH, data= pH_series, vertical = TRUE, method = "jitter", 
           jitter = 0.2, cex.axis = 0.8, pch = 1, col = "firebrick")

###

#stripchart(diff ~ pH, data = pH_series, vertical = TRUE, method = "jitter", pch = 1)
#m <- tapply(pH_series$diff, pH_series$pH, mean, na.rm=TRUE)
#se <- tapply(pH_series$diff, pH_series$pH, 
#             function(y){ sd(y, na.rm=TRUE)/sqrt(length(na.omit(y))) })
#points( m ~ c(2:11 + 0.2) + 0.2, pch=16, col = "red")
#segments(x0 = c(2:11 + 0.2), y0 = m - se, 
#   x1 = c(2:11 + 0.2), y1 = m + se, col = "red")



#e <- ggplot(pH_series, aes(x = pH, y = diff))
#e + stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1))

#plot(pH_series$diff~pH_series$pH, data= pH_series)


#ggplot(pH_series, aes(x=pH, y=diff)) + 
# geom_jitter(size = 2, width = 0.1) +
#stat_summary(fun.data = mean_sdl, 
#            geom = "errorbar", 
#           fun.args = list(mult=1),
#          width = 0.1, 
#         position=position_nudge(x = 0))






