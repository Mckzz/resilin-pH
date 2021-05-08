install.packages("tidyverse")
library(tidyverse)
library(ggplot2)


rm(pH_series)

is.data.frame(pH_series)
str(pH_series)


pH_series_norm7 <- pH_series %>%
  mutate_at(vars(pH), factor) %>%
  rename(pct.diff = diff) %>%
  mutate(pct.diff_norm7 = pct.diff - 2.42) %>%
  mutate(zeroline= 0)

print(pH_series_norm7)

means <-
  pH_series_norm7 %>%
  group_by(pH) %>% ## group by pH
  ## now compute mean and sd:
  summarize(across(everything(), 
                   tibble::lst(mean = mean, sd = sd))) %>%
  mutate(zeroline= 0)

#means_norm7 <-
 # means %>%

print(means)


ggplot(pH_series_norm7, aes(x = pH)) +
  geom_jitter(aes(y= pct.diff_norm7), color = "firebrick", size = 2, width = 0.15, pch = 1) +
  geom_line(data = pH_series_norm7, aes(y= zeroline)) +
  geom_point(data = means, 
             aes(x = pH, y = pct.diff_norm7_mean),
             size= 3) +
  geom_errorbar(data= means,
                mapping = aes(x = pH,
                              ymin = pct.diff_norm7_mean - pct.diff_norm7_sd,
                              ymax = pct.diff_norm7_mean + pct.diff_norm7_sd), 
                width = 0.3,
                size = 0.5) +
  geom_line(aes(y= zeroline), size = 5) +
  labs(x = "pH", y = "% change") + #labels axes
  theme_classic()   #takes out background
  


##

ggplot(pH_series, aes(pH, diff)) +
  geom_jitter(color = "firebrick", size = 2, width = 0.15, pch = 1) +
  labs(x = "pH", y = "% change") + #labels axes
  theme_classic() +  #takes out background
  stat_summary(
    fun.data = mean_sdl, geom = "errorbar", width = 0.2, fun.args = list(mult=1)) +
  stat_summary(
    fun = mean, geom = "point", 
    size = 3)

view(pH_series)

########################################################################################################

stripchart(diff~pH, data= pH_series, vertical = TRUE, method = "jitter", 
           jitter = 0.2, cex.axis = 0.8, pch = 1, col = "firebrick")


stripchart(diff ~ pH, data = pH_series, vertical = TRUE, method = "jitter", pch = 1)
m <- tapply(pH_series$diff, pH_series$pH, mean, na.rm=TRUE)
se <- tapply(pH_series$diff, pH_series$pH, 
             function(y){ sd(y, na.rm=TRUE)/sqrt(length(na.omit(y))) })
points( m ~ c(2:11 + 0.2) + 0.2, pch=16, col = "red")
segments(x0 = c(2:11 + 0.2), y0 = m - se, 
   x1 = c(2:11 + 0.2), y1 = m + se, col = "red")



e <- ggplot(pH_series, aes(x = pH, y = diff))
e + stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1))

plot(pH_series$diff~pH_series$pH, data= pH_series)


ggplot(pH_series, aes(x=pH, y=diff)) + 
 geom_jitter(size = 2, width = 0.1) +
stat_summary(fun.data = mean_sdl, 
            geom = "errorbar", 
           fun.args = list(mult=1),
          width = 0.1, 
         position=position_nudge(x = 0))






