


library(dplyr)
library(ggplot2)
library(here)

# Data Load and Setup
data <- read.csv(here('data', 'PiGu_exDat.csv'),
                 header = TRUE,
                 stringsAsFactors = FALSE)

head(data)

data_means <- data %>%
  group_by(year, mo, site) %>%
  summarize(temp = mean(temp, na.rm = T),
            count = mean(PG_count),
            tide = mean(v))






# Scatterplot ----



ggplot(data, aes(yday, temp)) +
  geom_point()



## Customize Colors & Labels

ggplot(data, aes(yday, temp)) +
  geom_point(color = 'blue') +
  xlab('Year Day') +
  ylab('Temperature')


# Cange our color to repersent month.



ggplot(data, aes(yday, temp)) +
  geom_point(aes(color = factor(mo))) +
  xlab('Year Day') +
  ylab('Temperature')


# Customize
# Change colors, sizes, and legend labels
# Overlapping points: alpha for transparency or position = 'jitter'



ggplot(data, aes(yday, temp)) +
  geom_point(aes(color = factor(site), size = year, alpha = 0.2),
             show.legend = F) +
  xlab('Year Day') +
  ylab('Temperature')




# Box Plots



ggplot(data, aes(factor(mo), temp)) +
  geom_boxplot(aes(color = factor(mo))) + #try geom_violin()!
  xlab('Month') + ylab('Temperature')


# Histogram

data %>%
  ggplot(aes(PG_count)) +
  geom_histogram()


## Faceting


ggplot(data, aes(factor(mo), temp)) +
  geom_boxplot(aes(color = factor(mo))) +
  xlab('Month') +
  ylab('Temperature') +
  facet_wrap(~year)




## Multiple geom layers

data_means %>%
  filter(site == 'Cliffside') %>%
  ggplot(aes(factor(year), temp, col = factor(mo), group = factor(mo))) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Temperature')



## Multiple geom layers

data %>%
  ggplot(aes(PG_count)) +
  geom_histogram() +
  geom_density()


# Stat layers


data_means %>%
  filter(site == 'Cliffside') %>%  #look at different sites, or facet by site below
  ggplot(aes(factor(year), temp, col = factor(mo), group = factor(mo))) +
  geom_point(size = 0.8) +
  geom_line(linetype = 'dashed') + #se = F, method = 'lm'
  geom_smooth(aes(fill = factor(mo)), alpha = 0.2, show.legend = F) +
  #or, instead, use geom_ribbon() if have own 95% CI
  #geom_ribbon(aes(mean = mean, ymin = lower, ymax = upper)) +
  xlab('') +
  ylab('Temperature')
  # facet_wrap(~site)



## Stat layers


ggplot(data_means,
       aes(tide, count, group = year)) +
  geom_point(size = 0.8) +
  # geom_line(linetype = 'dashed') + #se = F, method = 'lm'
  geom_smooth(method = 'lm') +
  xlab('Tide Height') + ylab('Seabird count') +
  facet_wrap(~year, scales = 'free_y')


