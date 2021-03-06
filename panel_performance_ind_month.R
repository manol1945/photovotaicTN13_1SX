# this procedure uses the solar panel readings from installation to end of 2020 and examines 
# panel performance for each month comparing with the expected kwh production that was
# provided during the installation based on data up to 2011.
#
# read_csv solar panel readings
file <- "solar_pv_monthly_reading_orchardsend_aug2011_to_dec2020_rev2.csv"
#
pnl <- read_csv(file)    
#
library(tidyverse) # add this package to make use of %>% "pipe"

# store order of months as in calender 
clevel <- c("jan", "feb", "mar", 
            "apr", "may", "jun", 
            "jul", "aug", "sep", 
            "oct", "nov", "dec")

# turn month to factor with levels = clevel
pnl <- pnl %>% mutate(month = factor(month,
                      levels = clevel))

# select the year, month and the reading_kwh variables

pnl <- pnl %>% select(year, month, reading_kwh)
#
# read the expected kWh for SE England
#
exp_kwh <- read_csv("solar_pv_expected_southeast.csv")

#### january ######
###################
# select the observation for january

pnl_jan <- filter(pnl, month == "jan")
head(pnl_jan)

# the performance by month is shown in plot_1
pnl_jan %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))


# the performance by year in shown in plot_3
pnl_jan %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  mutate(exp_kwh = exp_kwh$exp_kwh[1], 
         group = seq(1, nrow(pnl_jan), 1)) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production January 2012 to 2020") +
  geom_hline(yintercept = exp_kwh$exp_kwh[1]) +
  geom_point(aes(y = exp_kwh, color = "red")) 

# investigate the statistics of kWh over the years 
pnl_jan_stats <- pnl_jan %>% 
    summarise(mean_kwh = mean(reading_kwh),
              sd_kwh = sd(reading_kwh))
pnl_jan_stats
delta_mean_jan <- pnl_jan_stats$mean_kwh - exp_kwh$exp_kwh[1]
#
#### february ######
####################
pnl_feb <- filter(pnl, month == "feb")
head(pnl_feb)

# the performance by month is shown in plot_1
pnl_feb %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))


# the performance by year in shown in plot_3
pnl_feb %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production February 2012 to 2020")

# investigate the statistics of kWh over the years 
pnl_feb_stats <- pnl_feb %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_feb_stats
delta_mean_feb <- pnl_feb_stats$mean_kwh - exp_kwh$exp_kwh[2]
#
#### march ######
####################
pnl_mar <- filter(pnl, month == "mar")
head(pnl_mar)

# the performance by month is shown in plot_1
pnl_mar %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))


# the performance by year in shown in plot_3
pnl_mar %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production March 2012 to 2020")

# investigate the statistics of kWh over the years 
pnl_mar_stats <- pnl_mar %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_mar_stats
delta_mean_mar <- pnl_mar_stats$mean_kwh - exp_kwh$exp_kwh[3]
#
#
#### april ######
####################
pnl_apr <- filter(pnl, month == "apr")
head(pnl_apr)

# the performance by month is shown in plot_1
pnl_apr %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))


# the performance by year in shown in plot_3
pnl_apr %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production April 2012 to 2020")

# investigate the statistics of kWh over the years 
pnl_apr_stats <- pnl_apr %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_apr_stats
delta_mean_apr <- pnl_apr_stats$mean_kwh - exp_kwh$exp_kwh[4]
#
#
#### may ######
####################
pnl_may <- filter(pnl, month == "may")
head(pnl_may)

# the performance by month is shown in plot_1
pnl_may %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))


# the performance by year in shown in plot_3
pnl_may %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production May 2012 to 2020")

# investigate the statistics of kWh over the years 
pnl_may_stats <- pnl_may %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_may_stats
delta_mean_may <- pnl_may_stats$mean_kwh - exp_kwh$exp_kwh[5]
#
#### june ######
####################
pnl_jun <- filter(pnl, month == "jun")
head(pnl_may)

# the performance by month is shown in plot_1
pnl_jun %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))


# the performance by year in shown in plot_3
pnl_jun %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production June 2012 to 2020")

# investigate the statistics of kWh over the years 
pnl_jun_stats <- pnl_jun %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_jun_stats
delta_mean_jun <- pnl_jun_stats$mean_kwh - exp_kwh$exp_kwh[6]
#
#### july ######
####################
pnl_jul <- filter(pnl, month == "jul")
head(pnl_jul)

# the performance by month is shown in plot_1
pnl_jul %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))

# the performance by year in shown in plot_3
pnl_jul %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production July 2012 to 2020")

# investigate the statistics of kWh over the years 
pnl_jul_stats <- pnl_jul %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_jul_stats
delta_mean_jul <- pnl_jul_stats$mean_kwh - exp_kwh$exp_kwh[7]
#
#
#### august ######
####################
pnl_aug <- filter(pnl, month == "aug")
head(pnl_aug)

# the performance by month is shown in plot_1
pnl_aug %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))

# the performance by year in shown in plot_3
pnl_aug %>% mutate(year = factor(year)) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production August 2011 to 2020")

# investigate the statistics of kWh over the years 
pnl_aug_stats <- pnl_aug %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_aug_stats
delta_mean_aug <- pnl_aug_stats$mean_kwh - exp_kwh$exp_kwh[8]
#
#
#### september ######
####################
pnl_sep <- filter(pnl, month == "sep")
head(pnl_sep)

# the performance by month is shown in plot_1
pnl_sep %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))

# the performance by year in shown in plot_3
pnl_sep %>% mutate(year = factor(year)) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production September 2011 to 2020")

# investigate the statistics of kWh over the years 
pnl_sep_stats <- pnl_sep %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_sep_stats
delta_mean_sep <- pnl_sep_stats$mean_kwh - exp_kwh$exp_kwh[9]
#
#
#### october ######
####################
pnl_oct <- filter(pnl, month == "oct")
head(pnl_nov)

# the performance by month is shown in plot_1
pnl_oct %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))

# the performance by year in shown in plot_3
pnl_oct %>% mutate(year = factor(year)) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production October 2011 to 2020")

# investigate the statistics of kWh over the years 
pnl_oct_stats <- pnl_oct %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_oct_stats
delta_mean_oct <- pnl_oct_stats$mean_kwh - exp_kwh$exp_kwh[10]
#
#
#### november ######
####################
pnl_nov <- filter(pnl, month == "nov")
head(pnl_nov)

# the performance by month is shown in plot_1
pnl_nov %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))

# the performance by year in shown in plot_3
pnl_nov %>% mutate(year = factor(year)) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production November 2011 to 2020")

# investigate the statistics of kWh over the years 
pnl_nov_stats <- pnl_nov %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_nov_stats
delta_mean_nov <- pnl_nov_stats$mean_kwh - exp_kwh$exp_kwh[11]
#
#
#### december######
####################
pnl_dec <- filter(pnl, month == "dec")
head(pnl_dec)

# the performance by month is shown in plot_1
pnl_dec %>% 
  ggplot(aes(x = month, y = reading_kwh)) +
  geom_boxplot() +
  geom_point(aes(color = year))

# the performance by year in shown in plot_3
pnl_dec %>% mutate(year = factor(year)) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = reading_kwh)) +
  geom_boxplot()+
  geom_point(aes(color = year)) +
  ggtitle("kWh production December 2011 to 2020")

# investigate the statistics of kWh over the years 
pnl_dec_stats <- pnl_dec %>% 
  summarise(mean_kwh = mean(reading_kwh),
            sd_kwh = sd(reading_kwh))
pnl_dec_stats
delta_mean_dec <- pnl_dec_stats$mean_kwh - exp_kwh$exp_kwh[12]
#
## mean measured kWh ####
#
delta_mean_jan <- pnl_jan_stats$mean_kwh - exp_kwh$exp_kwh[1]
delta_mean_feb <- pnl_feb_stats$mean_kwh - exp_kwh$exp_kwh[2]
delta_mean_mar <- pnl_mar_stats$mean_kwh - exp_kwh$exp_kwh[3]
delta_mean_apr <- pnl_apr_stats$mean_kwh - exp_kwh$exp_kwh[4]
delta_mean_may <- pnl_may_stats$mean_kwh - exp_kwh$exp_kwh[5]
delta_mean_jun <- pnl_jun_stats$mean_kwh - exp_kwh$exp_kwh[6]
delta_mean_jul <- pnl_jul_stats$mean_kwh - exp_kwh$exp_kwh[7]
delta_mean_aug <- pnl_aug_stats$mean_kwh - exp_kwh$exp_kwh[8]
delta_mean_sep <- pnl_sep_stats$mean_kwh - exp_kwh$exp_kwh[9]
delta_mean_oct <- pnl_oct_stats$mean_kwh - exp_kwh$exp_kwh[10]
delta_mean_nov <- pnl_nov_stats$mean_kwh - exp_kwh$exp_kwh[11]
delta_mean_dec <- pnl_dec_stats$mean_kwh - exp_kwh$exp_kwh[12]
#
df_delta <- data.frame(month = factor(c("jan", "feb", "mar", 
                                 "apr", "may", "jun", 
                                 "jul", "aug", "sep", 
                                 "oct", "nov", "dec"),
                                 levels = clevel),
          delta_mean = c(delta_mean_jan, delta_mean_feb,
                         delta_mean_mar, delta_mean_apr,
                         delta_mean_may, delta_mean_jun,
                         delta_mean_jul, delta_mean_aug,
                         delta_mean_sep, delta_mean_oct,
                         delta_mean_nov, delta_mean_dec))
#
avg_delta <- df_delta %>% 
  summarise(avg_delta = mean(delta_mean))
#
df_delta %>% 
  group_by(month) %>%
  ggplot(aes(x = month, y = delta_mean)) +
  geom_point(color = "red", size = 3, shape = 2) +
  geom_hline(yintercept = 0,  
            color = "blue", size = 1.5, linetype = 2)

###############
# the performance by year in shown in plot_3
p <- pnl_jan %>% mutate(year = factor(year)) %>%
  filter(year != 2011) %>%
  mutate(exp_kwh = exp_kwh$exp_kwh[1], 
         group = seq(1, nrow(pnl_jan), 1)) %>%
  group_by(year)

  ggplot(p, aes(x = year, y = kwh)) +
  geom_point(aes(color = year)) +
  ggtitle("kWh production January 2012 to 2020") +
  geom_hline(yintercept = exp_kwh$exp_kwh[1]) +
  geom_line(aes(y = kwh, group = group)) +
  geom_text(aes(1.5,exp_kwh$exp_kwh[1], label = "exp_kwh",
                vjust = -0.2 )) +
  geom_text(aes(0.9,120, label = "delta_kwh(typ)",
                vjust = 0, srt = 90))
    
  
p1 <- select(p, year, month, reading_kwh, group)
names(p1)[3] <- "kwh"
p2 <- select(p, year, month, exp_kwh, group)
names(p2)[3] <- "kwh"
p <- bind_rows(p1, p2)
  










       
       