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
pnl <- pnl %>% mutate(month = factor(month, levels = clevel))

# select the year, month and the reading_kwh variables

pnl <- pnl %>% select(year, month, reading_kwh)
#
# rearrange pnl according to month
pnl <- arrange(pnl, month)
#
# read the expected kWh for SE England
#
exp_kwh <- read_csv("solar_pv_expected_southeast.csv")

# for each month 
#  1.	create plot showing production over the years - kwh 
#  2.	create plot showing the deviation from the expected - delta(kwh - exp_kwh)
#  3.	calculate the mean and sd statistics of generated kwh - (mean(kwh), sd(kwh))
#  4.	calculate the delta (difference) between mean_produced and exp_kwh
# activities 1 to 4 are performed with a for_loop over the 12 months

delta_mean <- data.frame(month = clevel, delta = 1:12)
#
i <- 1
#################for (i in 1:12){
  xm <- clevel[i]
  df <- filter(pnl, month == xm)
  df %>% 
     ggplot(aes(x = month, y = reading_kwh)) +
     geom_boxplot() +
     geom_point(aes(color = year)) +
     geom_hline(yintercept = exp_kwh$exp_kwh[i], linetype = 2,
                color = "red", size = 1) +
     geom_text(aes(x = 0.5, y = exp_kwh$exp_kwh[i],
                   label = "exp_kwh") , vjust = -0.5) +
     ggtitle(paste0("produced kwh for  ", xm,
                   "  between  ", min(df$year),
                   "  and  ", max(df$year)))
  df %>% mutate(year = factor(year)) %>%
    mutate(exp_kwh = rep(exp_kwh$exp_kwh[i],nrow(df)), 
           group = seq(1, nrow(df), 1)) %>%
    ggplot(aes(x = year, y = reading_kwh)) +
    geom_boxplot()+
    geom_point(aes(color = year)) +
    geom_hline(yintercept = exp_kwh$exp_kwh[i], linetype = 2,
               color = "red", size = 1) +
    geom_point(aes(y = exp_kwh, color = "red")) +
    ggtitle(paste0("produced kwh for  ", xm,
                   "  between  ", min(df$year),
                   "  and  ", max(df$year))) +
    geom_text(aes(x = 1.0, y = exp_kwh,
                  label = "exp_kwh") , vjust = -0.5)
  df_stat <- df %>% summarise(
                avg_kwh = mean(reading_kwh),
                sd_kwh = sd(reading_kwh))
  delta_mean$delta[i] <- df_stat$avg_kwh - exp_kwh$exp_kwh[i]
################}
  
# plot kwh production over the years

  pnl <- pnl %>% mutate(year = factor(year))
  total <- pnl %>% group_by(year) %>%
     summarise(total = sum(reading_kwh))
  
  total_exp <- sum(exp_kwh$exp_kwh)
  total_exp
  
  
  p <- total %>% 
    filter(as.numeric(year) > 1) %>%
    ggplot(aes(x = year, y = total)) +
    ylim(2500, 4000)
  
  p + geom_point(color = "blue") +
    geom_hline(yintercept = total_exp,
               linetype = 2, 
               color = "grey", 
               size = 1) + 
    ggtitle(paste0(
      "yearly produced kwh between 2012 and 2020")) +
    geom_text(aes(x = 3, y = total_exp,
              label = "exp_yearly_kwh") , 
              vjust = 1, color= "red", cex = 3)
  
  
