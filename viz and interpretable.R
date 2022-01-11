
# Visualization

suppressMessages({
  library(tidyverse)
})

the_data_raw <- read.csv('C:/Users/samue/Documents/to_r_tdab_raw.csv')
the_data_full <- read.csv('C:/Users/samue/Documents/to_r_tdab_full.csv')
the_data_super <- read.csv('C:/Users/samue/Documents/to_r_tdab_super.csv')

# Add by hand as notebook was running
colnames(the_data_full) <- c('DateTime', 'CurrentSpeed', 'CurrentDir', 'TWS', 'TWA', 'AWS', 'AWA',
                             'Roll', 'Pitch', 'HeadingMag', 'HoG', 'HeadingTrue', 'AirTemp',
                             'Long', 'Lat', 'SoG', 'SoS', 'AvgSoS', 'VMG', 'RudderAng',
                             'Leeway', 'TWD', 'WSoG', 'VoltageDrawn', 'Yaw', 'ModePilote', 'Tacking')

check_diff <- the_data_full %>%
  group_by(ModePilote) %>%
  select(-DateTime) %>%
  summarize(across(everything(), mean))

mod_1 <- lm(VMG ~ .-(DateTime + AvgSoS + HoG + HeadingTrue + Tacking + ModePilote), 
          data = the_data_full[the_data_full$ModePilote == 1,])

mod_0 <- lm(VMG ~ .-(DateTime + AvgSoS + HoG + HeadingTrue + Tacking + ModePilote), 
            data = the_data_full[the_data_full$ModePilote == 0,])

summary(mod_1)
summary(mod_0)

# Checking data 30min before tacking
start_tack <- the_data_raw %>%
  rownames_to_column() %>%
  filter(Tacking != lag(Tacking) & Tacking == 1) %>%
  select(rowname) %>%
  mutate(rowname = as.numeric(rowname))

the_data_raw$before_tack = NA

for(i in start_tack$rowname) {
  the_data_raw$before_tack[(i-60*30):(i-1)] <- 1:1800
}

data_before_tack <- the_data_raw[!is.na(the_data_raw$before_tack),]

data_before_tack <- data_before_tack %>%
  group_by(before_tack) %>%
  select(-DateTime, -X, -time_to_tack) %>%
  summarize(across(everything(), mean))

# CurrentSpeed
ggplot(data_before_tack, aes(-before_tack, CurrentSpeed)) +
  geom_line()

# CurrentDir
ggplot(data_before_tack, aes(-before_tack, CurrentDir)) +
  geom_line()

# Roll
ggplot(data_before_tack, aes(-before_tack, Roll)) +
  geom_line()

# Pitch
ggplot(data_before_tack, aes(-before_tack, Pitch)) +
  geom_line()

# VMG
ggplot(data_before_tack, aes(-before_tack, VMG)) +
  geom_line()

# HeadingMag
ggplot(data_before_tack, aes(-before_tack, VoltageDrawn)) +
  geom_line()


