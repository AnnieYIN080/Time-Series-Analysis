library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(zoo)
library(data.table)

path <- '/Users/annie/Downloads/timeseries.csv'

df <- fread(path)

glimpse(df)

filtered <- df %>%
  select( Extension,
          FileSize = "Size",
          ModifiedTime = "Modified")


Timeseries_df <- filtered %>%
  mutate(Date = as.Date(ModifiedTime),
         Year = year(Date),
         Month = month(Date),
         Quarter = quarter(Date),
         Year_Month = paste0(Year, "-", sprintf("%02d", Month)),
         Year_Quarter = paste(Year, "-Q", Quarter,sep = ""),
         Week_of_Month = paste0(Year, "-", sprintf("%02d", Month), "-W", ceiling((day(Date) + lubridate::wday(floor_date(Date, "month")) - 1) / 7)),
         Weekday = lubridate::wday(Date, label = TRUE, abbr = FALSE)  # Sunday is the first day of a week
  )%>%   select(-ModifiedTime)


nrow(Timeseries_df)
head(Timeseries_df)


naRows <- Timeseries_df %>% filter(is.na(Date))
naRows


options(repr.plot.width = 18, repr.plot.height = 8.5)


ggplot(Timeseries_df, aes(x = Year_Month, y = Quarter, fill = total_size)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("Q1", "Q2", "Q3", "Q4")
  ) +
  labs(title = "Monthly File Size by Quarter", x = "Month", y = "Quarter")


# Plot of year with size
yearly_sum <- Timeseries_df %>%
  group_by(Year) %>%
  summarise(yearly_size = sum(as.numeric(FileSize))) %>% # Convert 'FileSize' to numeric
  arrange(Year) %>%
  mutate(cum_year_size = cumsum(yearly_size)) %>%  # Calculate cum_year_size *after* summarizing
  filter(!is.na(cum_year_size)) #Now filter can find the column

yearly_sum

ggplot(yearly_sum, aes(x = Year, y = cum_year_size)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Cumulative File Size by Year", x = "Year", y = "Cumulative Size")


# Plot of quarter with size
quarterly_sum <- Timeseries_df %>%
  group_by(Year_Quarter) %>%
  summarise(quarterly_size = sum(FileSize)) %>%
  mutate(cum_quar_size = cumsum(quarterly_size))%>%
  filter(!is.na(cum_quar_size))

quarterly_sum

ggplot(quarterly_sum, aes(x = Year_Quarter)) +
  geom_line(aes(y = quarterly_size, color = "Quarterly Size"), group = 1) +
  geom_point(aes(y = quarterly_size, color = "Quarterly Size"), size = 4) +
  
  geom_line(aes(y = cum_quar_size, color = "Cumulative Size"), group = 1, linetype = "dashed") +
  geom_point(aes(y = cum_quar_size, color = "Cumulative Size"), size = 4) +
  
  geom_text(aes(y = quarterly_size, label = round(quarterly_size, 2)), vjust = -1, size = 3) +
  geom_text(aes(y = cum_quar_size, label = round(cum_quar_size, 2)), vjust = 2, size = 3) +
  
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  labs(title = "(Cumulative) File Size by Quarter",
       x = "Quarter",
       y = "Quarterly (Cumulative) Size")



# # pie chat
# quarterly_pie <- quarterly_sum %>%
#   filter(
#     Year_Quarter == "2024-Q2" |
#       Year_Quarter == "2024-Q3" |
#       Year_Quarter == "2024-Q4" |
#       Year_Quarter == "2025-Q1"'"
#   ) %>%
#   mutate(
#     percentage = quarterly_size / sum(quarterly_size) * 100
#   )
# 
# quarterly_pie
# 
# ggplot(quarterly_pie, aes(x = "", y = quarterly_size, fill = Year_Quarter)) +
#   geom_bar(stat = 'identity', width = 1, show.legend = FALSE) +
#   coord_polar(theta = 'y') +
#   geom_label_repel(
#     aes(label = paste0(Year_Quarter, '\n', round(percentage, 2), '%\n', quarterly_size)),
#     size = 4,
#     nudge_x = 1,
#     show.legend = FALSE,
#     segment.size = 0.3,
#     segment.color = 'gray'
#   ) +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   labs(title = 'Quarterly Size Distribution (2024-Q2 to 2025-Q1)',
#        x = '',
#        y = 'Quarterly Size Comparison')


# Plot of month with size
monthly_sum <- Timeseries_df %>%
  group_by(Year_Month) %>%
  summarise(monthly_size = sum(FileSize)) %>%
  mutate(cum_mon_size = cumsum(monthly_size))%>%
  filter(!is.na(cum_mon_size))

monthly_sum

monthly_sum$Year_Month <- as.Date(paste0(monthly_sum$Year_Month, "-01"))
ggplot(monthly_sum, aes(x = Year_Month, y = monthly_size)) +
  geom_line() +
  geom_point(size=4) +
  geom_text(aes(label = round(cum_mon_size, 2)), vjust = -1, size = 3) +
  #  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(1, 1, 2, 1, "cm")
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "File Size by Month",
       x = "Month",
       y = "Monthly Size")


ggplot(monthly_sum, aes(x = Year_Month, y = cum_mon_size)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(cum_mon_size, 2)), vjust = -1, size = 3)  +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(1, 1, 2, 1, "cm")
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Cumulative File Size by Month",
       x = "Month",
       y = "Monthly Cumulative Size")


# daily rolling average
try_day_roll <- Timeseries_df %>%
  arrange(Date) %>%
  filter(Date > "2024-02-05") %>%
  mutate(rolling_avg = rollapply(FileSize, width = 200, FUN = mean, align = "right", fill = NA))

ggplot(data = try_day_roll, aes(x = Date, y = rolling_avg)) +
  geom_line() +
  geom_point(size = 4, alpha = 0.4, color = "blue") +
#  geom_text(aes(label = round(FileSize, 2)), vjust = -1, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
         plot.title = element_text(hjust = 0.5),
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 14),
         plot.margin = margin(1, 1, 2, 1, "cm")
   ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d %b %Y") +
  labs(title = "Rolling Average of File Size Over Time", x = "Date", y = "Rolling Average Size")


# monthly rolling average
Timeseries_df$Year_Month <- as.Date(paste0(Timeseries_df$Year_Month, "-01"))

try_month_roll <- Timeseries_df %>%
  arrange(Year_Month) %>%
  filter(Year_Month > "2024-01-01") %>%
  mutate(rolling_avg = rollapply(FileSize, width = 3, FUN = mean, align = "right", fill = NA))

ggplot(data = try_month_roll, aes(x = Year_Month, y = rolling_avg)) +
  geom_line() +
  geom_point(size = 4, alpha = 0.4, color = "grey") +
  #  geom_text(aes(label = round(FileSize, 2)), vjust = -1, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(1, 1, 2, 1, "cm")
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Rolling Average of File Size by month", x = "Date", y = "Rolling Average Size")


# Plot with week and size
weekly_sum <- Timeseries_df %>%
  group_by(Week_of_Month) %>%
  summarise(weekly_size = sum(FileSize)) %>%
  mutate(cum_week_size = cumsum(weekly_size))%>%
  filter(!is.na(cum_week_size))

weekly_sum <- weekly_sum %>%
  mutate(
    Year = str_sub(Week_of_Month, 1, 4),
    Month = str_sub(Week_of_Month, 6, 7),
    Week = str_sub(Week_of_Month, 9, 10),
    Week = as.integer(str_replace(Week, "W", "")),
    Week_Start = as.Date(paste(Year, Month, "01", sep = "-")),  
    Week_Date = Week_Start + weeks(Week - 1)                   
  )
weekly_sum

weekly$Week_of_Month <- as.Date(paste0(Timeseries_df$Week_of_Month, "-01"))

ggplot(weekly_sum, aes(x = Week_Date, y = weekly_size)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Weekly File Size", x = "Week", y = "Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(1, 1, 2, 1, "cm")
  )


# ggplot(weekly_sum, aes(x = Week_Date)) +
#   geom_line(aes(y = weekly_size, color = "Weekly Size"), group = 1) +
#   geom_point(aes(y = weekly_size, color = "Weekly Size"), size = 4) +
#   
#   geom_line(aes(y = cum_week_size, color = "Cumulative Size"), group = 1, linetype = "dashed") +
#   geom_point(aes(y = cum_week_size, color = "Cumulative Size"), size = 4) +
#   
#   geom_text(aes(y = weekly_size, label = round(weekly_size, 2)), vjust = -1, size = 3) +
#   geom_text(aes(y = cum_week_size, label = round(cum_week_size, 2)), vjust = 2, size = 3) +
#   
#   theme_minimal() +
#   theme(#axis.text.x = element_text(angle = 0, hjust = 1),
#     plot.title = element_text(hjust = 0.5),
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14)
#   ) +
#   labs(title = "(Cumulative) File Size by Week",
#        x = "Week",
#        y = "Weekly (Cumulative) Size")

# weekday plot

weekday_sum <- Timeseries_df %>%
  group_by(Weekday) %>%
  summarise(weekday_size = sum(FileSize)) %>%
  mutate(cum_wkday_size = cumsum(weekday_size))%>%
  filter(!is.na(cum_wkday_size))

weekday_sum

# weekday_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
# weekday_sum$Weekday <- factor(weekday_sum$Weekday, levels = weekday_order, ordered = TRUE)

ggplot(weekday_sum, aes(x = Weekday, y = weekday_size)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Weekday File Size", x = "Weekday", y = "Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(1, 1, 2, 1, "cm")
  )
