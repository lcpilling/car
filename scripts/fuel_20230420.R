
.libPaths(c("E:/Luke/R_lib/4.2.2","E:/Program Files/R-4.2.2/library"))

## general
library(tidyverse)
library(lukesRlib)

## plotting
library(ggpubr)
theme_set(theme_bw())

## load and inspect data
dd = read_tsv("E:/Luke/Documents/Cars/C4/fuel_20230420.txt")
dd |> print(n=40)

## calculate km
dd = dd |> mutate(km=miles*1.6093)

## calculate km/l & m/gallon & ?/l
dd = dd |> mutate(kpl=km/liters,
                  mpg=miles/(liters/4.546),
                  ppl=pounds/liters)

dd |> select(mpg) |> summary()

## exclude outliers
dd |> print(n=40)
dd = dd |> filter(mpg<60)
dd = dd |> filter(mpg<60&mpg>20)
dd |> print(n=40)

## get date vectors for x axis
x_limits = as_date(c("2021-01-01","2023-05-01"))
x_breaks = as_date(c("2021-01-01","2021-07-01","2022-01-01","2022-07-01","2023-01-01","2023-07-01"))
x_minor_breaks = as_date(c("2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01",
                           "2021-07-01","2021-08-01","2021-09-01","2021-10-01","2021-11-01","2021-12-01",
                           "2022-01-01","2022-02-01","2022-03-01","2022-04-01","2022-05-01","2022-06-01",
                           "2022-07-01","2022-08-01","2022-09-01","2022-10-01","2022-11-01","2022-12-01",
                           "2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01"))

## price change over time
p1=ggplot(dd, aes(x=date, y=ppl)) + 
  labs(title="Citroen C4: Cost of fuel at each fueling", x="Date", y="GBP per liter") +
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) + 
  geom_line() + 
  geom_point() +
  ylim(c(1.1,2)) +
  scale_x_continuous(limits = x_limits,
                     breaks = x_breaks, 
                     minor_breaks = x_minor_breaks)

## mile per month 
dd = dd |> mutate(total_miles = cumsum(miles),
                  days_since_last_fill = date - lag(date, default = first(date)),
                  miles_per_day_since_last_fill = miles / as.numeric(days_since_last_fill))

p2=ggplot(dd |> filter(is.finite(miles_per_day_since_last_fill)), aes(x=date, y=miles_per_day_since_last_fill)) + 
  labs(title="Citroen C4: miles driven per day since last fill up", x="Date", y="Miles per day since last fill") + 
  geom_line() + 
  geom_point() +
  ylim(c(0,175)) +
  scale_x_continuous(limits = x_limits,
                     breaks = x_breaks, 
                     minor_breaks = x_minor_breaks)


## mpg over time
ggplot(dd, aes(x=mpg)) + geom_histogram(binwidth=1)

p3=ggplot(dd, aes(x=date, y=mpg)) + 
  labs(title="Citroen C4: fuel efficiency", x="Date", y="Miles per gallon",
       caption="Excluded entries with typos (2 in Aug 2021, 1 in Aug 2022)") +
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) + 
  geom_line() + 
  geom_point() +
  ylim(c(37,50)) +
  scale_x_continuous(limits = x_limits,
                     breaks = x_breaks, 
                     minor_breaks = x_minor_breaks)



## combine plots into figure
figure = ggarrange(p1, p2, p3,
                   labels = c("A", "B", "C"),
                   ncol = 1, nrow = 3)
figure

ggexport(figure, filename = "E:/Luke/Documents/Cars/C4/C4_fuel_20230420.png",
         width = 1800, height = 2400, res = 300)

