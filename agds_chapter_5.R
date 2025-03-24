
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)

setwd("C:/Users/valen/Documents/agds_report_valentinlanz")
half_hourly_fluxes <- readr::read_csv("./data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006_CLEAN.csv")


#plot the data, type = 1 ist mit Linien statt Punkten
plot(half_hourly_fluxes$TIMESTAMP_START, half_hourly_fluxes$GPP_NT_VUT_REF, type = "l")

#same thing with the ggplot syntax
ggplot(data = half_hourly_fluxes, aes(x = TIMESTAMP_START, y = GPP_NT_VUT_REF)) +
  geom_line()



# prepare plot data, select narrower data sequence with dplyr::slice(index)
plot_data <- half_hourly_fluxes |> 
  dplyr::slice(24000:25000)

# plot figure
plotme <- ggplot(
  data = plot_data,
  aes(x = TIMESTAMP_START, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "Gross primary productivity", 
       subtitle = "Site: CH-Lae",
       x = "Time", 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic()

plotme





# read in demo daily data
# as saved in the previous chapter
daily_fluxes <- read_csv("./data/daily_fluxes.csv")


# Aggregate to monthly
mdf <- daily_fluxes |> 
  dplyr::mutate(month = month(date, label = TRUE)) |> 
  dplyr::group_by(month) |> 
  dplyr::summarise(GPP_NT_VUT_REF = mean(GPP_NT_VUT_REF))

# Bar plot
plot_1 <- ggplot(
  data = mdf,
  aes(x = month, y = GPP_NT_VUT_REF)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Bar plot",
       x = "Month", 
       y =expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")")))

# Custom plot
plot_2 <- ggplot(
  data = mdf,
  aes(x = month, y = GPP_NT_VUT_REF)) +
  geom_segment(aes(x = month, xend = month, y = 0, yend = GPP_NT_VUT_REF), 
               size = 3, color = "grey40") +
  geom_point(aes(x = month, y = GPP_NT_VUT_REF), size = 8, color = "tomato") +
  geom_text(aes(x = month, y = GPP_NT_VUT_REF, label = format(GPP_NT_VUT_REF, digits = 2)),
            size = 3, color = "white") +
  theme_classic() +
  labs(title = "Custom plot",
       x = "Month", 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  scale_y_continuous(limits = c(0, 8.75), expand = c(0, 0)) +
  coord_flip()

# combine plots
cowplot::plot_grid(plot_1, plot_2)





#Histogramme
ggplot(
  data = half_hourly_fluxes,
  aes(x = GPP_NT_VUT_REF, y = ..density..)
) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_density(color = "red") +  # we can overlay multiple plot layers!
  labs(title = "Histogram and density", 
       x = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic()





#----Boxplots etc----



# prepare plot data
set.seed(1985)  # for random number reproducibility in sample_n() and jitter
half_hourly_fluxes_subset <- half_hourly_fluxes |> 
  sample_n(300) |> 
  mutate(Night = ifelse(NIGHT == 1, TRUE, FALSE))

# Boxplot 
plot_1 <- ggplot(
  data = half_hourly_fluxes_subset,
  aes(x = Night, y = VPD_F)) +
  geom_boxplot(fill = "grey70") +
  labs(title = "Boxplot") +
  labs(y = "VPD (hPa)") +
  theme_classic()

# Box plot + jittered points
plot_2 <- ggplot(
  data = half_hourly_fluxes_subset,
  aes(x = Night, y = VPD_F)) +
  geom_boxplot(fill = "grey70", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Boxplot + jitter points") +
  labs(y = "VPD (hPa)") +
  theme_classic()

# Violin plot
plot_3 <- ggplot(
  data = half_hourly_fluxes_subset,
  aes(x = Night, y = VPD_F)) +
  geom_violin(fill = "grey70") +
  labs(title = "Violin plot") +
  labs(y = "VPD (hPa)") +
  theme_classic()

# combine plots
cowplot::plot_grid(plot_1, plot_2, plot_3, ncol = 3)






#----Regression----

# prepare plot data
half_hourly_fluxes_subset <- half_hourly_fluxes |>
  sample_n(1000)

# a) lineare Regressionsgerade mit geom_smooth(method="lm")
plot_1 <- ggplot(
  data = half_hourly_fluxes_subset,
  aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", color = "red") +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic()

# b) variable NIGHT is mapped on the aesthetic  {color}. R default is a continuous color scale
plot_2 <- ggplot(
  data = half_hourly_fluxes_subset,
  aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = NIGHT)) +
  geom_point(size = 0.75) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic()

# c) use color scheme suitable for categorical variables with as.factor()
plot_3 <- ggplot(
  data = half_hourly_fluxes_subset,
  aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = as.factor(NIGHT))) +
  geom_point(size = 0.75) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic()

# d) temperature mapped on a continuous color scheme
plot_4 <- ggplot(
  data = half_hourly_fluxes_subset,
  aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = TA_F)) +
  geom_point(size = 0.75) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")),
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic() +
  scale_color_viridis_c()

# combine plots
cowplot::plot_grid(plot_1, plot_2, plot_3, plot_4, ncol = 2, labels = "auto")






# Regression within categories (here months)
# prepare data
daily_fluxes <- daily_fluxes |>
  dplyr::mutate(month = month(date, label = TRUE))

ggplot(
  data = daily_fluxes,
  aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = month)) +
  geom_point(alpha = 0.5) +
  geom_smooth(formula = y ~ x + 0, method = "lm", se = FALSE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")")) ) +
  theme_classic() +
  scico::scale_color_scico_d(palette = "romaO")


#separate visualisation into different sub-plots with facet_wrap()
ggplot(
  data = daily_fluxes, # reusing the previously subset data (see above)
  aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(alpha = 0.4) +
  geom_smooth(formula = y ~ x + 0, method = "lm", color = "red", se = FALSE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")), 
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")")) ) +
  facet_wrap(~month)




