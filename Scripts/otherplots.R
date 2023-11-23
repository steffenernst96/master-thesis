# Graph DDM

setwd("C:/Users/steff/Documents/Universität/Master Psychologie/SS 2022/Masterarbeit/Bilder/plots")
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, janitor, patchwork)

wiener_process2 <- function (v = 1, a = 1, z = 0.5, ndt = 0.3) {
  max_iter <- 1e4  # maximum process duration in ms
  dt <- 0.001     # time steps
  sd <- 1         # sd for noise
  sqrt_dt <- sqrt(dt * sd)
  whole_path <- 0.5
  iteration <- 0
  
  # initialize diffusion path for current trial
  path <- a * z
  
  # sample diffusion process noise
  noise <- rnorm(max_iter, 0, 1)
  
  # evidence accumulation process
  iter <- 1
  while (path > 0 & path < a & iter < max_iter) {
    path <- path + v * dt + sqrt_dt * noise[iter]
    whole_path <- c(whole_path, path)
    iteration <- c(iteration, iter)
    iter <- iter + 1
  }
  
  # create a data frame
  whole_path[length(whole_path)] <- ifelse(whole_path[length(whole_path)]>=1, 1, 0)
  df <- data.frame(whole_path, iteration)
  
  # convert data frame to tibble
  tib <- as_tibble(df)
  
  # return tibble
  return(tib)
}


##Plot 1 - DDM - Modell
toplot <- wiener_process2()
toplot2 <- read_csv("cool_line_for_DDM_plot.csv") # a
max_length <- max(c(nrow(toplot), nrow(toplot2)))

ggplot(data = toplot) +
  geom_line(aes(x = iteration, y = whole_path)) +
  geom_line(data = toplot2, aes(x = iteration, y = whole_path)) +
  geom_segment(aes(x=0, y=.5, xend=max_length*.9, yend=1), arrow = arrow(angle=10)) +
  annotate("text", x = max_length*.92, y = .26, label = "Akkumulations- \n kurven") +
  geom_segment(aes(x=58, y=.88, xend=90, yend=0.7)) +
  annotate("text", x =  30, y = .9, label = substitute(paste("drift rate ", italic("v")))) +
  geom_segment(aes(x=0, y=-.15, xend=0, yend=1.15)) +
  geom_segment(aes(x=230, y=.3, xend=185, yend=0.53)) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "z", "a"), limits = c(-0.2,1.2)) +
  geom_hline(yintercept = c(0, 1)) + 
  annotate("text", x = max_length - 0.1 * max_length, y = 1.1, label = "korrekte Antwort") +
  annotate("text", x = max_length - 0.1 * max_length, y = -0.1, label = "falsche Antwort")+
  labs(x = NULL, y = NULL) +  # Remove x and y axis labels
  theme(
    axis.text.x = element_blank(),  # Remove x-axis ticks
    axis.ticks.x = element_blank(), # Remove x-axis ticks  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "white")
    )
ggsave(filename = "DDM.png", width = 10000, height = 7000, units = "px", dpi = 1600)




##time series plot
df <- read_csv("C:/Users/steff/Documents/Universität/Master Psychologie/SS 2022/Masterarbeit/Experiment/Daten/Daten_cleaned/special/df_all.csv")


###erste Hälfte
rects <- data.frame(xstart = seq(0.5,336.5,48), xend = seq(48.5,384.5,48), 
                    col = rep(c("firebrick", "steelblue"), 4))
subject = 7
p1 <- df %>% filter(subject_nr == subject,
              tutorial == 0
              ) %>%
  slice(1:384) %>% 
  ggplot() + 
  geom_rect(data=rects, aes(ymin=0, ymax=2000, xmin=xstart,
                            xmax=xend, fill=col), alpha =0.5) +
  geom_line(aes(x = 1:384, y=rt_in_ms)) +
    scale_x_continuous(breaks = c(seq(0, 300, 100),384)) + 
   scale_fill_manual(values = c("firebrick" = "firebrick", "steelblue" = "steelblue"),
                    labels = c("firebrick" = "Geschwindigkeitsbedingung", "steelblue" = "Genauigkeitsbedingung")) +
  labs(x = "Trialzahl", y = "Reaktionszeit (in ms)", fill = "Bedingung") +
  theme(legend.position = "none")
 
  
### zweite Hälfte
p2 <- df %>% filter(subject_nr == subject,
              tutorial == 0)%>%
  slice(385:768) %>% 
  ggplot() + 
  geom_rect(data=rects, aes(ymin=0, ymax=2000, xmin=xstart,
                            xmax=xend, fill=col), alpha =0.5) +
    geom_line(aes(x = 1:384, y=rt_in_ms)) +
  labs(x = element_blank(), y = element_blank(), fill = "Bedingung") +
  scale_x_continuous(breaks = c(seq(16, 416, 100),384), labels = c(seq(400, 800, 100),768)) + 
  scale_fill_manual(values = c("firebrick" = "firebrick", "steelblue" = "steelblue"),
                    labels = c("firebrick" = "Geschwindigkeit", "steelblue" = "Genauigkeit")) + 
  theme(legend.position = "bottom")
timeseries <- p1 / p2# + plot_layout(guides = 'keep')
timeseries
ggsave("timeseries.png", timeseries,  width = 5000, height = 3500, units = "px", dpi = 800)

### Plot for static vs dynamic estimations

# Set the random seed for reproducibility
set.seed(42)

# Generate the exponential growth values with increased length and less steepness
column1 <- exp(seq(1, 3.8, length.out = 150))

# Generate the second column with more extreme random fluctuations
column2 <- column1 + rnorm(100, 0, 2.4)

# Calculate the mean of the second column 200 times
column3 <- rep(mean(column2), length(column2))

# Create the dataframe
df <- data.frame(Column1 = column1, Column2 = column2, Column3 = column3)

mean(column2)
sd(column2)
# Calculate the manual confidence interval for Column 1
ci_lower <- column1 - 5
ci_upper <- column1 + 5


# Plot the values as lines with confidence intervals
hypothetical_parameter <-  ggplot(df) +
  geom_line(aes(x = seq_along(Column1), y = Column1, color = "Column 1")) +
  geom_ribbon(aes(x = 1:nrow(df), ymin = rep(mean(column2)-sd(column2), nrow(df)), ymax = rep(mean(column2)+sd(column2), nrow(df))),
              fill = "#768da8", alpha = 0.3) +
  geom_ribbon(aes(x = seq_along(Column1), ymin = ci_lower, ymax = ci_upper),
              fill = "firebrick4", alpha = 0.8) +
  
  geom_line(aes(x = seq_along(Column3), y = Column3, color = "Column 3 (Mean)")) +
  labs(x = substitute(paste("Zeit ", italic("T"))), y = substitute(paste("Parameter ", italic("\u03B8")))) +
  scale_color_manual(values = c("firebrick4", "black", "#768da8")) +
  geom_line(aes(x = seq_along(Column2), y = Column2, color = "Column 2")) +
  theme_apa() +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
hypothetical_parameter
ggsave("Hypothetical Parameter.png",  hypothetical_parameter, width = 5000, height = 3500, units = "px", dpi = 800)


## table 
#we want to know which value for erstebedingung and blauetaste there is

