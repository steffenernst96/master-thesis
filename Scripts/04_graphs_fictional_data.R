# Graph DDM

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets path to where this file is stored
dir.create(path = "graphs") #create folder where graphs are stored, if not already there
dir.create(path = "graphs/fictional_graphs") #create folder where models are stored.

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, janitor, patchwork)

#this function is a little different from the one used in script 3 because we want to visualize the whole path
#and not only get the outcome.
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
    whole_path <- c(whole_path, path) #we get the whole path, not only the outcome
    iteration <- c(iteration, iter)
    iter <- iter + 1
  }
  
  # create a tibble from the whole path
  whole_path[length(whole_path)] <- ifelse(whole_path[length(whole_path)]>=1, 1, 0)
  tib <- as_tibble(data.frame(whole_path, iteration))
  
  # return tibble
  return(tib)
}


##Plot 1 - DDM - Model

set.seed(401)
toplot <- wiener_process2()
toplot2 <- wiener_process2()
#toplot2 <- read_csv("cool_line_for_DDM_plot.csv") # a special line used for visualization
max_length <- max(nrow(toplot), nrow(toplot2))

### german
ggplot(data = toplot) +
  geom_line(aes(x = iteration, y = whole_path)) +
  geom_line(data = toplot2, aes(x = iteration, y = whole_path)) +
  geom_segment(aes(x=0, y=.5, xend=max_length*.9, yend=1), arrow = arrow(angle=10)) +
  #annotate("text", x = 435, y = .3, label = "Akkumulations- \n kurven") +
  #annotate("text", x =  100, y = .9, label = substitute(paste("drift rate ", italic("v")))) +
  geom_segment(aes(x=0, y=-.15, xend=0, yend=1.15)) +
  #geom_segment(aes(x=110, y=.84, xend=150, yend=0.7)) + #line for drift rate v
  #geom_segment(aes(x=390, y=.5, xend=430, yend=0.37)) + #line for accumulation curve
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


ggsave(filename = "graphs/fictional_data/DDM_german.png", width = 10000, height = 7000, units = "px", dpi = 1600)


### english
ggplot(data = toplot) +
  geom_line(aes(x = iteration, y = whole_path)) +
  geom_line(data = toplot2, aes(x = iteration, y = whole_path)) +
  geom_segment(aes(x=0, y=.5, xend=max_length*.9, yend=1), arrow = arrow(angle=10)) +
  #annotate("text", x = 435, y = .28, label = "accumulation- \n curves") +
  #annotate("text", x =  100, y = .9, label = substitute(paste("drift rate ", italic("v")))) +
  geom_segment(aes(x=0, y=-.15, xend=0, yend=1.15)) +
  #geom_segment(aes(x=110, y=.84, xend=150, yend=0.7)) + #line for drift rate v
  #geom_segment(aes(x=390, y=.5, xend=430, yend=0.37)) + #line for accumulation curve
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "z", "a"), limits = c(-0.2,1.2)) +
  geom_hline(yintercept = c(0, 1)) + 
  annotate("text", x = max_length - 0.1 * max_length, y = 1.1, label = "correct answer") +
  annotate("text", x = max_length - 0.1 * max_length, y = -0.1, label = "wrong answer")+
  labs(x = NULL, y = NULL) +  # Remove x and y axis labels
  theme(
    axis.text.x = element_blank(),  # Remove x-axis ticks
    axis.ticks.x = element_blank(), # Remove x-axis ticks  
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "white")
  )

ggsave(filename = "graphs/fictional_data/DDM_english.png", width = 10000, height = 5000, units = "px", dpi = 1600)



### Plot for static vs dynamic estimations

# Set the random seed for reproducibility
set.seed(42)

column1 <- exp(seq(1, 3.8, length.out = 150)) # exp. growth 

column2 <- column1 + rnorm(100, 0, 2.4) #add randomness for "true" parameter

column3 <- rep(mean(column2), length(column2))

# Create dataframe
df <- data.frame(Column1 = column1, Column2 = column2, Column3 = column3)

# Calculate a fake confidence interval for Column 1
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
ggsave("graphs/fictional_data/hypothetical_parameter.png",  hypothetical_parameter, width = 5000, height = 3500, units = "px", dpi = 800)
