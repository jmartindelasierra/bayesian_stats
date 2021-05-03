
library(dplyr)
library(rjags)
library(ggplot2)
library(ggpubr)
library(patchwork)

# Model string specification
model_string <- "model {
  # Prize location
  prize ~ dcat(pi_p[])
  pi_p[1] <- 1/3
  pi_p[2] <- 1/3
  pi_p[3] <- 1/3

  # Contestant 1st guess
  contestant1 ~ dcat(pi_c[])
  pi_c[1] <- 1/3
  pi_c[2] <- 1/3
  pi_c[3] <- 1/3
  
  # Host behaviour
  host ~ dcat(pi_h[])
  pi_h[1] <- ifelse(contestant1 == 2 && prize == 2, 1/2, 
                    ifelse(contestant1 == 3 && prize == 3, 1/2, 
                           ifelse(contestant1 == 2 && prize == 3, 1, 
                                  ifelse(contestant1 == 3 && prize == 2, 1, 0))))
  pi_h[2] <- ifelse(contestant1 == 1 && prize == 1, 1/2, 
                    ifelse(contestant1 == 3 && prize == 3, 1/2, 
                           ifelse(contestant1 == 1 && prize == 3, 1, 
                                  ifelse(contestant1 == 3 && prize == 1, 1, 0))))
  pi_h[3] <- ifelse(contestant1 == 1 && prize == 1, 1/2, 
                    ifelse(contestant1 == 2 && prize == 2, 1/2, 
                           ifelse(contestant1 == 1 && prize == 2, 1, 
                                  ifelse(contestant1 == 2 && prize == 1, 1, 0))))
  
  # Contestant 2nd guess (switch strategy)
  contestant2 <- ifelse(contestant1 != 1 && host != 1, 1,
                        ifelse(contestant1 != 2 && host != 2, 2,
                              ifelse(contestant1 != 3 && host != 3, 3, 0)))
  
  # Wins with switch strategy
  win <- ifelse(contestant2 == prize, 1, 0)
}"

set.seed(100)

# Model specification into JAGS
bayes_model <- jags.model(textConnection(model_string), 
                          data = list(), 
                          n.chains = 1)

# Parameters to monitor
params <- c("prize", "contestant1", "contestant2", "host", "win")

# Simulations
bayes_sim <- coda.samples(model = bayes_model,
                          variable.names = params,
                          n.iter = 5000)

# Combined simulations (useful for n.chains > 1)
bayes_csim <- do.call(rbind, bayes_sim)
bayes_csim <- data.frame(bayes_csim)

# Plots
pl_graph <- ggplot() +
  background_image(png::readPNG("monty_hall_graphical_model.png")) +
  labs(title = "Graphical model") +
  coord_fixed()

pl_prize <- bayes_csim %>%
  ggplot() +
  geom_bar(aes(x = prize, y = ..count../sum(..count..)), width = 0.5, fill = "steelblue") +
  geom_text(stat = "count", aes(x = prize, y = ..count../sum(..count..) + 0.04, label = round(..count../sum(..count..), 2))) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("A", "B", "C")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(title = "Prob. of prize location", x = "Door", y = "Probability")

pl_contestant1 <- bayes_csim %>%
  ggplot() +
  geom_bar(aes(x = contestant1, y = ..count../sum(..count..)), width = 0.5, fill = "steelblue") +
  geom_text(stat = "count", aes(x = contestant1, y = ..count../sum(..count..) + 0.04, label = round(..count../sum(..count..), 2))) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("A", "B", "C")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(title = "Probability of contestant 1st guess", x = "Door", y = "Probability")

pl_host <- bayes_csim %>%
  ggplot() +
  geom_bar(aes(x = host, y = ..count../sum(..count..)), width = 0.5, fill = "steelblue") +
  geom_text(stat = "count", aes(x = host, y = ..count../sum(..count..) + 0.04, label = round(..count../sum(..count..), 2))) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("A", "B", "C")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(title = "Probability of host choice", x = "Door", y = "Probability")

pl_contestant2 <- bayes_csim %>%
  ggplot() +
  geom_bar(aes(x = contestant2, y = ..count../sum(..count..)), width = 0.5, fill = "steelblue") +
  geom_text(stat = "count", aes(x = contestant2, y = ..count../sum(..count..) + 0.04, label = round(..count../sum(..count..), 2))) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("A", "B", "C")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(title = "Probability of contestant 2nd guess", x = "Door", y = "Probability")

pl_win <- bayes_csim %>%
  ggplot() +
  geom_bar(aes(x = win, y = ..count../sum(..count..)), width = 0.5, fill = "steelblue") +
  geom_text(stat = "count", aes(x = win, y = ..count../sum(..count..) + 0.04, label = round(..count../sum(..count..), 2))) +
  geom_hline(yintercept = 1/2, linetype = "dashed") +
  annotate(geom = "text", x = 0.5, y = 0.5, label = "random\nchoice") +
  scale_x_continuous(breaks = c(0, 1), labels = c("stay", "switch")) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(title = "Probability of winning", x = "Strategy", y = "Probability")

# Combined plots
pl_graph + pl_win +
  plot_annotation(title = "Monty Hall problem",
                  caption = "Jesús Martín de la Sierra",
                  theme = theme(plot.title = element_text(size = 18)))
