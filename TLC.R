library(tidyverse)
library(ggdist)
library(patchwork)

n <- 25
n_sims <- 100

data <- tibble(Simulation = rep(1:n_sims, each = n), 
               Normal = rnorm(n * n_sims, 60, 10),
               Uniform = runif(n * n_sims, min = 40, max = 80),
               Gamma = rgamma(n * n_sims, shape = 10, scale = 6))

means <- data |> 
  group_by(Simulation) |> 
  summarize(across(Normal:Gamma, mean)) |> 
  pivot_longer(-Simulation)

means$sd <- data |> 
  group_by(Simulation) |> 
  summarize(across(Normal:Gamma, sd)) |> 
  pivot_longer(-Simulation) |> pull(value)

data |> 
  pivot_longer(-Simulation) |> 
  ggplot() + 
  geom_point(aes(x = value, y = Simulation), alpha = 0.35, colour = "lightblue") + 
  geom_path(data = means, aes(x = value, y = Simulation, group = name), colour = "orange", alpha = 0.5) + 
  geom_ribbon(data = means, aes(y = Simulation, xmin = value - sd, xmax = value + sd), fill = "orange", alpha = 0.1) + 
  facet_wrap(~name) + theme_void() + 
  scale_y_continuous(breaks = seq(0, 100, 10), labels = \(x){paste0("Simulation ", x)}, trans = "reverse") +
  scale_x_continuous(limits = c(30, 90)) +
  theme(axis.text.y = element_text(), strip.text = element_blank()) -> middle

plot_function <- function(f, args, mean_line = 60){
  ggplot(data = data.frame(x = c(30, 90)), aes(x)) +
    stat_function(fun = f, n = 101, args = args) + ylab("") +
    geom_vline(xintercept = mean_line, linetype = "11") +
    scale_y_continuous(breaks = NULL) + theme_void()
}

norm <- plot_function(dnorm, list(mean = 60, sd = 10))
unif <- plot_function(dunif, list(min = 40, max = 80))
gamma <- plot_function(dgamma, list(shape = 10, scale = 6))

ggplot(means) + 
  geom_histogram(aes(x = value), fill = "orange", colour = "white", alpha = 0.5) + 
  facet_wrap(~name) + theme_void() + 
  scale_x_continuous(limits = c(30, 90)) +
  scale_y_continuous(trans = "reverse") +
  theme(strip.text = element_blank()) -> means_hist

means |> 
  group_by(name) |> 
  summarize(mean = mean(value), sd = sd(value)) -> gaussians

mean_norm <- plot_function(dnorm, list(mean = gaussians$mean[which(gaussians$name == "Normal")],
                                       sd = gaussians$sd[which(gaussians$name == "Normal")]),
                           mean_line = gaussians$mean[which(gaussians$name == "Normal")])
mean_unif <- plot_function(dnorm, list(mean = gaussians$mean[which(gaussians$name == "Uniform")],
                                       sd = gaussians$sd[which(gaussians$name == "Uniform")]),
                           mean_line = gaussians$mean[which(gaussians$name == "Uniform")])
mean_gamma <- plot_function(dnorm, list(mean = gaussians$mean[which(gaussians$name == "Gamma")],
                                        sd = gaussians$sd[which(gaussians$name == "Gamma")]),
                            mean_line = gaussians$mean[which(gaussians$name == "Gamma")])

annotations <- tibble(label = c("Population", "Distribution", "Mean μ", "Sample", "n = 25", "Observations", "Std. Dev.", "Mean", "Sampling Distribution",
                                "of the sample mean", "Est. Pop. Mean", "Fitted Gaussian", "Sample Means"),
                      y = c(14, 13.5, 13, 11.9, 11.5, 11, 10.5, 10, 1.9, 1.5, 1, .5, 0), 
                      x = c(0, 0.5, 0.5, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0.5, 0.5, 0.5),
                      fontface = c("bold", "plain", "plain", "bold", "italic", "plain", "plain", "plain", "bold", "italic", "plain", "plain", "plain"))

lines <- tibble(type = c("solid", "11", "solid", "11", "solid"),
                colour = c("black", "black", "orange", "black", "black"),
                from_x = rep(0.1, times = 5),
                to_x = rep(0.4, times = 5),
                y = c(13.5, 13, 10, 1, 0.5))  

squares <- tibble(type = c(16, 15, 15),
                  x = rep(0.25, 3),
                  y = c(11, 10.5, 0),
                  colour = c("lightblue", "orange", "orange"))

ggplot(annotations) + 
  geom_text(aes(label = label, x = x, y = y, fontface = I(fontface)), hjust = 0) +
  geom_segment(data = lines, aes(linetype = I(type), colour = I(colour), x = from_x, xend = to_x, y = y, yend = y)) +   
  geom_point(data = squares, aes(shape = I(type), x = x, y = y, colour = I(colour))) + 
  geom_hline(yintercept = 2.5, alpha = 0.5) + 
  geom_hline(yintercept = 12.5, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 17)) + theme_void() -> annotations_plot

main_fig <- (gamma + norm + unif)/middle/(mean_gamma + mean_norm + mean_unif)/means_hist + plot_layout(heights = c(2, 10, 1.5, 1.5)) 

final <- annotations_plot + patchwork::inset_element(main_fig, left = 0.2, top = 1, bottom = 0, right = 1) +
  plot_annotation(title = "Visualizing the Central Limit Theorem:", subtitle = "Sampling Distributions of the Sample Mean") &
  theme(plot.title = element_text(hjust = .5, face = "bold"), plot.subtitle = element_text(hjust = .5, face = "italic"))

ggsave(final, file = "clt.png")