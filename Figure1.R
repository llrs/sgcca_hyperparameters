library("ggplot2")
library("patchwork")
library("dplyr")

tau <- readRDS("factorial_tau.RDS")
theme_set(theme_bw())


plot_tau2AVE <- function(data, title) {
  limits <- c(0.45, 0.65)
  p1 <- data %>%
    group_by(GE, y) %>%
    summarize(AVE_inner = median(AVE_inner)) %>%
    ungroup() %>%
    ggplot() +
    geom_point(aes(GE, y, col = AVE_inner)) +
    scale_color_viridis_c(limits = limits) +
    theme_minimal() +
    labs(col = "inner AVE") +
    scale_y_continuous(position = "right") +
    theme(axis.title.y.right = element_text(angle = 0, vjust = 0.5, hjust = 1))

  p2 <- data %>%
    group_by(CGH, y) %>%
    summarize(AVE_inner = median(AVE_inner)) %>%
    ggplot() +
    geom_point(aes(CGH, y, col = AVE_inner)) +
    scale_color_viridis_c(limits = limits) +
    theme_minimal() +
    labs(col = "inner AVE") +
    theme(axis.title.y = element_blank())
  p1 + p2 +  plot_layout(guides = 'collect')  +
    plot_annotation(tag_levels = "A", title = title)

}

p3 <- plot_tau2AVE(tau, "Inner AVE depending on tau")
ggsave("Figures/Figure1c.tiff", plot = p3, width = 170, dpi = 300, units = "mm")

comm0 <- ggplot(tau, aes(y, AVE_inner)) +
  ylab("inner AVE") +
  xlab("tau y") +
  scale_fill_viridis_c()
plot1 <- comm0 +
  geom_point(aes(fill = CGH, color = CGH)) +
  geom_point(data = . %>% filter(GE == 0.503293099215374 & CGH == 0.636243186088126), col = "red") +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Colored by tau of CGH")
plot2 <- comm0 +
  geom_point(aes(color = GE, fill = GE)) +
  guides(fill = FALSE) +
  geom_point(data = . %>% filter(GE == 0.503293099215374 & CGH == 0.636243186088126), col = "red") +
  labs(title = "Colored by tau of GE", color = "tau") +
  theme( axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.title.y = element_blank())
p1 <- plot1 + plot2 + plot_annotation(tag_levels = "A")
p1
ggsave("Figures/Figure1.png", plot = p1, width = 170, dpi = 300, units = "mm")

factorial <- tau
# See the rank on the AVE_inner of those with closer to the tau.estimate
factorial %>% arrange(-AVE_inner) %>%
  mutate(n = row_number()) %>%
  filter(GE == 0.503293099215374 & CGH == 0.636243186088126) %>%
  filter(AVE_inner == max(AVE_inner)) %>%
  # summarize(median(AVE_inner), median(AVE_outer), median(cc1)) %>%
  print()
# Number 97 is very good, the inner AVE median is slightly higher than those without.
factorial %>% arrange(-AVE_inner) %>%
  mutate(n = row_number()) %>%
  filter(GE != 0.503293099215374 & CGH != 0.636243186088126) %>%
  filter(AVE_inner == max(AVE_inner)) %>%
  # summarize(median(AVE_inner), median(AVE_outer), median(cc1)) %>%
  head()
tau2 <- tau %>%
  mutate(method = case_when(GE == 0.503293099215374 & CGH == 0.636243186088126 ~ "Schäfer's method",
                            TRUE ~ "grid search"))
p2 <- ggplot(tau2) +
  geom_point(aes(GE, y, col = AVE_inner, shape = method)) +
  labs(title = "by GE", shape = "Method", col = "inner AVE") +
  scale_color_viridis_c(option = "A", direction = -1) +
  ggplot(tau2) +
  geom_point(aes(CGH, y, col = AVE_inner, shape = method)) +
  scale_color_viridis_c(option = "A", direction = -1) +
  labs(title = "by CGH", shape = "Method", col = "inner AVE") +
  theme( axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.title.y = element_blank()) +
  plot_layout(guides='collect') +
  plot_annotation(tag_levels = "A", title = "Inner AVE depending on tau")
# Following recommendation from reviewer 2.
ggsave("Figures/Figure1b.tiff", plot = p2, width = 170, dpi = 300, units = "mm")

group_by(tau2, GE, CGH, y) %>%
  summarize(median(AVE_inner), median(AVE_outer))
comm <- ggplot(tau) +
  scale_colour_viridis_c() +
  ylim(c(0.4, .7)) +
  scale_fill_gradient()
plot1b <- comm +
  ylab("inner AVE") +
  geom_point(aes(y, AVE_inner)) +
  xlab("tau y") +
  ggtitle("By tau y")
plot2b <- comm +
  geom_point(aes(GE, AVE_inner)) +
  xlab("tau GE") +
  ylab("") +
  ggtitle("By tau GE")
plot3b <- comm +
  geom_point(aes(CGH, AVE_inner)) +
  xlab("tau CGH") +
  ylab("") +
  ggtitle("By tau  CGH")
plot1b + plot2b + plot3b
