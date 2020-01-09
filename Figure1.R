library("ggplot2")
library("patchwork")

tau <- readRDS("factorial_tau.RDS")
theme_set(theme_bw())
comm0 <- ggplot(tau, aes(y, AVE_inner)) +
  geom_point() +
  ylab("inner AVE") +
  xlab("tau y") +
  scale_fill_viridis_c()
  scale_color_viridis_c()
plot1 <- comm0 +
  geom_point(aes(fill = CGH, color = CGH)) +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Colored by tau of CGH")
plot2 <- comm0 +
  geom_point(aes(color = GE, fill = GE)) +
  guides(fill = FALSE) +
  labs(title = "Colored by tau of GE", color = "tau") +
  theme( axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.title.y = element_blank())
p1 <- plot1 + plot2 + plot_annotation(tag_levels = "A")
p1
ggsave("Figures/Figure1.png", plot = p1, width = 170, dpi = 300, units = "mm")


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
