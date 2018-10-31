library("ggplot2")
library("patchwork")

tau <- readRDS("factorial_tau.RDS")
theme_set(theme_bw())
comm0 <- ggplot(tau, aes(y, AVE_inner)) +
  geom_point() +
  ylab("inner AVE") +
  xlab("tau.y") +
  scale_colour_viridis_c() +
  scale_fill_gradient()
plot1 <- comm0 +
  geom_point(aes(fill = CGH, color = CGH)) +
  guides(color = FALSE, fill = FALSE) +
  ggtitle("Colored by tau of CGH")
plot2 <- comm0 +
  ylab("") +
  geom_point(aes(color = GE, fill = GE)) +
  guides(color = guide_legend(title = "tau"),
         fill = FALSE) +
  ggtitle("Colored by tau of GE")
plot1 + plot2


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
