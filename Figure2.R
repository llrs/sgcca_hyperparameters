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

library("broom")
library("dplyr")
lmM <- lm(AVE_inner ~ 0+GE^3*log10(CGH)*y^2, data = tau)
#similar output to log10 everything or guiving polinomials
glance(lmM)
tidy(lmM) %>% arrange(desc(abs(estimate)))
fit.loess <- loess(AVE_inner ~ 0+GE*CGH*y, data = tau)
plot1c <- plot1b +
  geom_smooth(aes(y, AVE_inner), formula = ~ 0+GE^3*log10(CGH)*y^2, tau)
plot2c <- plot2b +
  geom_smooth(aes(GE, AVE_inner), formula = ~ 0+GE^3*log10(CGH)*y^2, tau)
plot3c <- plot3b +
  geom_smooth(aes(CGH, AVE_inner), formula = ~ 0+GE^3*log10(CGH)*y^2, tau)
plot1c + plot2c + plot3c
