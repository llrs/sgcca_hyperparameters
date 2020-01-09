horst_weights <- readRDS("horst_weights.RDS")
factorial_weights <- readRDS("factorial_weights.RDS")
centroid_weights <- readRDS("centroid_weights.RDS")


library("patchwork")
library("ggplot2")

df <- rbind(horst_weights, centroid_weights, factorial_weights)
df$type <- rep(c("horst", "centroid", "factorial"), each = nrow(horst_weights))
df$n <- rep(seq_len(nrow(horst_weights)), 3)


inner <- data.frame(horst = horst_weights$AVE_inner,
           factorial = factorial_weights$AVE_inner,
           centroid = centroid_weights$AVE_inner)
ggplot(df) +
  geom_count(aes(AVE_inner, AVE_outer, col = type, group = type)) +
  theme_bw()  +
  facet_wrap(~type, scales = "free")
ggplot(df) +
  geom_point(aes(AVE_inner, AVE_outer, col = n, group = type)) +
  theme_bw()
ggplot(df) +
  geom_density(aes(AVE_inner, col = type)) +
  theme_bw()
ggplot(df) +
  geom_density(aes(AVE_outer, col = type)) +
  theme_bw()

library("dplyr")
ds <- df %>%
  group_by(n) %>%
  summarise(mean(AVE_inner), mean(AVE_outer), sd(AVE_inner), sd(AVE_outer))

df %>%
  group_by(type) %>%
  summarise(max)
ggplot(ds) +
  # geom_point(aes(`mean(AVE_inner)`, `mean(AVE_outer)`)) +
  geom_errorbar(aes(x = `mean(AVE_inner)`,
                    ymin = `mean(AVE_outer)` -`sd(AVE_outer)`,
                    ymax = `mean(AVE_outer)` + `sd(AVE_outer)`)) +
  geom_errorbarh(aes(y = `mean(AVE_outer)`,
                     xmin = `mean(AVE_inner)` -`sd(AVE_inner)`,
                     xmax = `mean(AVE_inner)` + `sd(AVE_inner)`)) +
  theme_bw()
