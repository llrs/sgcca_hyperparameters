library("patchwork")
library("ggplot2")

horst_weights <- readRDS("horst_weights.RDS")
factorial_weights <- readRDS("factorial_weights.RDS")
centroid_weights <- readRDS("centroid_weights.RDS")



df <- rbind(horst_weights, centroid_weights, factorial_weights)
df$type <- rep(c("horst", "centroid", "factorial"), each = nrow(horst_weights))
df$n <- rep(seq_len(nrow(horst_weights)), 3)


inner <- data.frame(horst = horst_weights$AVE_inner,
           factorial = factorial_weights$AVE_inner,
           centroid = centroid_weights$AVE_inner)
ggplot(df) +
  geom_count(aes(AVE_inner, AVE_outer, col = type, shape = type)) +
  theme_bw()  +
  # scale_colour_grey() +
  # facet_wrap(~type, scales = "free")
  labs(col = "Scheme", group = "Model", shape = "Scheme",
       x = "inner AVE", y = "outer AVE",
       title = "Models with different schemes", size = "Count") +
  NULL
ggsave("Figures/Additional_file_0.png")

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
  filter(AVE_inner == max(AVE_inner))
df %>%
  group_by(type) %>%
  summarise(mean(AVE_inner), mean(AVE_outer), sd(AVE_inner), sd(AVE_outer)) %>%
  arrange(desc(`mean(AVE_inner)`))

ggplot(ds) +
  # geom_point(aes(`mean(AVE_inner)`, `mean(AVE_outer)`)) +
  geom_errorbar(aes(x = `mean(AVE_inner)`,
                    ymin = `mean(AVE_outer)` -`sd(AVE_outer)`,
                    ymax = `mean(AVE_outer)` + `sd(AVE_outer)`)) +
  geom_errorbarh(aes(y = `mean(AVE_outer)`,
                     xmin = `mean(AVE_inner)` -`sd(AVE_inner)`,
                     xmax = `mean(AVE_inner)` + `sd(AVE_inner)`)) +
  theme_bw()


write.csv(df, "additional_file_0.csv", row.names = FALSE)
