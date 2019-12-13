library("ggplot2")

sgcca <- readRDS("superblock_best.RDS")
data(ge_cgh_locIGR, package = "gliomaData")
A <- ge_cgh_locIGR$multiblocks
Loc <-factor(ge_cgh_locIGR$ylabel)
levels(Loc) <-colnames(ge_cgh_locIGR$multiblocks$y)

sample <- cbind.data.frame(sgcca$Y[[3]], Loc, Names = rownames(sgcca$Y[[3]]))
theme_set(theme_bw())
theme_update(strip.background = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank())
ggplot(sample, aes(comp1, comp2, color = Loc)) +
  geom_point() +
  ggtitle("Design with superblock") +
  labs(color = "Localization", x = "Superblock component 1", y = "Superblock omponent 2") +
  scale_y_continuous(breaks = seq(-1, 0.75, by = 0.25)) +
  scale_x_continuous(breaks = seq(-1, 0.75, by = 0.25))
ggsave("Figures/Figure2.png", dpi = 300, units = "mm", width = 170)
