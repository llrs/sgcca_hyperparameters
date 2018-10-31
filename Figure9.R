library("ggplot2")

sgcca <- readRDS("superblock_best.RDS")
data(ge_cgh_locIGR, package = "gliomaData")
A <- ge_cgh_locIGR$multiblocks
Loc <-factor(ge_cgh_locIGR$y)
levels(Loc) <-colnames(ge_cgh_locIGR$multiblocks$y)

sample <- cbind.data.frame(sgcca$Y[[3]], Loc, Names = rownames(sgcca$Y[[3]]))
theme_set(theme_bw())
ggplot(sample, aes(comp1, comp2, color = Loc)) +
  geom_text(aes(label = Names)) +
  ggtitle("Final design", subtitle = "(with superblock)") +
  labs(color = "Localization")
