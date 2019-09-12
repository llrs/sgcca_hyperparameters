library("dplyr")
library("tidyr")
library("ggplot2")

# Load data
# model 2 without interaction
model2 <- readRDS("model2_sgcca.RDS")
model2_loo <- readRDS("loo-model2.RDS")

model2_best <- readRDS("model2_best.RDS")
model2_best_loo <- readRDS("loo-model2_best.RDS")

model3 <- readRDS("model3_sgcca.RDS")
model3_loo <- readRDS("loo-model3.RDS")

model3_best <- readRDS("model3_best.RDS")
model3_best_loo <- readRDS("loo-model3_interaction_best.RDS")

superblock <- readRDS("superblock_sgcca.RDS")
superblock_loo <- readRDS("loo-superblock.RDS")

superblock_best <- readRDS("superblock_best.RDS")
superblock_best_loo <- readRDS("loo-superblock_best.RDS")


getAVEs <- function(x, pos = 1L) {
  x$AVE$AVE_inner[pos]
}


tidyer <- function(data, model, type) {
  d <- as.data.frame(data) %>%
    mutate(Model = model) %>%
    gather(Component, !!type, comp1)
  d$Rownames <- rownames(data)
  if ("comp2" %in% colnames(d)) {
    d[, -grep("comp2", colnames(d))]
  } else {
    d
  }
}

# Prepare some data
data(ge_cgh_locIGR, package = "gliomaData")
data("clinic", package = "gliomaData")
A <- ge_cgh_locIGR$multiblocks
Loc <- factor(ge_cgh_locIGR$y)
levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)

clinic$age_an <- as.numeric(sub(",", ".", as.character(clinic$age_an), fixed = TRUE))
# An outlier of age that we assume that that those are days and not years,
# so it is divided by 365
clinic$age_an[clinic$age_an > 100] <- clinic$age_an[clinic$age_an > 100]/365.25
meta <- cbind.data.frame(Loc, Age = clinic$age_an, Sex = clinic$sexe)

m2GE <- tidyer(model2$Y[[1]], "1", "GE")
m2M <- tidyer(model2$Y[[2]], "1", "CGH")
m2bGE <- tidyer(model2_best$Y[[1]], "1.2", "GE")
m2bM <- tidyer(model2_best$Y[[2]], "1.2", "CGH")
m3GE <- tidyer(model3$Y[[1]], "2", "GE")
m3M <- tidyer(model3$Y[[2]], "2", "CGH")
m3bGE <- tidyer(model3_best$Y[[1]], "2.2", "GE")
m3bM <- tidyer(model3_best$Y[[2]], "2.2", "CGH")
mSGE <- tidyer(superblock$Y[[1]], "superblock", "GE")
mSM <- tidyer(superblock$Y[[2]], "superblock", "CGH")
mSbGE <- tidyer(superblock_best$Y[[1]], "superblock.2", "GE")
mSbM <- tidyer(superblock_best$Y[[2]], "superblock.2", "CGH")

df <- rbind(
  merge(m2M, m2GE),
  merge(m2bM, m2bGE),
  merge(m3M, m3GE),
  merge(m3bM, m3bGE),
  merge(mSM, mSGE),
  merge(mSbM, mSbGE)
)
df <- cbind(df, meta)
df <- as_tibble(df)

# Set the theme
theme_set(theme_bw())
theme_update(strip.background = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank())

figure3 <- df %>%
  filter(Component == "comp1") %>%
  ggplot() +
  geom_point(aes(GE, CGH, col = as.factor(Loc))) +
  facet_wrap(~Model, scales = "free") +
  labs(col = "Localization", x = "Transcriptome", y = "CGH") +
  scale_y_continuous(breaks = seq(-1, 0.75, by = 0.25)) +
  scale_x_continuous(breaks = seq(-1, 0.75, by = 0.25))
ggsave(plot = figure3, filename = "Figures/Figure3.png", width = 170,
       units = "mm", dpi = 300)
df %>%
  filter(Component == "comp1") %>%
  ggplot() +
  geom_point(aes(GE, CGH, col = Sex)) +
  facet_wrap(~Model, scales = "free") +
  labs(title = "Samples by model",
       subtitle = "Colored by sex",
       caption = "Glioma dataset",
       col = "Sex")

