library("ggpubr")

superblock_interactions <- readRDS("superblock_interactions.RDS")
weights_interactions <- readRDS("centroid_interactions.RDS")

si <- superblock_interactions[,  c("AVE_inner", "var11")]
wi <- weights_interactions[,  c("AVE_inner", "var11")]

si <- cbind(si, Type = "Superblock")
wi <- cbind(wi, Type = "Weights")
df <- rbind.data.frame(si, wi)

df$Interaction[df$var11 != 0] <- "Yes"
df$Interaction[df$var11 == 0] <- "No"

df$Classification <- paste0(df$Type, "-", df$Interaction)
my_comparisons <- list(c("Superblock-No", "Superblock-Yes"),
                       c("Weights-No", "Weights-Yes"),
                       c("Superblock-No", "Weights-No"),
                       c("Weights-Yes", "Superblock-Yes"))

figure4 <- ggviolin(df, x = "Classification", y = "AVE_inner") +
  stat_compare_means(comparisons = my_comparisons, paired = FALSE) + # Pairwise comparisons p-value
  grids() +
  ylab("inner AVE") +
  xlab("Study-interaction") +
  ggtitle("Effect of the interaction within GE")

ggsave("Figures/Figure4.png", figure4)

library("dplyr")
df %>%
  group_by(Interaction, Type) %>%
  summarise(mean(AVE_inner), sd(AVE_inner), max(AVE_inner), min(AVE_inner))
