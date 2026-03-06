# ============================================
# House Prices Analysis
# Author: xipecast
# Date: 2026
# ============================================

library(dplyr, lib.loc="~/R/library")
library(ggplot2, lib.loc="~/R/library")
library(readr, lib.loc="~/R/library")

# Global Theme
theme_set(theme_minimal(base_size = 14))

# Output Path
output_path <- "/mnt/SSD/ssd_Documents/PROJECTS/analyst-portfolio/house-prices/outputs/"

# Load Data
houses <- read_csv("/mnt/SSD/ssd_Documents/PROJECTS/analyst-portfolio/house-prices/data/train.csv")

# ---- Price Distribution ----
p1 <- ggplot(houses, aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  geom_vline(aes(xintercept = mean(SalePrice)),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(SalePrice)),
             color = "green", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(houses$SalePrice) + 15000, y = 150,
           label = "Mean 180k", color = "red") +
  annotate("text", x = median(houses$SalePrice) - 15000, y = 150,
           label = "Median 163k", color = "green") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of House Sale Prices",
       subtitle = "Mean vs Median comparison",
       x = "Sale Price (USD)",
       y = "Count")

# ---- Cluster Plot ----
set.seed(42)
clusters <- kmeans(
  scale(houses[, c("SalePrice", "GrLivArea")]),
  centers = 3
)
houses$cluster <- as.factor(clusters$cluster)

p2 <- ggplot(houses, aes(x = GrLivArea, y = SalePrice, color = cluster)) +
  geom_point(alpha = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "House Price Clusters",
       subtitle = "Grouped by Price + Living Area",
       x = "Living Area (sq ft)",
       y = "Sale Price (USD)",
       color = "Cluster")

# ---- Neighborhood Analysis (n>50) ----
p3 <- houses %>%
  group_by(Neighborhood) %>%
  summarise(
    avg_price = mean(SalePrice),
    count = n()
  ) %>%
  filter(count > 50) %>%
  ggplot(aes(x = reorder(Neighborhood, avg_price),
             y = avg_price,
             fill = avg_price)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(title = "Average Price by Neighborhood (n>50)",
       subtitle = "Statistically significant neighborhoods only",
       x = "Neighborhood",
       y = "Average Price (USD)")

# ---- Save Plots ----
ggsave(paste0(output_path, "p1-price-distribution.png"), plot = p1, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p2-clusters.png"), plot = p2, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p3-neighborhoods.png"), plot = p3, width = 8, height = 6, dpi = 300)

print("All plots saved!")
