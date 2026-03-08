# ============================================
# House Prices Analysis
# Author: xipecast
# Date: 2026
# ============================================

# Detect machine
if (file.exists("/mnt/SSD")) {
  base_path <- "/mnt/SSD/ssd_Documents/PROJECTS/analyst-portfolio/house-prices"
  lib_path <- "~/R/library"
} else {
  base_path <- "/home/boss/Documents/myDocuments/ProjectsOnLenovo/xipecast-analyst-portfolio/house-prices"
  lib_path <- .libPaths()[1]
}

# Libraries
library(dplyr, lib.loc = lib_path)
library(ggplot2, lib.loc = lib_path)
library(readr, lib.loc = lib_path)
library(tidyr, lib.loc = lib_path)

# Global Theme
theme_set(theme_minimal(base_size = 14))

# Paths
data_path <- file.path(base_path, "data/train.csv")
output_path <- file.path(base_path, "outputs/")

# Load Data
houses <- read_csv(data_path)

# ---- Clean Data ----
houses_clean <- houses %>%
  select(-PoolQC, -MiscFeature, -Alley, -Fence) %>%
  mutate(
    LotFrontage = ifelse(is.na(LotFrontage),
                         median(LotFrontage, na.rm = TRUE),
                         LotFrontage),
    FireplaceQu = ifelse(is.na(FireplaceQu), "None", FireplaceQu),
    GarageType = ifelse(is.na(GarageType), "None", GarageType)
  )

# ---- P1: Price Distribution ----
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

# ---- P2: Cluster Plot ----
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

# ---- P3: Neighborhood Analysis (n>50) ----
p3 <- houses %>%
  group_by(Neighborhood) %>%
  summarise(avg_price = mean(SalePrice), count = n()) %>%
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

# ---- P4: Quality vs Price ----
p4 <- houses %>%
  group_by(OverallQual) %>%
  summarise(median_price = median(SalePrice)) %>%
  ggplot(aes(x = OverallQual, y = median_price)) +
  geom_line(color = "steelblue", linewidth = 1.5) +
  geom_point(size = 3, color = "red") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Median Price by Quality Rating",
       subtitle = "Clear price jump at quality 7+",
       x = "Quality Rating (1-10)",
       y = "Median Price (USD)")

# ---- P5: Year Built vs Price ----
p5 <- ggplot(houses, aes(x = YearBuilt, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sale Price by Year Built",
       subtitle = "Newer houses command higher prices",
       x = "Year Built",
       y = "Sale Price (USD)")

# ---- P6: Quality vs Year Built ----
p6 <- houses %>%
  group_by(YearBuilt) %>%
  summarise(avg_quality = mean(OverallQual)) %>%
  ggplot(aes(x = YearBuilt, y = avg_quality)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Average Quality by Year Built",
       subtitle = "1980 inflection point—modern construction standards",
       x = "Year Built",
       y = "Average Quality Rating")

# ---- P7: Garage Size vs Price ----
p7 <- houses_clean %>%
  filter(GarageCars <= 3) %>%
  group_by(GarageCars) %>%
  summarise(median_price = median(SalePrice)) %>%
  ggplot(aes(x = factor(GarageCars), y = median_price)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = scales::comma(round(median_price))),
            vjust = -0.5) +
  labs(title = "Median Price by Garage Size",
       subtitle = "Excluding statistically insignificant 4-car garages",
       x = "Garage Capacity (Cars)",
       y = "Median Price (USD)")

# ---- P8: Living Area vs Price ----
p8 <- ggplot(houses_clean, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Sale Price vs Living Area",
       subtitle = "Strong linear relationship",
       x = "Living Area (sq ft)",
       y = "Sale Price (USD)")

# ---- Save All Plots ----
ggsave(paste0(output_path, "p1-price-distribution.png"), plot = p1, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p2-clusters.png"), plot = p2, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p3-neighborhoods.png"), plot = p3, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p4-quality-price.png"), plot = p4, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p5-year-price.png"), plot = p5, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p6-quality-year.png"), plot = p6, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p7-garage-price.png"), plot = p7, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p8-area-price.png"), plot = p8, width = 8, height = 6, dpi = 300)

print("All 8 plots saved!")
