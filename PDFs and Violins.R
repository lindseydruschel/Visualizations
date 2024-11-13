# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggridges)
library(gridExtra)


# Specify the file path
file_path <- "C:/Users/druschel/Downloads/Yulia Important Only.xlsx"

# Load the "Important Only" sheet
important_only_df <- read_excel(file_path, sheet = "Important Only")

# Select only the primary markers of interest
selected_genes <- important_only_df %>%
  filter(gene_name %in% c("Adgre1", "Cd68", "Cd14", "Ly6c", "Csf1r", "Mrc1", "Il1b", "Tnf", "Tgfb1"))


# Log2-transform the expression data (adding +1 to avoid log(0) issues)
selected_genes <- selected_genes %>%
  mutate(across(starts_with("D2_") | starts_with("D4_"), ~ log2(. + 1)))

# Pivot data to long format for ggplot
long_data <- selected_genes %>%
  pivot_longer(cols = starts_with("D2_") | starts_with("D4_"),
               names_to = "Sample",
               values_to = "Expression") %>%
  mutate(Group = if_else(str_detect(Sample, "D2"), "D2", "D4"))

# Convert gene_name to a factor to ensure it is treated as a categorical variable
long_data$gene_name <- as.factor(long_data$gene_name)

# Density Ridges Plot (Expression on x, Genes on y)
ridge_plot <- ggplot(long_data, aes(x = Expression, y = gene_name, fill = Group)) +
  geom_density_ridges(scale = 1, alpha = 0.7, rel_min_height = 0.01, color = "black") +
  labs(title = "Density Ridges of Primary Markers (D2 vs D4)",
       x = "Log2 Expression Level",
       y = "Gene") +
  theme_minimal() +
  scale_fill_manual(values = c("D2" = "skyblue", "D4" = "salmon"))

# Print Density Ridges Plot
print(ridge_plot)

# Violin Plots (Genes on x, Expression on y)
violin_plot <- ggplot(long_data, aes(x = gene_name, y = Expression, fill = Group)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.7, color = "black", scale = "width") +
  labs(title = "Violin Plots of Primary Markers (D2 vs D4)",
       x = "Gene",
       y = "Log2 Expression Level") +
  theme_minimal() +
  scale_fill_manual(values = c("D2" = "skyblue", "D4" = "salmon"))

# Print Violin Plot
print(violin_plot)


# Low Resolution (Preliminary Use, 72 dpi)
ggsave("density_ridges_plot_low_res.png", plot = ridge_plot, 
       path = "C:/Users/druschel/Downloads/", 
       width = 8, height = 6, dpi = 72)
ggsave("violin_plot_low_res.png", plot = violin_plot, 
       path = "C:/Users/druschel/Downloads/", 
       width = 8, height = 6, dpi = 72)

## High-Resolution (Journal Quality, 300 dpi)
#ggsave("density_ridges_plot_high_res.png", plot = ridge_plot, 
#       path = "C:/Users/druschel/Downloads/", 
#       width = 8, height = 6, dpi = 300)
#ggsave("violin_plot_high_res.png", plot = violin_plot, 
#       path = "C:/Users/druschel/Downloads/", 
#       width = 8, height = 6, dpi = 300)
