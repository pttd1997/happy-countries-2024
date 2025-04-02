# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(ggExtra)
library(broom)
library(reshape2)
library(cluster)
library(reshape2)
library(dplyr)
library(viridis)

# Load datasets
happiness_2024 <- read.csv("/Users/banhbao/Downloads/archive/World-happiness-report-2024.csv")
happiness_2005_2024 <- read.csv("/Users/banhbao/Downloads/archive/World-happiness_2024.csv")

# 1. Dataset Overview 
# Check dataset structure
str(happiness_2024)
str(happiness_2005_2024)

# Preview the data
head(happiness_2024)
head(happiness_2005_2024)

# 2.1 Factors of Happiness
## Diversity of Happiness Scores
ggplot(happiness_2024, aes(x = Ladder.score)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Happiness Scores (2024)", 
       x = "Happiness Score", y = "Count") +
  theme_minimal()

## Correlation Between the Factors
corrplot(correlation_matrix, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, number.cex = 0.7, title = "Correlation Heatmap", mar = c(0, 0, 2, 0))

happiness_factors_2024 <- happiness_2024 %>%
  select(Country.name, Ladder.score, Log.GDP.per.capita, Social.support, Healthy.life.expectancy,
         Freedom.to.make.life.choices, Generosity, Perceptions.of.corruption)

# Create a pairplot
ggpairs(pairplot_data, 
        aes(color = happiness_bucket, alpha = 0.7),  # Color by happiness bucket
        columns = 2:8,  # Include only numerical columns
        lower = list(continuous = wrap("points", alpha = 0.5, size = 1.5)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.6)),
        upper = list(continuous = wrap("cor", size = 3))) +
  theme_minimal() +
  labs(title = "Pairplot of Factors Affecting Happiness (Colored by Happiness Bucket)")


##Region vs Happiness Score
### Region vs Happiness Score (Horizontal Violin Plot)
ggplot(happiness_2024, aes(x = Regional.indicator, y = Ladder.score, fill = Regional.indicator)) +
  geom_violin(trim = FALSE) +
  coord_flip() +
  labs(title = "Regional Distribution of Happiness Scores (2024)", 
       x = "Region", y = "Happiness Score") +
  theme_minimal()


###Boxplot for Happiness Score by Region:
ggplot(happiness_data, aes(x = Regional.indicator, y = Ladder.score, fill = Regional.indicator)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Happiness Score by Region", y = "Ladder Score", x = "Region") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###Average Happiness Score by Region
region_avg <- happiness_data %>%
  group_by(Regional.indicator) %>%
  summarize(Average.Happiness = mean(Ladder.score, na.rm = TRUE))

ggplot(region_avg, aes(x = reorder(Regional.indicator, Average.Happiness), y = Average.Happiness, fill = Regional.indicator)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Happiness Score by Region", x = "Region", y = "Average Happiness Score") +
  theme_minimal()

### Social Support vs. Happiness
ggplot(happiness_2024, aes(x = Social.support, y = Ladder.score, color = Regional.indicator)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Happiness Score vs. Social Support", 
       x = "Social Support", y = "Happiness Score") +
  theme_minimal()

### GDP per Capita vs Happiness Score (Marginal Distribution)
# Define a custom color palette for regions for better observer
region_colors <- c(
  "Western Europe" = "darkblue",
  "North America and ANZ" = "red",
  "Sub-Saharan Africa" = "purple",
  "Middle East and North Africa" = "green",
  "Latin America and Caribbean" = "orange",
  "South Asia" = "cyan",
  "Southeast Asia" = "pink",
  "East Asia" = "yellow",
  "Central and Eastern Europe" = "brown",
  "Commonwealth of Independent States" = "darkgreen"
)

# Plot with distinct region colors
p <- ggplot(happiness_2024, aes(x = Log.GDP.per.capita, y = Ladder.score)) +
  geom_point(aes(color = Regional.indicator), alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = region_colors) +
  labs(title = "Happiness Score vs. GDP per Capita (2024)",
       x = "Log GDP per Capita", y = "Happiness Score", color = "Region") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(size = 12))

# Add marginal distributions
ggMarginal(p, type = "density", fill = "skyblue", alpha = 0.5)


###Positive and Negative Affect vs. Happiness Score
##Combined plot: Positive and Negative Affect vs. Happiness Score by Country
ggplot(happiness_2005_2024, aes(x = Life.Ladder)) +
  geom_point(aes(y = Positive.affect, color = Life.Ladder), size = 3) +
  geom_point(aes(y = Negative.affect, color = Life.Ladder), size = 3, alpha = 0.5) +
  geom_smooth(aes(y = Positive.affect), method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_smooth(aes(y = Negative.affect), method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
  scale_color_gradient(low = "green", high = "red") + 
  labs(title = "Happiness Score vs Positive & Negative Affect",
       x = "Happiness Score (Life Ladder)", y = "Affect") +
  annotate("text", x = 7, y = 0.8, label = "Positive Affect (Red Line)", color = "black", size = 5, hjust = 0) +
  annotate("text", x = 7, y = 0.15, label = "Negative Affect (Blue Line)", color = "darkblue", size = 5, hjust = 0) +
  theme_minimal() +
  theme(legend.position = "none")

## Positive and Negative Affect vs. Happiness Score by region
ggplot(region_summary, aes(x = median_positive_affect, y = happiness_score, 
                           size = median_negative_affect, color = Regional.indicator)) +
  geom_point(alpha = 0.7) +  # Adjust transparency for better visibility
  scale_size_continuous(name = "Median Negative Affect", range = c(5, 15)) +  # Increase bubble size range
  scale_color_viridis_d(name = "Region") +  # Add a title to the color legend
  scale_x_continuous(limits = c(0.4, 0.8), expand = c(0, 0)) +  # Narrow the x-axis range
  labs(title = "Bubble Chart: Happiness Score vs Positive Affect by Region",
       x = "Median Positive Affect", 
       y = "Happiness Score") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

###Bubble plot for Generosity vs Happiness Score
ggplot(happiness_2024, aes(x = Generosity, y = Ladder.score, size = Log.GDP.per.capita, color = Regional.indicator)) +
  geom_point(alpha = 0.6, shape = 19, stroke = 1) +
  scale_size_continuous(range = c(3, 12), breaks = c(1, 1.5, 2, 2.5), name = "GDP per Capita") +
  scale_color_viridis_d(option = "I", name = "Region") +  # Use Viridis for discrete regions
  labs(
    title = "Generosity vs Happiness Score by Country (Global View)",
    subtitle = "Bubble size indicates GDP per Capita, colored by region",
    x = "Generosity",
    y = "Happiness Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 1)),
    size = guide_legend(title = "Log GDP per Capita")
  )


### Perceptions of Corruption vs Happiness by Region using a bar plot
# Scatterplot with a linear regression line
ggplot(happiness_2024, aes(x = Perceptions.of.corruption, y = Ladder.score, color = Regional.indicator)) +
  geom_point(alpha = 0.7, size = 3) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Add linear regression line
  scale_color_viridis_d(name = "Region") +  # Color by region
  labs(
    title = "Relationship Between Perceptions of Corruption and Happiness Score",
    subtitle = "Happiness decreases with increasing perceptions of corruption",
    x = "Perceptions of Corruption",
    y = "Happiness Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


### Freedom to make life choice vs Happiness by Region 
install.packages("ggridges")
library(ggridges)

ggplot(happiness_2024, aes(x = Freedom.to.make.life.choices, y = Regional.indicator, fill = Regional.indicator)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  scale_fill_viridis_d(name = "Region") +
  labs(
    title = "Distribution of Freedom to Make Life Choices by Region",
    x = "Freedom to Make Life Choices",
    y = "Region"
  ) +
  theme_ridges(font_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16)
  )


#2.2 Regression model to predict happiness score
# Extract significant factors
model_summary <- tidy(regression_model) %>%
  filter(p.value < 0.05)

# Bar graph of significant factors
ggplot(model_summary, aes(x = reorder(term, estimate), y = estimate, fill = term)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Significant Factors Affecting Happiness (2024)", 
       x = "Factors", y = "Coefficient Estimate") +
  theme_minimal()


#2.3 Happiness Score overtimes 

### Extract top 10 happiest and bottom 10 least happy countries in 2024
top_10_countries <- happiness_2024 %>%
  arrange(desc(Ladder.score)) %>%
  slice(1:10)

bottom_10_countries <- happiness_2024 %>%
  arrange(Ladder.score) %>%
  slice(1:10)

## Bar plot for top 10 happiest countries
ggplot(top_10_countries, aes(x = reorder(Country.name, Ladder.score), y = Ladder.score, fill = Regional.indicator)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Happiest Countries (2024)",
       x = "Country", y = "Happiness Score", fill = "Region") +
  theme_minimal()

## Bar plot for bottom 10 least happy countries
ggplot(bottom_10_countries, aes(x = reorder(Country.name, Ladder.score), y = Ladder.score, fill = Regional.indicator)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Bottom 10 Least Happy Countries (2024)",
       x = "Country", y = "Happiness Score", fill = "Region") +
  theme_minimal()

### Top 3 and Bottom 3 Countries: Happiness Scores Over Time
top_3_countries <- c("Finland", "Denmark", "Iceland")
bottom_3_countries <- c("Lesotho", "Lebanon", "Afghanistan")

filtered_data <- happiness_2005_2024 %>%
  filter(Country.name %in% c(top_3_countries, bottom_3_countries))

# Add a column to classify countries as Top 3 or Bottom 3
filtered_data <- filtered_data %>%
  mutate(Group = case_when(
    Country.name %in% top_3_countries ~ "Top 3 Happiest Countries",
    Country.name %in% bottom_3_countries ~ "Bottom 3 Least Happy Countries"
  ))

# Create the chart
ggplot(filtered_data, aes(x = year, y = Life.Ladder, color = Country.name, linetype = Group)) +
  geom_line(size = 1) +  # Draw lines
  geom_point(size = 2) +  # Add points for each year
  labs(
    title = "Happiness Scores Over Time: Top 3 and Bottom 3 Countries",
    subtitle = "Comparison of Life Ladder Scores from 2005 to 2024",
    x = "Year",
    y = "Happiness Score (Life Ladder)",
    color = "Country",
    linetype = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### Bubble chart for region-wise happiness trends
ggplot(regional_happiness_trends, aes(x = year, y = avg_happiness, size = avg_happiness, color = Regional.indicator)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(2, 10)) +  # Adjust bubble size range
  labs(title = "Region-wise Happiness Trends (Bubble Chart: 2005-2024)",
       x = "Year", y = "Average Happiness Score", 
       size = "Happiness Score", color = "Region") +
  theme_minimal()






