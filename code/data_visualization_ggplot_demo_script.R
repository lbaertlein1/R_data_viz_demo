###########################################################
# R DEMO 2: Building ggplot2 visualizations step by step
# Goal: Explain each component of ggplot2 syntax
# Assumes we already have a tidy dataset called 'data'
###########################################################

library(ggplot2)
library(scales)   # for percent formatting

#---------------------------------------
# PREP: Summarize data for bar chart
#---------------------------------------

data <- readRDS("data/pca_data_clean.Rds")

data_barchart <- data %>%
  group_by(Region) %>%
  summarise(across(c(numerator, denominator), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(pct = numerator / denominator)

data_linegraph <- data %>%
  mutate(Campaign = factor(
    Campaign,
    levels = c("Nov SIA 2024", "Dec SIA 2024", "Jan SIA 2025",
               "Feb SIA 2025", "Apr NID 2025", "May NID 2025")
  )) %>%
  mutate(pct = numerator / denominator)

###########################################################
# PART 1: ggplot2 COMPONENTS
###########################################################

# --------------------------------------
# COMPONENT 1: The ggplot() function
# --------------------------------------
# ggplot() always starts with:
#   - the data you’re using
#   - (optional) default aes() mappings

base_plot <- ggplot(data = data_barchart)
base_plot   # Right now: empty canvas, no layers yet!


# --------------------------------------
# COMPONENT 2: aes() – the “aesthetic mapping”
# --------------------------------------
# aes() tells ggplot HOW to use the data:
#   - x-axis, y-axis, color, fill, size, labels, etc.

# Example: say we want Region on x-axis and pct on y-axis:
base_plot <- ggplot(data = data_barchart,
                    aes(x = Region, y = pct))
base_plot   # Still nothing visible — we haven’t added a “geom” yet!


# --------------------------------------
# COMPONENT 3: geom_*() – the geometric layer
# --------------------------------------
# A “geom” tells ggplot WHAT KIND OF PLOT to draw:
#   geom_col() for bar chart
#   geom_line() for lines
#   geom_point() for dots
#   etc.

plot1 <- ggplot(data = data_barchart,
                aes(x = Region, y = pct)) +
  geom_col()   # makes a bar chart

plot1


# --------------------------------------
# COMPONENT 4: Aesthetic mappings inside vs outside aes()
# --------------------------------------
# - If we put “fill =” INSIDE aes(), the color will come from the data
# - If we put “fill =” OUTSIDE aes(), we set a single color for all bars

# Example: set ALL bars to steelblue:
plot1 <- ggplot(data = data_barchart,
                aes(x = Region, y = pct)) +
  geom_col(fill = "steelblue", color = "black")  # color = bar outline

plot1


# --------------------------------------
# COMPONENT 5: Adding layers
# --------------------------------------
# ggplot uses the + operator to “add” layers.

# Add a horizontal line showing the 95% target:
plot1 <- plot1 +
  geom_hline(yintercept = 0.95, color = "red", size = 1)

plot1


# --------------------------------------
# COMPONENT 6: geom_text() – adding labels
# --------------------------------------
# We can place text labels on the plot.
# Here we’ll label each bar with the % value.

plot1 <- plot1 +
  geom_text(aes(label = paste0(round(pct * 100, 2), "%")),
            vjust = -0.3)   # moves label slightly above each bar

plot1


# --------------------------------------
# COMPONENT 7: scale_*() – control axes, colors, etc.
# --------------------------------------
# scale_y_continuous() controls the y-axis.
# - limits = c(0, 1.05): start at 0, end slightly above 100%
# - labels = scales::percent: show 0.95 as 95%

plot1 <- plot1 +
  scale_y_continuous(limits = c(0, 1.05),
                     labels = scales::percent)

plot1


# --------------------------------------
# COMPONENT 8: labs() – titles and labels
# --------------------------------------
# Add plot title, subtitle, axis labels, and caption.

plot1 <- plot1 +
  labs(
    x = "Region",
    y = NULL,
    title = "Average PCA Coverage in Recent Campaigns, by Region",
    subtitle = "Campaigns from November 2024 through May 2025.\nRed line shows 95% coverage target.",
    caption = "Data Source: APMIS"
  )

plot1


# --------------------------------------
# COMPONENT 9: theme() – styling the plot
# --------------------------------------
# theme_bw() = simple black-and-white look
# theme_minimal() = cleaner, lighter look
# theme(axis.text.x = ...) = modify specific text elements

plot1 <- plot1 +
  theme_bw()

plot1   # Finished bar chart


###########################################################
# PART 2: LINE GRAPH – show different geoms & facets
###########################################################

# Same idea, different geoms: geom_line() and geom_point()

plot2 <- ggplot(data = data_linegraph,
                aes(x = Campaign, y = pct, group = Region, color = Region)) +
  
  # Line for each region
  geom_line(size = 1) +
  
  # Points on each data value
  geom_point(size = 2) +
  
  # Format y-axis as %
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.05)) +
  
  # Facet by region (each region gets its own small plot)
  facet_wrap(~ Region) +
  
  # Titles
  labs(
    x = "Campaign",
    y = "PCA Coverage (%)",
    title = "Trend in PCA Coverage in Recent Campaigns, by Region",
    subtitle = "Campaigns from November 2024 through May 2025.",
    caption = "Data Source: APMIS"
  ) +
  
  # Rotate x-axis labels
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plot2   # Finished line graph


###########################################################
# OPTIONAL: Save both plots into a single PDF for students
###########################################################

pdf("PCA_Coverage_GGPlot2_Tutorial.pdf", width = 8, height = 5)
print(plot1)
print(plot2)
dev.off()