###########################################################
# R DEMO: Cleaning and Visualizing PCA Coverage Data
# Goal: Show how to tidy messy Excel data and make bar & line charts
###########################################################

###########################################################
# STEP 1: Install and load packages
###########################################################

# Run these install lines once:
install.packages("tidyverse")   # for data cleaning & plotting
install.packages("rio")         # for importing many file types easily
install.packages("janitor")     # for cleaning messy data
install.packages("scales")      # for formatting numbers as percentages

# Load packages into R (do this every session)
library(tidyverse)
library(rio)
library(janitor)
library(scales)

###########################################################
# STEP 2: Import the raw Excel file
###########################################################

data1 <- rio::import("data/Indicator_Trend_Summary_PCA___Finger_Mark_Coverage_(0_59m).xlsx")

# Look at the data
View(data1)

###########################################################
# STEP 3: Tidy the data
  # Each variable has its own column
  # Each observation has its own row
  # Each value has its own cell
###########################################################

# (a) Remove the title row and make first row the column names
data2 <- data1 %>%
  janitor::row_to_names(row_number = 1)

# (b) Reshape from WIDE (many columns) to LONG (three columns: Region, Campaign, PCA_Coverage)
data3 <- data2 %>%
  pivot_longer(cols = -Region,
               names_to = "Campaign",
               values_to = "PCA_Coverage")

# (c) Split PCA_Coverage column into:
#     pct (percentage), numerator, denominator
data4 <- data3 %>%
  #Remove the % sign for easier processing
  mutate(PCA_Coverage_clean = str_remove(PCA_Coverage, "%")) %>%
  
  #Split at the "(" to make two columns:
  #   - part before the "(" (the percentage)
  #   - part after the "(" (the numerator/denominator)
  separate(PCA_Coverage_clean, into = c("pct", "numden"), sep = "\\(") %>%
  
  #Remove the closing ")" from the numden column
  mutate(numden = str_remove(numden, "\\)")) %>%
  
  #Split at the "/" → makes two new columns: numerator and denominator
  separate(numden, into = c("numerator", "denominator"), sep = "/") %>%
  
  # Remove commas (e.g., "139,551" → "139551")
  mutate(across(c(pct, numerator, denominator), ~ str_remove_all(.x, ","))) %>%
  # Convert from text to numeric
  mutate(across(c(pct, numerator, denominator), as.numeric)) %>%
  # Drop PCA_Coverage column now that it’s split
  select(-PCA_Coverage) %>%
  # Remove the “Total” row to only keep regions
  filter(Region != "Total")

###########################################################
# STEP 4: Prepare data for charts
###########################################################

# (a) For bar chart: sum numerator & denominator for each region
data_barchart <- data4 %>%
  group_by(Region) %>%
  summarise(across(c(numerator, denominator), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(pct = numerator / denominator)

# (b) For line graph: keep the long version, but make Campaign an ordered factor
data_linegraph <- data4 %>%
  mutate(Campaign = factor(
    Campaign,
    levels = c("Nov SIA 2024", "Dec SIA 2024", "Jan SIA 2025",
               "Feb SIA 2025", "Apr NID 2025", "May NID 2025")
  )) %>%
  mutate(pct = numerator/denominator)

###########################################################
# STEP 5: Create visuals
###########################################################

## --- BAR CHART: Average PCA coverage by region ---

plot1 <- ggplot(data_barchart) +
  geom_col(aes(x = Region, y = pct),
           fill = "steelblue", color = "black") +
  
  # Horizontal line at the 95% target
  geom_hline(yintercept = 0.95, color = "red", size = 1) +
  
  # Add labels on top of bars (like “87%”)
  geom_text(aes(x = Region, y = pct,
                label = paste0(round(pct * 100, 2), "%")),
            vjust = -0.3) +   # slightly above the bar
  
  # Format y-axis to show percentages (0–105%)
  scale_y_continuous(limits = c(0, 1.05),
                     labels = scales::percent) +
  
  # Titles and labels
  labs(
    x = "Region",
    y = NULL,
    title = "Average PCA Coverage in Recent Campaigns, by Region",
    subtitle = "Campaigns from November 2024 through May 2025.\nRed line shows 95% coverage target.",
    caption = "Data Source: APMIS"
  ) +
  
  # Clean theme
  theme_bw()

plot1   # <-- shows the bar chart


## --- LINE GRAPH: PCA coverage over time by region ---

plot2 <- ggplot(data_linegraph) +
  geom_line(aes(x = Campaign, y = pct, group = Region),
            size = 1) +
  geom_point(aes(x = Campaign, y = pct), size = 2) +
  
  facet_wrap(~Region) +
  
  scale_y_continuous(labels = scales::percent, limits=c(0,1.05)) +
  
  labs(
    x = "Campaign",
    y = "PCA Coverage (%)",
    title = "Trend in PCA Coverage in Recent Campaigns, by Region",
    subtitle = "Campaigns from November 2024 through May 2025.",
    caption = "Data Source: APMIS"
  ) +
  
  theme_bw() + #Simple black and white theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))   # <-- rotate labels

plot2   # <-- shows the line graph


###########################################################
# STEP 6: Save the plots as image files
###########################################################

# Save bar chart (plot1)
ggsave("Average_PCA_Coverage_by_Region.png", plot = plot1,
       width = 6, height = 6, dpi = 300)   # 300 dpi = high quality

# Save line graph (plot2)
ggsave("Trend_in_PCA_Coverage_by_Region.png", plot = plot2,
       width = 6, height = 6, dpi = 300)

###########################################################
# STEP 7: Save BOTH plots into a single PDF
###########################################################

# Open a PDF file for writing (this will create or overwrite the file)
pdf("PCA_Coverage_Plots.pdf", width = 8, height = 5)

# Print the plots — each one will go on its own page
print(plot1)   # Page 1: Bar chart
print(plot2)   # Page 2: Line graph

# Close the PDF file
dev.off()
