###########################

# HISTOGRAM ORIGIN AGGREGATE 
# Next to each other 
# With significance
# Percentage

###########################



# Rename the column 

Total_Biodiversity <- rename(Total_Biodiversity, Taxon = Taxa)

# Join the total to the the gain / loss dataframe

plot_dataframe2 <- left_join(plot_dataframe, Total_Biodiversity, by = "Taxon")

# Add a new column named "Difference_Perc"
plot_dataframe2 <- plot_dataframe2 %>%
  mutate(Difference_Perc = (Difference_0_to_60 / Total_Species_Number) * 100)


# Define the colors for each origin
colors <- c("Endemic" = "darkblue", "NativeNonEndemic" = "darkgreen", "Exotic" = "darkred")


# Define a function to create the plot with color mapping based on origin
plot_taxa <- function(plot_dataframe2) {
  ggplot(plot_dataframe2, aes(x = Taxon, y = Difference_Perc, fill = Origin)) +
    geom_bar(stat = "identity") +  # Plot bars with identity statistic
    geom_text(aes(label = ifelse(C.I_Overlap == "No overlap", "Significant", ""), color = "red"),
              position = position_dodge(width = 0.7), vjust = -0.5, size = 3, show.legend = FALSE) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = c("red" = "red"))  +
    labs(x = "Taxa", y = " % of gained / lost species after 60 years of deforestation") +
    ggtitle("Extinction Debt and Immigration Credit for Different Taxa and Origins in North Eastern Madagscar ") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_fill_manual(values = c("darkblue","darkred","darkgreen"))  # Set colors for origins
}

# Plot the data with the defined function
plot_taxa(plot_dataframe2)
