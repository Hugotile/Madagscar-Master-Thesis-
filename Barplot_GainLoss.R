####################################

#HISTOGRAM TOTAL GAIN LOST AGREGATE 

###################################

library(stringr)
library(ggplot2)

############################################################################################################################################

# Filter endemic species (names starting with "endemic")
Endemic_species <- subset(result_dataframe, grepl("^Endemic", Taxon, ignore.case = TRUE))
Endemic_species$Taxon <- sub("^Endemic", "", Endemic_species$Taxon)

# Filter exotic species (names starting with "exotic")
Exotic_species <- subset(result_dataframe, grepl("^Exotic", Taxon, ignore.case = TRUE))
Exotic_species$Taxon <- sub("^Exotic", "", Exotic_species$Taxon)


# Filter NativeNonEndemic species (names starting with "NativeNonEndemic")
NativeNonEndemic_species <- subset(result_dataframe, grepl("^NativeNonEndemic", Taxon, ignore.case = TRUE))
NativeNonEndemic_species$Taxon <- sub("^NativeNonEndemic", "", NativeNonEndemic_species$Taxon)

# Create a histogram
ggplot(Endemic_species, aes(x = Taxon, y = Difference_0_to_60)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "Endemic Species", y = "Gain / Loss in 60 years ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a histogram
ggplot(Exotic_species, aes(x = Taxon, y = Difference_0_to_60)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Exotic Species", y = "Gain / Loss in 60 years ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a histogram
ggplot(NativeNonEndemic_species, aes(x = Taxon, y = Difference_0_to_60)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Native Non Endemic Species", y = "Gain / Loss in 60 years ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################################################################################################################################
############################################################################################################################################

############################

#Prepare the plot data frame  

############################



#copy result_dataframe

plot_dataframe <- result_dataframe 


# Add a new column named "Origin"
plot_dataframe <- plot_dataframe %>% mutate(Origin = ifelse(grepl("NativeNonEndemic", Taxon), "Native Non Endemic", 
                                    ifelse(grepl("Endemic", Taxon), "Endemic", "Exotic")))


# Add a space in the Taxon column terms by removing the origin prefix from the 'Taxon' column

plot_dataframe$Taxon <- str_replace_all(plot_dataframe$Taxon, "(?<!^)([A-Z])", " \\1")


# Create a Taxa column by removing the origin prefix from the 'Taxon' column

plot_dataframe$Taxa <- gsub("^(Endemic|Native Non Endemic|Exotic)", "", plot_dataframe$Taxon)


###########################

# HISTOGRAM ORIGIN AGGREGATE 
# Next to each other 
# With significance
# Abs1

###########################

# Define the colors for each origin
colors <- c("Endemic" = "darkblue", "NativeNonEndemic" = "darkgreen", "Exotic" = "darkred")


# Define a function to create the plot with color mapping based on origin
plot_taxa <- function(plot_dataframe) {
  ggplot(plot_dataframe, aes(x = Taxon, y = Difference_0_to_60, fill = Origin)) +
    geom_bar(stat = "identity") +  # Plot bars with identity statistic
    geom_text(aes(label = ifelse(C.I_Overlap == "No overlap", "Significant", ""), color = "red"),
              position = position_dodge(width = 0.7), vjust = -0.5, size = 3, show.legend = FALSE) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = c("red" = "red"))  +
    labs(x = "Taxon", y = "nb. of species lost / gained after 60 years of deforestation") +
    ggtitle("Extinction Debt and Immigration Credit for Different Taxa and Origins in North Eastern Madagascar ") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_fill_manual(values = c("darkblue","darkred","darkgreen"))  # Set colors for origins
 }

# Plot the data with the defined function
plot_taxa(plot_dataframe)



###############################################################################################################


##########################

# HISTOGRAM TAXA AGGREGATE 
# One above the other 
# With significance
# Abs2
##########################





# Define a function to create the plot with color mapping based on origin
plot_taxa <- function(plot_dataframe) {
  ggplot(plot_dataframe, aes(x = Taxa, y = Difference_0_to_60, fill = Origin)) +
    geom_bar(stat = "identity") +  # Plot bars with identity statistic
    geom_text(aes(label = ifelse(C.I_Overlap == "No overlap", "Significant", ""), color = "red"),
              position = position_dodge(width = 0.7), vjust = -0.5, size = 3, show.legend = FALSE) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = c("red" = "red"))  +
    labs(x = "Taxon", y = "nb. of species lost / gained after 60 years of deforestation") +
    ggtitle("Extinction Debt and Immigration Credit for Different Taxa and Origins in North Eastern Madagascar ") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_fill_manual(values = c("darkblue","darkred","darkgreen"))  # Set colors for origins
}

# Plot the data with the defined function
plot_taxa(plot_dataframe)




###############################################################################################################





plot_taxa <- function(df) {
  ggplot(df, aes(x = Taxon, y = Difference_0_to_60, fill = Origin)) +
    geom_bar(stat = "identity") +  # Plot bars with identity statistic
    geom_point(data = df[df$C.I_Overlap == "No overlap", ], aes(x = Taxon, y = Difference_0_to_60 + 5), shape = 8, size = 3, color = "black") +  # Add points for no CI overlap
    labs(x = "Taxon", y = "Gain / Loss in Number of Species after 60 Years of Deforestation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_fill_manual(values = c("Endemic" = "darkblue", "NativeNonEndemic" = "darkgreen", "Exotic" = "darkred"))  # Set colors for origins
}
plot_taxa(plot_dataframe)

####################################################################################################################################
####################################################################################################################################


##########################

# HISTOGRAM TAXA AGREGATE 
# One next to each other 
# With significance 
# Abs3
##########################



p <- ggplot(plot_dataframe, aes(x = Taxa, y = Difference_0_to_60, fill = Origin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = ifelse(C.I_Overlap == "No overlap", "Significant", ""), color = "red"),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("red" = "red")) +  # Define the color mapping
  scale_fill_manual(values = c("darkblue","darkred","darkgreen")) +
  labs(x = "Taxa", y = "nb. of species lost / gained after 60 years of deforestation", fill = "Origin") +
  theme_minimal() +
  ggtitle("Extinction Debt and Immigration Credit for Different Taxa and Origins in North Eastern Madagascar ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)


