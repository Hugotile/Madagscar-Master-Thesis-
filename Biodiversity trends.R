######################################

# Birds
#NICE PLOT OF PREDICTION 
# 0, 10, 20 , 30 , 40 , 50 , 60 
# INTERVALLS 


#####################################

plot_list <- list()

# Read the data
df <- predictions_0_to_60

# Create a subset of the data with required columns
df_subset_Birds <- df[, c("td_in_year",
                          "predicted_value_ExoticBirds", "conf_low_ExoticBirds", "conf_high_ExoticBirds",
                          "predicted_value_EndemicBirds", "conf_low_EndemicBirds", "conf_high_EndemicBirds",
                          "predicted_value_NativeNonEndemicBirds", "conf_low_NativeNonEndemicBirds", "conf_high_NativeNonEndemicBirds")]


plot_list[[1]] <- ggplot() +
  # ExoticBirds
  geom_line(data = df_subset_Birds, aes(x = td_in_year, y = predicted_value_ExoticBirds, color = "Exotic")) +
  geom_ribbon(data = df_subset_Birds, aes(x = td_in_year, ymin = conf_low_ExoticBirds, ymax = conf_high_ExoticBirds), fill = "darkred", alpha = 0.5) +
  # EndemicBirds
  geom_line(data = df_subset_Birds, aes(x = td_in_year, y = predicted_value_EndemicBirds, color = "Endemic")) +
  geom_ribbon(data = df_subset_Birds, aes(x = td_in_year, ymin = conf_low_EndemicBirds, ymax = conf_high_EndemicBirds), fill = "darkblue", alpha = 0.5) +
  # NativeNonEndemicBirds
  geom_line(data = df_subset_Birds, aes(x = td_in_year, y = predicted_value_NativeNonEndemicBirds, color = "Native Non Endemic")) +
  geom_ribbon(data = df_subset_Birds, aes(x = td_in_year, ymin = conf_low_NativeNonEndemicBirds, ymax = conf_high_NativeNonEndemicBirds), fill = "darkgreen", alpha = 0.5) +
  # Add legend for lines only
  scale_color_manual(name = "Origins", 
                     values = c("Exotic" = "darkred", "Endemic" = "darkblue", "Native Non Endemic" = "darkgreen"),
                     labels = c("Exotic", "Endemic", "Native Non Endemic"),
                     guide = guide_none()) +
  # Customize plot
  labs(x = "Time since deforestation in years ", y = "Predicted nb. of species", title = "Birds") +
  theme_minimal()

######################################

# Ants
#NICE PLOT OF PREDICTION 
# 0, 10, 20 , 30 , 40 , 50 , 60 
# INTERVALLS 


#####################################

# Read the data
df <- predictions_0_to_60

# Create a subset of the data with required columns
df_subset_Ants <- df[, c("td_in_year",
                         "predicted_value_ExoticAnts", "conf_low_ExoticAnts", "conf_high_ExoticAnts",
                         "predicted_value_EndemicAnts", "conf_low_EndemicAnts", "conf_high_EndemicAnts",
                         "predicted_value_NativeNonEndemicAnts", "conf_low_NativeNonEndemicAnts", "conf_high_NativeNonEndemicAnts")]


plot_list[[2]] <- ggplot() +
  # ExoticAnts
  geom_line(data = df_subset_Ants, aes(x = td_in_year, y = predicted_value_ExoticAnts, color = "Exotic")) +
  geom_ribbon(data = df_subset_Ants, aes(x = td_in_year, ymin = conf_low_ExoticAnts, ymax = conf_high_ExoticAnts), fill = "darkred", alpha = 0.5) +
  # EndemicAnts
  geom_line(data = df_subset_Ants, aes(x = td_in_year, y = predicted_value_EndemicAnts, color = "Endemic")) +
  geom_ribbon(data = df_subset_Ants, aes(x = td_in_year, ymin = conf_low_EndemicAnts, ymax = conf_high_EndemicAnts), fill = "darkblue", alpha = 0.5) +
  # NativeNonEndemicAnts
  geom_line(data = df_subset_Ants, aes(x = td_in_year, y = predicted_value_NativeNonEndemicAnts, color = "Native Non Endemic")) +
  geom_ribbon(data = df_subset_Ants, aes(x = td_in_year, ymin = conf_low_NativeNonEndemicAnts, ymax = conf_high_NativeNonEndemicAnts), fill = "darkgreen", alpha = 0.5) +
  # Add legend for lines only
  scale_color_manual(name = "Origins", 
                     values = c("Exotic" = "darkred", "Endemic" = "darkblue", "Native Non Endemic" = "darkgreen"),
                     labels = c("Exotic", "Endemic", "Native Non Endemic"),
                     guide = guide_none()) +
  # Customize plot
  labs(title = "Ants") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


######################################

# Herbs
#NICE PLOT OF PREDICTION 
# 0, 10, 20 , 30 , 40 , 50 , 60  
# INTERVALLS 


#####################################


# Read the data
df <- predictions_0_to_60

# Create a subset of the data with required columns
df_subset_Herbs <- df[, c("td_in_year",
                          "predicted_value_ExoticHerbs", "conf_low_ExoticHerbs", "conf_high_ExoticHerbs",
                          "predicted_value_EndemicHerbs", "conf_low_EndemicHerbs", "conf_high_EndemicHerbs",
                          "predicted_value_NativeNonEndemicHerbs", "conf_low_NativeNonEndemicHerbs", "conf_high_NativeNonEndemicHerbs")]


plot_list[[3]] <- ggplot() +
  # ExoticHerbs
  geom_line(data = df_subset_Herbs, aes(x = td_in_year, y = predicted_value_ExoticHerbs, color = "Exotic")) +
  geom_ribbon(data = df_subset_Herbs, aes(x = td_in_year, ymin = conf_low_ExoticHerbs, ymax = conf_high_ExoticHerbs), fill = "darkred", alpha = 0.5) +
  # EndemicHerbs
  geom_line(data = df_subset_Herbs, aes(x = td_in_year, y = predicted_value_EndemicHerbs, color = "Endemic")) +
  geom_ribbon(data = df_subset_Herbs, aes(x = td_in_year, ymin = conf_low_EndemicHerbs, ymax = conf_high_EndemicHerbs), fill = "darkblue", alpha = 0.5) +
  # NativeNonEndemicHerbs
  geom_line(data = df_subset_Herbs, aes(x = td_in_year, y = predicted_value_NativeNonEndemicHerbs, color = "Native Non Endemic")) +
  geom_ribbon(data = df_subset_Herbs, aes(x = td_in_year, ymin = conf_low_NativeNonEndemicHerbs, ymax = conf_high_NativeNonEndemicHerbs), fill = "darkgreen", alpha = 0.5) +
  # Add legend for lines only
  scale_color_manual(name = "Origins", 
                     values = c("Exotic" = "darkred", "Endemic" = "darkblue", "Native Non Endemic" = "darkgreen"),
                     labels = c("Exotic", "Endemic", "Native Non Endemic"),
                     guide = guide_none()) +
  # Customize plot
  labs(title = "Herbs") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


######################################

# Trees
#NICE PLOT OF PREDICTION 
# 0, 10, 20 , 30 , 40 , 50 , 60 
# INTERVALLS 


#####################################


# Read the data
df <- predictions_0_to_60

# Create a subset of the data with required columns
df_subset_Trees <- df[, c("td_in_year",
                          "predicted_value_ExoticTrees", "conf_low_ExoticTrees", "conf_high_ExoticTrees",
                          "predicted_value_EndemicTrees", "conf_low_EndemicTrees", "conf_high_EndemicTrees",
                          "predicted_value_NativeNonEndemicTrees", "conf_low_NativeNonEndemicTrees", "conf_high_NativeNonEndemicTrees")]


plot_list[[4]] <- ggplot() +
  # ExoticTrees
  geom_line(data = df_subset_Trees, aes(x = td_in_year, y = predicted_value_ExoticTrees, color = "Exotic")) +
  geom_ribbon(data = df_subset_Trees, aes(x = td_in_year, ymin = conf_low_ExoticTrees, ymax = conf_high_ExoticTrees), fill = "darkred", alpha = 0.5) +
  # EndemicTrees
  geom_line(data = df_subset_Trees, aes(x = td_in_year, y = predicted_value_EndemicTrees, color = "Endemic")) +
  geom_ribbon(data = df_subset_Trees, aes(x = td_in_year, ymin = conf_low_EndemicTrees, ymax = conf_high_EndemicTrees), fill = "darkblue", alpha = 0.5) +
  # NativeNonEndemicTrees
  geom_line(data = df_subset_Trees, aes(x = td_in_year, y = predicted_value_NativeNonEndemicTrees, color = "Native Non Endemic")) +
  geom_ribbon(data = df_subset_Trees, aes(x = td_in_year, ymin = conf_low_NativeNonEndemicTrees, ymax = conf_high_NativeNonEndemicTrees), fill = "darkgreen", alpha = 0.5) +
  # Add legend for lines only
  scale_color_manual(name = "Origins", 
                     values = c("Exotic" = "darkred", "Endemic" = "darkblue", "Native Non Endemic" = "darkgreen"),
                     labels = c("Exotic", "Endemic", "Native Non Endemic"),
                     guide = guide_none()) +
  # Customize plot
  labs(title = "Trees") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())



######################################

# Butterflies
#NICE PLOT OF PREDICTION 
# 0, 10, 20 , 30 , 40 , 50 , 60 
# INTERVALLS 


#####################################


# Read the data
df <- predictions_0_to_60

# Create a subset of the data with required columns
df_subset_Butterflies <- df[, c("td_in_year",
                                "predicted_value_EndemicButterflies", "conf_low_EndemicButterflies", "conf_high_EndemicButterflies",
                                "predicted_value_NativeNonEndemicButterflies", "conf_low_NativeNonEndemicButterflies", "conf_high_NativeNonEndemicButterflies")]


plot_list[[5]] <- ggplot() +
  
  # EndemicButterflies
  geom_line(data = df_subset_Butterflies, aes(x = td_in_year, y = predicted_value_EndemicButterflies, color = "Endemic")) +
  geom_ribbon(data = df_subset_Butterflies, aes(x = td_in_year, ymin = conf_low_EndemicButterflies, ymax = conf_high_EndemicButterflies), fill = "darkblue", alpha = 0.5) +
  # NativeNonEndemicButterflies
  geom_line(data = df_subset_Butterflies, aes(x = td_in_year, y = predicted_value_NativeNonEndemicButterflies, color = "Native Non Endemic")) +
  geom_ribbon(data = df_subset_Butterflies, aes(x = td_in_year, ymin = conf_low_NativeNonEndemicButterflies, ymax = conf_high_NativeNonEndemicButterflies), fill = "darkgreen", alpha = 0.5) +
  # Add legend for lines onl
  scale_color_manual(name = "Origins", 
                     values = c( "Endemic" = "darkblue", "Native Non Endemic" = "darkgreen"),
                     labels = c("Endemic", "Native Non Endemic"),
                     guide = guide_none()) +
  # Customize plot
  labs(title = "Butterflies") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())



######################################

# Reptiles
#NICE PLOT OF PREDICTION 
# 0, 10, 20 , 30 , 40 , 50 , 60 
# INTERVALLS 


#####################################

# Read the data
df <- predictions_0_to_60

# Create a subset of the data with required columns
df_subset_Reptiles <- df[, c("td_in_year",
                             "predicted_value_EndemicReptiles", "conf_low_EndemicReptiles", "conf_high_EndemicReptiles",
                             "predicted_value_NativeNonEndemicReptiles", "conf_low_NativeNonEndemicReptiles", "conf_high_NativeNonEndemicReptiles")]


plot_list[[6]] <- ggplot() +
  
  # EndemicReptiles
  geom_line(data = df_subset_Reptiles, aes(x = td_in_year, y = predicted_value_EndemicReptiles, color = "Endemic")) +
  geom_ribbon(data = df_subset_Reptiles, aes(x = td_in_year, ymin = conf_low_EndemicReptiles, ymax = conf_high_EndemicReptiles), fill = "darkblue", alpha = 0.5) +
  # NativeNonEndemicReptiles
  geom_line(data = df_subset_Reptiles, aes(x = td_in_year, y = predicted_value_NativeNonEndemicReptiles, color = "Native Non Endemic")) +
  geom_ribbon(data = df_subset_Reptiles, aes(x = td_in_year, ymin = conf_low_NativeNonEndemicReptiles, ymax = conf_high_NativeNonEndemicReptiles), fill = "darkgreen", alpha = 0.5) +
  # Add legend for lines only
  scale_color_manual(name = "Origins", 
                     values = c( "Endemic" = "darkblue", "Native Non Endemic" = "darkgreen"),
                     labels = c("Endemic", "Native Non Endemic"),
                     guide = guide_none()) +
  # Customize plot
  labs( title = "Reptiles") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


######################################

# Amphibians
#NICE PLOT OF PREDICTION 
# 0, 10, 20 , 30 , 40 , 50 , 60 
# INTERVALLS 


#####################################

# Read the data
df <- predictions_0_to_60

# Create a subset of the data with required columns
df_subset_Amphibians <- df[, c("td_in_year",
                               "predicted_value_EndemicAmphibians", "conf_low_EndemicAmphibians", "conf_high_EndemicAmphibians",
                               "predicted_value_NativeNonEndemicAmphibians", "conf_low_NativeNonEndemicAmphibians", "conf_high_NativeNonEndemicAmphibians")]


plot_list[[7]] <- ggplot() +
  
  # EndemicAmphibians
  geom_line(data = df_subset_Amphibians, aes(x = td_in_year, y = predicted_value_EndemicAmphibians, color = "Endemic")) +
  geom_ribbon(data = df_subset_Amphibians, aes(x = td_in_year, ymin = conf_low_EndemicAmphibians, ymax = conf_high_EndemicAmphibians), fill = "darkblue", alpha = 0.5) +
  # NativeNonEndemicAmphibians
  geom_line(data = df_subset_Amphibians, aes(x = td_in_year, y = predicted_value_NativeNonEndemicAmphibians, color = "Native Non Endemic")) +
  geom_ribbon(data = df_subset_Amphibians, aes(x = td_in_year, ymin = conf_low_NativeNonEndemicAmphibians, ymax = conf_high_NativeNonEndemicAmphibians), fill = "darkgreen", alpha = 0.5) +
  # Add legend for lines only
  scale_color_manual(name = "Origins", 
                     values = c( "Endemic" = "darkblue", "Native Non Endemic" = "darkgreen"),
                     labels = c( "Endemic", "Native Non Endemic"),
                     guide = guide_none()) +
  # Customize plot
  labs(title = "Amphibians") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


# Assume plot_list contains multiple ggplot objects
# Combine all plots in plot_list into a single plot
combined_plot <- grid.arrange(grobs = plot_list, ncol = 3)

# Now, plot the combined plot
print(combined_plot)

################################################################################################################################################################################

# Define the labels and colors
labels <- c("Exotic", "Endemic", "Native Non Endemic")
colors <- c("darkred", "darkblue", "darkgreen")

# Create a data frame with one row for each label
df <- data.frame(labels = factor(labels))

# Create the plot
legend_plot <- ggplot(df, aes(x = 1, fill = labels)) +
  geom_bar() +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Origins", direction="vertical")) +
  scale_fill_manual(values = colors, labels = labels)

# Extract the legend
legend <- cowplot::get_legend(legend_plot)

# Add the legend to the list of plots
plot_list <- append(plot_list, list(legend))

library(grid)

# Arrange all the plots including the legend
combined_plot <- grid.arrange(grobs = plot_list, ncol = 3, top = textGrob("Biodiversity trends over time since deforestation", gp=gpar(fontsize=20, fontface="bold")))
print(combined_plot)





