#####################

# TOTAL BIODIVERSITY

####################




###################
# Total Herbs count
###################

# Count the number of distinct categories and their elements
value_counts <- all_Herbs$species.origin %>% table()

# Create the Total_Biodiversity dataframe
value_counts <- as.data.frame(t(value_counts))

# Select the second and third columns and rename them
Total_Herbs <- value_counts[, c(".", "Freq")]

colnames(Total_Herbs) <- c("Taxa", "Total_Species_Number")

# Add "Herbs" after each term in the "Taxa" column

Total_Herbs$Taxa <- paste(Total_Herbs$Taxa, "Herbs")

Total_Herbs[3,1] <-"Native Non Endemic Herbs"


###################
# Total Trees count
###################

# Count the number of distinct categories and their elements
value_counts <- merged_tree$origin %>% table()

# Create the Total_Biodiversity dataframe
value_counts <- as.data.frame(t(value_counts))

# Select the second and third columns and rename them
Total_Trees <- value_counts[, c(".", "Freq")]

colnames(Total_Trees) <- c("Taxa", "Total_Species_Number")

# Add "Herbs" after each term in the "Taxa" column
Total_Trees$Taxa <- paste(Total_Trees$Taxa, "Trees")

Total_Trees[1,1] <- "Endemic Trees"
Total_Trees[2,1] <- "Exotic Trees"
Total_Trees[3,1] <- "Native Non Endemic Trees"


###################
# Total Birds count
###################

# Count the number of distinct categories and their elements
value_counts <- Bird_Origin$origin %>% table()

value_counts["endemic"] <- value_counts["family"] + value_counts["genera"] + value_counts["species"] + value_counts["subfamily"]

# Select the second and third columns and rename them
Total_Birds <- value_counts[c("nonendemic", "endemic")]

Total_Birds <- as.data.frame(t(Total_Birds))

# Select the second and third columns and rename them

Total_Birds <- Total_Birds[,2:3]
colnames(Total_Birds) <- c("Taxa", "Total_Species_Number")

Total_Birds$Taxa <- as.character(c(Total_Birds$Taxa))

# rename according notation 

Total_Birds[1,1] <- "Native Non Endemic Birds"
Total_Birds[2,1] <- "Endemic Birds"

# Add the two exotic Birds 
new_row <- data.frame(Taxa = "Exotic Birds", Total_Species_Number = 2)
Total_Birds <- rbind(Total_Birds, new_row)


###################
# Total Ants count
###################

# Create the data frame manually
# from the 4 different data frame at 
# https://osf.io/4nwsz/?view_only=fda54010f0ae4c09a381e9fdb7d49acb

Total_Ants <- data.frame(
  Taxa = c("Exotic Ants", "Native Non Endemic Ants", "Endemic Ants", "Unknown Ants"),
  Total_Species_Number = c(19, 18, 57, 34)
)


#########################
# Total Butterflies Count
#########################

file_path_Butterflies <- "/Users/hugo/Desktop/madagascar/DiversityTurndata /osfstorage-archive/Species_List_With_SPECIES_ENDEMISM.CSV"

# Read the data from the CSV file into a data frame
Butterflies <- read.csv(file_path_Butterflies, sep = ";", header = TRUE)

Total_Butterflies <- table(Butterflies$Endemic)

# Create a data frame to store the results
Total_Butterflies <- data.frame(
  Taxa = c("Native Non Endemic Butterflies", "Endemic Butterflies"),
  Total_Species_Number = as.numeric(Total_Butterflies)
)

Total_Biodiversity <- rbind(Total_Herbs, Total_Trees, Total_Birds, Total_Ants, Total_Butterflies)


#########################
# Total Amphibians Count
#########################

file_path_Amphibians <- "/Users/hugo/Desktop/madagascar/DiversityTurndata /osfstorage-archive/amphibians_species_by_site_matrix - Nature_communications_Wurz_etal.csv"

# Read the data from the CSV file into a data frame
Amphibians <- read.csv(file_path_Amphibians, header = TRUE)

Total_Amphibians <- table(Amphibians$origin)

# Create a data frame to store the results
Total_Amphibians <- data.frame(
  Taxa = c("Endemic Amphibians", "Native Non Endemic Amphibians"),
  Total_Species_Number = as.numeric(Total_Amphibians)
)


#########################
# Total Reptiles Count
#########################

file_path_Reptiles <- "/Users/hugo/Desktop/madagascar/DiversityTurndata /osfstorage-archive/reptile_species_by_site_matrix - Nature_communications_Wurz_etal.csv"

# Read the data from the CSV file into a data frame
Reptiles <- read.csv(file_path_Reptiles, header = TRUE)


Total_Reptiles <- table(Reptiles$origin)

# Create a data frame to store the results
Total_Reptiles <- data.frame(
  Taxa = c("Endemic Reptiles", "Native Non Endemic Reptiles"),
  Total_Species_Number = as.numeric(Total_Reptiles)
)

Total_Biodiversity <- rbind(Total_Herbs, Total_Trees, Total_Birds, Total_Ants, Total_Butterflies, Total_Amphibians, Total_Reptiles)



###################

# TOTAL SUMMARY BY 
# ORIGIN 

###################


# Load dplyr package for data manipulation (if not already loaded)
library(dplyr)

# Group data by Origin and calculate sums and percentage
total_biodiversity <- plot_dataframe2 %>%
  group_by(Origin) %>%
  summarize(Sum_Difference_0_to_60 = sum(Difference_0_to_60),
            Total_Species_Sum = sum(Total_Species_Number),
            Gain_Loss_Percentage = round((Sum_Difference_0_to_60 / Total_Species_Sum) * 100, 2))

# View the new data frame
View(total_biodiversity)


###################

# TOTAL SUMMARY BY 
# ORIGIN 
# Percentage plot 

###################


p <- ggplot(total_biodiversity, aes(x = Origin, y = Gain_Loss_Percentage, fill = Origin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("darkblue","darkred","darkgreen")) +
  labs(x = "Taxa", y = "% of total species lost / gained after 60 years of deforestation", fill = "Origin") +
  theme_minimal() +
  ggtitle("Extinction Debt and Immigration Credit for Different Taxa and Origins in North Eastern Madagascar ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)




###################

# TOTAL SUMMARY BY 
# ORIGIN 
# Absolute plot 

###################


p <- ggplot(total_biodiversity, aes(x = Origin, y = Sum_Difference_0_to_60, fill = Origin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("darkblue","darkred","darkgreen")) +
  labs(x = "Taxa", y = "total nb. of species lost / gained after 60 years of deforestation", fill = "Origin") +
  theme_minimal() +
  ggtitle("Extinction Debt and Immigration Credit for Different Taxa and Origins in North Eastern Madagascar ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)





total_biodiversity$Remaining_Species <- total_biodiversity$Total_Species_Sum + total_biodiversity$Sum_Difference_0_to_60


ggplot(total_biodiversity, aes(x = Origin, y = Sum_Difference_0_to_60)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.5) +  # Filled bars with partial transparency
  geom_rect(aes(xmin = Origin, xmax = Origin, ymin = 0, ymax = Remaining_Species), fill = "white", color = "black") +  # Outlined rectangles for total species
  labs(x = "Origin", y = "Change in Species Abundance", fill = "Species After Change") +  # Adjust fill legend label
  theme_minimal() +
  ggtitle("Species Change by Origin (Absolute Values)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

