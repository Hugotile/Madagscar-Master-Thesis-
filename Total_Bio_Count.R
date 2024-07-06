
#########################





#########################

#LOAD BIODIVERSITY ORIGIN

#########################





#########################






##########

#NEW HERBS

##########

#Data from Land-use intensification increases richness of native 
#And exotic herbaceous plants, but not endemics, in Malagasy vanilla landscapes

#https://datadryad.org/stash/dataset/doi:10.5061/dryad.pzgmsbcjp

# Specify the directory containing the data
file_path_all_Herbs <- "/Users/hugo/Desktop/madagascar/Biodiversity_data/Herbs/Data_species_by_site_matrix__final.csv"

# Import the Herbs Data_species_by_site_matrix__final
all_Herbs <- read.csv(file_path_all_Herbs, header = TRUE)

# Copy the first five columns to a new data frame
first_seven_columns <- all_Herbs[, 1:5]

# Convert the rest of the columns to presence/absence
presence_absence_df <- as.data.frame(lapply(all_Herbs[, -c(1:5)], function(x) as.integer(x > 0)))

# Combine the first three columns with the presence/absence data frame
all_Herbs <- cbind(first_seven_columns, presence_absence_df)



# Get the plot code columns
plot_codes <- colnames(all_Herbs)[grepl("PF|FF|HF|RP|VH|VL|VM|WF", colnames(all_Herbs))]

# Create a list to store the results
results <- list()

# Loop over the plot code columns
for (plot_code in plot_codes) {
  # Summarize the data for the current plot code
  summary <- all_Herbs %>%
    group_by(species.origin) %>%
    summarize(count = sum(!!sym(plot_code))) %>%
    spread(species.origin, count)
  
  # Add the summary to the results list
  results[[plot_code]] <- summary
}

# Combine the results into a single data frame
Herbs_origin <- bind_rows(results, .id = "plotcode")

# Rename the columns
colnames(Herbs_origin)[colnames(Herbs_origin) == "Endemic"] <- "EndemicHerbs"
colnames(Herbs_origin)[colnames(Herbs_origin) == "Exotic"] <- "ExoticHerbs"
colnames(Herbs_origin)[colnames(Herbs_origin) == "Native"] <- "NativeNonEndemicHerbs"
colnames(Herbs_origin)[colnames(Herbs_origin) == "Unknown origin "] <- "UnknownOriginHerbs"

# Add a new column 'All_Herbs' to the 'Herbs_origin' data frame
Herbs_origin$AllHerbs <- Herbs_origin$EndemicHerbs + Herbs_origin$ExoticHerbs + Herbs_origin$NativeNonEndemicHerbs

# Replace all "." by "-" in the column names
Herbs_origin$plotcode <- gsub("\\.", "-", Herbs_origin$plotcode)


Herbs_origin$plotcode[1:10] <- master_NA$plotcode[1:10]


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



##########

#NEW Tree

##########


#############################

#Had to combine two data set 

#############################

#1 trees in vanilla agroforestry_Appendix_S11.csv
#From https://osf.io/24z6p/
#For species origin ( Endemic/Native ect ) information 



#2 tree_species_by_site_matrix - Nature_communications_Wurz_etal.csv
#From https://osf.io/j54fx/?view_only=1bd699c5cda64023963e058254a33eec
#For plotcode count 


#Import #1 

# Set the file path
file_path_Tree_Origin <- "/Users/hugo/Desktop/madagascar/Biodiversity_data/Trees/trees in vanilla agroforestry_Appendix_S11.csv"

# Read the data from the CSV file into a data frame
Tree_Origin <- read.csv(file_path_Tree_Origin, header = TRUE)



#Import #2

# Set the file path
file_path_Tree_plotcode_count  <- "/Users/hugo/Desktop/madagascar/Biodiversity_data/Trees/tree_species_by_site_matrix - Nature_communications_Wurz_etal.csv"

# Read the data from the CSV file into a data frame
Tree_plotcode_count <- read.csv(file_path_Tree_plotcode_count, sep = ";", header = TRUE)





###

# Replace all "." by "-" in the column names
colnames(Tree_plotcode_count) <- gsub("\\.", "-", colnames(Tree_plotcode_count))

# Rename the first column of the Tree_Origin data frame
colnames(Tree_Origin)[1] <- "species"
colnames(Tree_Origin)[4] <- "origin"

# Extract the species and origin information from the Tree_Origin data frame
species_info <- Tree_Origin[, c("species", "origin")]


# Merge the species information with the Tree_plotcode_count data frame
merged_tree <- merge(Tree_plotcode_count, species_info, by = "species")

# Rearrange the columns in the merged data frame
merged_tree <- merged_tree[, c("species", "origin", setdiff(colnames(merged_tree), c("species", "origin")))]


# Get the plot code columns
plot_codes <- colnames(merged_tree)[grepl("PF|FF|HF|RP|VH|VL|VM|WF", colnames(merged_tree))]

# Create a list to store the results
results <- list()

# Loop over the plot code columns
for (plot_code in plot_codes) {
  # Summarize the data for the current plot code
  summary <- merged_tree %>%
    group_by(origin) %>%
    summarize(count = sum(!!sym(plot_code))) %>%
    spread(origin, count)
  
  # Add the summary to the results list
  results[[plot_code]] <- summary
}

Tree_origin <- bind_rows(results, .id = "plot_code")
colnames(Tree_origin)[colnames(Tree_origin) == "plot_code"] <- "plotcode"

# Load the dplyr package
library(dplyr)

# renaming the plotcode
Tree_origin<- Tree_origin %>%
  mutate(plotcode = case_when(
    plotcode == "MT-PF1" ~ "MT-OGF1",
    plotcode == "MT-PF2" ~ "MT-OGF2",
    plotcode == "MT-PF3" ~ "MT-OGF3",
    plotcode == "MT-PF4" ~ "MT-OGF4",
    plotcode == "MT-PF5" ~ "MT-OGF5",
    plotcode == "ME-PF1" ~ "ME-OGF1",
    plotcode == "ME-PF2" ~ "ME-OGF2",
    plotcode == "ME-PF3" ~ "ME-OGF3",
    plotcode == "ME-PF4" ~ "ME-OGF4",
    plotcode == "ME-PF5" ~ "ME-OGF5",
    TRUE ~ plotcode
  ))


# Rename the columns in the Tree_origin data frame
colnames(Tree_origin)[colnames(Tree_origin) == "endemic"] <- "EndemicTrees"
colnames(Tree_origin)[colnames(Tree_origin) == "introduced"] <- "ExoticTrees"
colnames(Tree_origin)[colnames(Tree_origin) == "native"] <- "NativeNonEndemicTrees"
colnames(Tree_origin)[colnames(Tree_origin) == "unknown"] <- "UnknownOriginTrees"




# Add a new column 'All_Trees' to the 'Tree_origin' data frame
Tree_origin$AllTrees <- Tree_origin$EndemicTrees + Tree_origin$ExoticTrees + Tree_origin$NativeNonEndemicTrees


#Tree_origin has 58 rows 
#2 withdrew V24-VL and V2-VM 
#10 missing Rice paddy = 0 
#10 missing Herbaceous fallow = 0 


Worked_bio<- merge(Herbs_origin, Tree_origin, by= "plotcode", all = TRUE)
# Replace all NA values with 0's
Worked_bio[is.na(Worked_bio)] <- 0

Worked_bio$UnknownOriginHerbs <- NULL
Worked_bio$UnknownOriginTrees <- NULL


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
# Bind Total Count
###################

Total_Biodiversity <- rbind(Total_Herbs, Total_Trees)


##########

#NEW BIRDS

##########

#Data from 
#Bird diversity and endemism along a land-use gradient in Madagascar
#: the conservation value of vanilla agroforests
#https://datadryad.org/stash/dataset/doi:10.5061/dryad.83bk3j9nm


################

#Data set import#2 has the count by plot and origin but 10 plots are missing 
#Need to take the missing information from data set import#1

################



#Import #1 

# Set the file path
file_path_Bird_plot_count <- "/Users/hugo/Desktop/madagascar/Biodiversity_data/birds/DATA_species_by_site_matrix.csv"

# Read the data from the CSV file into a data frame
Bird_plot_count <- read.csv(file_path_Bird_plot_count, header = TRUE)


# Replace all "." by "-" in the column names
colnames(Bird_plot_count) <- gsub("\\.", "-", colnames(Bird_plot_count))

# Rename the first column of the 'Bird_plot_count' data frame
colnames(Bird_plot_count)[1] <- "Species"




#Import #2 

# Set the file path
file_path_Bird_Origin <- "/Users/hugo/Desktop/madagascar/DiversityTurndata /osfstorage-archive/bird_species_by_site_matrix - Nature_communications_Wurz_etal.csv"

# Read the data from the CSV file into a data frame
Bird_Origin <- read.csv(file_path_Bird_Origin, header = TRUE)


# Replace all "." by "-" in the column names
colnames(Bird_Origin) <- gsub("\\.", "-", colnames(Bird_Origin))


# Merge the species information with the Bird_plot_count data frame
Birds_origin <- as.data.frame(c(Bird_Origin[,2:3], Bird_plot_count[,-1]))

# Update the 'origin' column for the "Helmeted Guineafowl" and "Common Mynah" species
Birds_origin$origin[Birds_origin$Species == "helmeted guineafowl"] <- "Exotic"
Birds_origin$origin[Birds_origin$Species == "common myna"] <- "Exotic"



# Get the plot code columns
plot_codes <- colnames(Birds_origin)[grepl("GF|PF|FF|HF|RP|VH|VL|VM|WF", colnames(Birds_origin))]

# Create a list to store the results
results <- list()

# Loop over the plot code columns
for (plot_code in plot_codes) {
  # Summarize the data for the current plot code
  summary <- Birds_origin %>%
    group_by(origin) %>%
    summarize(count = sum(!!sym(plot_code))) %>%
    spread(origin, count)
  
  # Add the summary to the results list
  results[[plot_code]] <- summary
}

# Combine the results into a single data frame
Birds_origin <- bind_rows(results, .id = "plot_code")



# Replace all "." by "-" in the column plot_code
# 

# Replace all "." with "-" in the 'plot_code' column
Birds_origin$plot_code <- gsub("\\.", "-", Birds_origin$plot_code)



# Add a new column Endemic to the 'Tree_origin' data frame
Birds_origin$Endemic <- Birds_origin$family + Birds_origin$genera + Birds_origin$subfamily+Birds_origin$species
Birds_origin$AllBirds <- Birds_origin$Endemic + Birds_origin$nonendemic + Birds_origin$Exotic

# Rename the columns
colnames(Birds_origin)[colnames(Birds_origin) == "Endemic"] <- "EndemicBirds"
colnames(Birds_origin)[colnames(Birds_origin) == "Exotic"] <- "ExoticBirds"
colnames(Birds_origin)[colnames(Birds_origin) == "nonendemic"] <- "NativeNonEndemicBirds"
colnames(Birds_origin)[colnames(Birds_origin) == "Unknown origin "] <- "UnknownOriginBirds"
colnames(Birds_origin)[colnames(Birds_origin) == "plot_code"] <- "plotcode"




# Create a new data frame 'new_Birds_origin' with columns 1, 2, 5, 9, and 10 from 'Birds_origin'
Birds_origin <- Birds_origin[, c(1, 2, 5, 9, 10)]

#######
#Warning : sometimes 1 species of difference between 
#Birds_origin and "master_NA" because of vasa parrot sp
Birds_origin$plotcode[1:10] <- master_NA$plotcode[1:10]


Worked_bio<- merge(Worked_bio, Birds_origin, by= "plotcode", all = TRUE)




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

# Bind 




Total_Biodiversity <- rbind(Total_Herbs, Total_Trees, Total_Birds)



##########

#NEW ANTS

##########



file_path_Ants <- "/Users/hugo/Desktop/madagascar/Biodiversity_data/Ants/ant_richness_enviro.csv"

# Read the data from the CSV file into a data frame
Ants_origin <- read.csv(file_path_Ants, header = TRUE)

Ants_origin <- Ants_origin[, c("plotcode", "richness_overall", "richness_exotic", "richness_endemic", "richness_non_endemic")]

# Rename the columns of the 'Ants_origin' data frame
colnames(Ants_origin)[colnames(Ants_origin) == "richness_endemic"] <- "EndemicAnts"
colnames(Ants_origin)[colnames(Ants_origin) == "richness_exotic"] <- "ExoticAnts"
colnames(Ants_origin)[colnames(Ants_origin) == "richness_non_endemic"] <- "NativeNonEndemicAnts"
colnames(Ants_origin)[colnames(Ants_origin) == "richness_overall"] <- "AllAnts"
colnames(Ants_origin)[colnames(Ants_origin) == "plot_code"] <- "plotcode"


Ants_origin$AllAnts <- Ants_origin$EndemicAnts + Ants_origin$NativeNonEndemicAnts + Ants_origin$ExoticAnts

Ants_origin$plotcode[1:10] <- master_NA$plotcode[1:10]

Worked_bio<- merge(Worked_bio, Ants_origin, by= "plotcode", all = TRUE)

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

Total_Biodiversity <- rbind(Total_Herbs, Total_Trees, Total_Birds, Total_Ants)




###############

#FROM master_NA

###############


#Data available at 
#https://zenodo.org/record/5554864

# Specify the directory containing the data
file_path_master_NA <- "/Users/hugo/Desktop/madagascar/Biodiversity_data/master/master.csv"

# Import the master_NA data 
master_NA <- read.csv(file_path_master_NA, , header = TRUE, stringsAsFactors = FALSE)





# Select the columns
master_NA_bio <- master_NA[, c("plotcode", "butf.sp.e", "butf.sp.a", "rept.sp.e", "rept.sp.a", "amph.sp.e", "amph.sp.a")]

# Rename the columns
colnames(master_NA_bio) <- c("plotcode", "EndemicButterflies", "AllButterflies", "EndemicReptiles", "AllReptiles", "EndemicAmphibians", "AllAmphibians")

# Load the dplyr package
library(dplyr)

master_NA <- master_NA %>%
  select(-c("bird.sp.a", "bird.sp.e", "butf.sp.e", "butf.sp.a", "hpla.sp.e", "hpla.sp.a", "rept.sp.e", "rept.sp.a", "tree.sp.a", "tree.sp.e", "ant.sp.a", "ant.sp.e", "amph.sp.e", "amph.sp.a"))



########################################################

#Warning 
#Total count of amphibian and reptiles not found 


########################################################
############################################################################


# There is no exotic Butterflies, Reptiles neither Amphibians
# So the difference between Endemic and All is a "native non endemic specie"

#############################################################################

# create those "NativeNonEndemic" and "Exotic" columns 

# Create new columns
master_NA_bio$NativeNonEndemicButterflies <- master_NA_bio$AllButterflies - master_NA_bio$EndemicButterflies
master_NA_bio$NativeNonEndemicReptiles <- master_NA_bio$AllReptiles - master_NA_bio$EndemicReptiles
master_NA_bio$NativeNonEndemicAmphibians <- master_NA_bio$AllAmphibians - master_NA_bio$EndemicAmphibians

# Create new columns and fill them with zeros

master_NA_bio$ExoticButterflies <- 0
master_NA_bio$ExoticReptiles <- 0
master_NA_bio$ExoticAmphibians <- 0

# Reorder columns
master_NA_bio <- master_NA_bio[, c("plotcode", 
                                   "EndemicButterflies", "NativeNonEndemicButterflies", "ExoticButterflies", "AllButterflies",
                                   "EndemicReptiles", "NativeNonEndemicReptiles","ExoticReptiles", "AllReptiles",
                                   "EndemicAmphibians", "NativeNonEndemicAmphibians", "ExoticAmphibians", "AllAmphibians")]









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




