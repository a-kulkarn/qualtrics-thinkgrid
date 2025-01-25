library(dplyr)
library(ragg)
library(magick)

# Remove all variables
rm(list = ls())
ls()

# Set WD (change path to your own directory)
setwd("/Users/zci7c/Library/CloudStorage/OneDrive-UniversityofVirginia/UVA/Year-Independent/Writing/Ongoing/Thinking-Grid/Taxicab Reanalysis/Affect Induction - Fall 2023")
source("ThinkingGridFunctions_ColorBrewer.R")

# Load CSV file (ensure the CSV is in the correct directory)
data <- read.csv("taxicab_Affect_Induction.csv")

# Now Call the Functions
pdf("thinking_grid_plots.pdf", width = 10, height = 10)

png("Neutral Video.png")
thinking_grid(data, "probe != 2", title = "Neutral Video",initial_max_percentage = .3,legend=TRUE, color_palette = "Blues")
dev.off()

png("Emotional Task.png")
thinking_grid(data, "probe == 2", title = "Emotional Task",initial_max_percentage = .3,legend=TRUE, color_palette = "Blues")
dev.off()

png("NeutralvsEmotional.png")
thinking_grid_compare(data,"probe != 2","probe==2", title = "Neutral Video (Blue) vs Emotional Task (Red)",initial_max_percentage = .3)
dev.off()



thinking_grid_compare(data,"probe > 2 & val == 0","probe > 2 & val == 0", title = "Blank Grid",initial_max_percentage = .3)

thinking_grid(data, "probe > 2 & condition == 'Negative'", title = "Negative",initial_max_percentage = .3,legend=TRUE)
thinking_grid(data, "probe > 2 & condition == 'Positive'", title = "Positive",initial_max_percentage = .3,legend=TRUE)

thinking_grid_compare(data,"probe > 2 & condition == 'Positive'","probe > 2 & condition == 'Negative'", title = "Conditions",initial_max_percentage = .3)
thinking_grid_compare(data,"probe > 2 & val == 0","probe > 2 & val >0", title = "Neutral - Positive",initial_max_percentage = .3)
thinking_grid_compare(data,"probe > 2 & val == 0","probe > 2 & val < 0", title = "Neutral - Negative",initial_max_percentage = .3)
thinking_grid_compare(data,"probe > 2 & val == 0","probe > 2 & val != 0", title = "Neutral - Valenced",initial_max_percentage = .3)
thinking_grid_compare(data,"probe > 2 & val > 0","probe > 2 & val < 0", title = "Positive - Negative",initial_max_percentage = .3)
thinking_grid_compare(data,"probe > 2","probe < 3 ", title = "Rest vs Affect Induction Task",initial_max_percentage = .3)



#thinking_grid_compare(data,"thought=='FMT'","thought=='ST'", title = "Mind-Wandering - Rumination",initial_max_percentage = .6)
#thinking_grid_compare(data,"thought=='ST'","thought=='DT'", title = "Rumination - Directed",initial_max_percentage = .6)

dev.off()


#Create a GIF

#Create five separate images
max_percentage_gif <-- .4 

png("val_n2.png")
thinking_grid(data, "probe > 2 & val == -2", title = "Valence = -2",initial_max_percentage = max_percentage_gif,legend=TRUE)
dev.off()

png("val_n1.png")
thinking_grid(data, "probe > 2 & val == -1", title = "Valence = -1",initial_max_percentage = max_percentage_gif,legend=TRUE)
dev.off()

png("val_0.png")
thinking_grid(data, "probe > 2 & val == 0", title = "Valence = 0",initial_max_percentage = max_percentage_gif,legend=TRUE)
dev.off()

png("val_1.png")
thinking_grid(data, "probe > 2 & val == 1", title = "Valence = 1",initial_max_percentage = max_percentage_gif,legend=TRUE)
dev.off()

png("val_2.png")
thinking_grid(data, "probe > 2 & val == 2", title = "Valence = 2",initial_max_percentage = max_percentage_gif,legend=TRUE)
dev.off()

# Load the images
img1 <- image_read("val_n2.png")
img2 <- image_read("val_n1.png")
img3 <- image_read("val_0.png")
img4 <- image_read("val_1.png")
img5 <- image_read("val_2.png")

# Combine the images into an animation
animation <- image_animate(c(img1, img2, img3, img4, img5), fps = .5)

# Save the animation
image_write(animation, "affect_induction_valence.gif")


