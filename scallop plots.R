

library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)





#################################################
#LINE CHART FOR CPUE COOK INLET FISHERY TABLE 
###############################################


# Create the data frame
cook_inlet <- data.frame(
  year = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 
           2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
           2014, 2015, 2016, 2017),
  mt_cpue = c(45, NA, 53, 52, 44, 63, 75, 62, 28, 18,
              17, 20, 5, NA, NA, NA, 26, 31, 30, NA,
              NA, 21, 15, NA)
)

# Replace NA values with zeros
cook_inlet$mt_cpue[is.na(cook_inlet$mt_cpue)] <- 0

ggplot(cook_inlet, aes(x = year, y = mt_cpue)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  theme_minimal() +
  labs(title = "Cook Inlet Fishery: mt_cpue by Year (NA replaced with zero)",
       x = "Year",
       y = "mt_cpue") +
  theme(plot.title = element_text(hjust = 0.5))
###############################################################################
## BAR CHART FOR CPUE COOK INLET FISHERY TABLE 
##################################################################################



cook_inlet <- data.frame(
  year = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
           2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
           2014, 2015, 2016, 2017),
  mt_cpue = c(45, NA, 53, 52, 44, 63, 75, 62, 28, 18,
              17, 20, 5, NA, NA, NA, 26, 31, 30, NA,
              NA, 21, 15, NA)
)

# Replace NA values 
cook_inlet$mt_cpue[is.na(cook_inlet$mt_cpue)] <- 0

# column for "Closed" or "No Participation"
cook_inlet$label <- ifelse(cook_inlet$year %in% c(2007, 2008, 2009), 
                           "No Participation", 
                           ifelse(cook_inlet$mt_cpue == 0, "Closed", ""))


ggplot(cook_inlet, aes(x = factor(year), y = mt_cpue)) +
  geom_bar(stat = "identity", fill = "grey80", color = "black") +
  geom_text(data = subset(cook_inlet, label != ""),
            aes(label = label),
            y = -2,
            angle = 90,
            hjust = -0.2,
            color = "black") +
  
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  # Change background to white
    panel.grid.major = element_line(color = "grey80"),             # Keep major grid lines
    panel.grid.minor = element_line(color = "grey90"),             # Keep minor grid lines
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),   
    plot.margin = margin(10, 10, 10, 10),    
    legend.title = element_text(face = "bold"),  
    legend.position = "bottom",  
    legend.box = "horizontal"    
  ) +
  
  labs(title = "Cook Inlet Fishery (Area H) - All Areas : CPUE by Year",
       x = "Year",
       y = "CPUE (mt/dredge hr.)") +
  
  theme(axis.text.x = element_text(angle = 90))




##############################################################################
## Catch from cook inlet fishery table - Retained pounds 
############################################################################


fishery_data <- read_csv("cook_inlet_fishery_table_1994_2017.csv")


fishery_data <- fishery_data %>%
  rename(year = season) %>%  # Rename 'season' to 'year'
  mutate(
    ghl = as.numeric(gsub(",", "", ghl)),  # Convert 'ghl' to numeric by removing commas
    ret_lbs_mt = as.numeric(gsub(",", "", ret_lbs_mt)),  # Convert 'ret_lbs_mt' to numeric by removing commas
    label = ifelse(year %in% c(1995, 2013, 2014), "Closed", "")  # Add 'label' column for closed years
  )

# years that need "Closed" labels
fishery_data$label <- ifelse(fishery_data$year %in% c(1995, 2013, 2014), "Closed", "")

#  retained pounds plot with GHL and "Closed" labels
retained_plot <- ggplot(fishery_data, aes(x = factor(year))) +
  
  # Retained Pounds bars
  geom_bar(aes(y = ret_lbs_mt, fill = "Retained Pounds (MT)"), stat = "identity", color = "black", width = 0.6) +
  
  # GHL lines
  geom_segment(aes(x = as.numeric(factor(year)), xend = as.numeric(factor(year)), 
                   y = 0, yend = ghl, color = "GHL"), size = 0.7) +
  geom_segment(aes(x = as.numeric(factor(year)) - 0.25, xend = as.numeric(factor(year)) + 0.25, 
                   y = ghl, yend = ghl, color = "GHL"), size = 0.7) +
  
  # "Closed" labels 
  geom_text(data = subset(fishery_data, label != ""),
            aes(label = label),
            y = max(fishery_data$ret_lbs_mt, na.rm = TRUE) * 0.00,  # Move up slightly
            angle = 90,
            hjust = -0.2,
            color = "black")+

  
  theme_minimal() +
  
  # Labels and Legends
  labs(
    title = "Retained Pounds of Meat (MT) by Year - All Areas",
    x = "Year",
    y = "Retained Pounds (MT)",
    fill = "Legend",  
    color = "Legend"
  ) +
  
  
  scale_fill_manual(values = c("Retained Pounds (MT)" = "gray70"), labels = c("Retained Pounds (MT)")) + 
  scale_color_manual(values = c("GHL" = "black"), labels = c("GHL")) +  
  scale_x_discrete(expand = expansion(add = c(0.3, 0.3))) +  
  
  # Formatting
  theme( panel.background = element_rect(fill = "white", colour = NA),  # Change background to white
         panel.grid.major = element_line(color = "grey80"),             # Keep major grid lines
         panel.grid.minor = element_line(color = "grey90"),       
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),   
    plot.margin = margin(10, 10, 10, 10),    
    legend.title = element_text(face = "bold"),  
    legend.position = "bottom",  
    legend.box = "horizontal"    
  )

# Print the plot
retained_plot


#############################################################
#Bar chart for abundance from adfg survey
#################################################################

# Load the CSV file
data <- read.csv("ADFG_FMR.csv")

# Convert abundance and confidence intervals to millions of pounds
data <- data %>%
  mutate(
    abundance_millions = abundance / 1e6, # Convert abundance to millions of pounds
    ci_millions = ci / 1e6                # Convert confidence intervals to millions of pounds
  )

# Define missing survey years for South
no_survey_years <- c(1996, 1999, 2001) 


no_survey_rows <- data.frame(
  Bed = "South",
  Year = no_survey_years,
  abundance_millions = NA,  # No bar will be drawn
  ci_millions = NA,         # No error bars
  label = "No Survey"       # Label will be plotted
)

# Combine with original dataset
data <- bind_rows(data, no_survey_rows)


ggplot(data, aes(x = factor(Year), y = abundance_millions, fill = Bed)) +
  

  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", na.rm = TRUE) +
  
  # Add error bars for confidence intervals
  geom_errorbar(aes(ymin = abundance_millions - ci_millions, ymax = abundance_millions + ci_millions),
                position = position_dodge(width = 0.9), width = 0.25, color = "black", na.rm = TRUE) +
  
  # Add vertical "No Survey" text for South in missing years
  geom_text(data = subset(data, !is.na(label) & Bed == "South"),
            aes(label = label, y = 0),  # Position text at y = 0
            position = position_dodge(width = 0.9),
            angle = -90,
            vjust = -0.7,
            hjust = 1,
            color = "black") +  
  

  theme_minimal() +
  
  labs(
    title = "Abundance per Year for North and South Beds (Millions of Pounds)",
    x = "Year",
    y = "Abundance (Millions of Pounds)",
    fill = "Region"
  ) +
  
  scale_fill_manual(values = c("North" = "gray40", "South" = "gray80")) + # Grayscale colors
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################
## LINE CHART FOR ABUNDANCE FROM ADFG SURVEY
################################################################

ggplot(data, aes(x = factor(Year), y = abundance_millions, color = Bed, group = Bed)) +
  
  
  geom_line(size = 1) +
  
  
  geom_point(size = 3) +
  
 
  geom_errorbar(aes(ymin = abundance_millions - ci_millions, ymax = abundance_millions + ci_millions),
                width = 0.25, color = "black") +
  
  
  theme_minimal() +
  
  labs(
    title = "Abundance per Year for North and South Beds (Millions of Pounds)",
    x = "Year",
    y = "Abundance (Millions of Pounds)",
    color = "Region"
  ) +
  
  scale_color_manual(values = c("North" = "blue", "South" = "red")) +  
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########################################################################
#NORTH AND SOUTH BED ABUNDANCE SEPARATELY
########################################################################



north_data_padded <- data %>% filter(Bed == "North") %>%
  mutate(Year = factor(Year, levels = 1996:2013)) # Ensure all years are represented

south_data_padded <- data %>% filter(Bed == "South") %>%
  bind_rows(tibble(Bed = "South", Year = 1996, abundance_millions = NA, ci_millions = NA)) %>%
  mutate(Year = factor(Year, levels = 1996:2013)) # Ensure all years are represented

# Set the same y-axis limit based on the maximum abundance for both plots
y_max <- max(north_data_padded$abundance_millions, na.rm = TRUE)

# North plot
north_plot <- ggplot(north_data_padded, aes(x = Year, y = abundance_millions, color = Bed, group = Bed)) +
  geom_line(size = 1) +  # Line connecting points
  geom_point(size = 3) + # Points
  geom_errorbar(aes(ymin = abundance_millions - ci_millions, ymax = abundance_millions + ci_millions),
                width = 0.25, color = "black") +
  labs(title = "North Bed Abundance (Millions of Pounds)", x = "Year",
       y = "Abundance") +
  scale_color_manual(values = c("North" = "gray40")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, y_max)) +
  scale_x_discrete(breaks = seq(1996, 2013, by = 1))  # Show every year for North

#  South plot
south_plot <- ggplot(south_data_padded, aes(x = Year, y = abundance_millions, color = Bed, group = Bed)) +
  geom_line(size = 1) +  # Line connecting points
  geom_point(size = 3) + # Points
  geom_errorbar(aes(ymin = abundance_millions - ci_millions, ymax = abundance_millions + ci_millions),
                width = 0.25, color = "black") +
  labs(title = "South Bed Abundance (Millions of Pounds)", x = "Year", y = "Abundance") +
  scale_color_manual(values = c("South" = "gray40")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, y_max)) +
  scale_x_discrete(breaks = seq(1996, 2013, by = 1))  # Show every year for South

# Combine both plots in a 1x2 layout
combined_plot <- grid.arrange(north_plot, south_plot, ncol = 1)

# Save the combined plot as a PNG
ggsave("abundance_comparison_separated_plots_with_line.png", combined_plot, width = 10, height = 8)
