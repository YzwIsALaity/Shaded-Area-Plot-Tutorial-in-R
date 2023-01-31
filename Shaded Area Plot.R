# Packages
library(lubridate)
library(MMWRweek)
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)
library(ggthemes)

# Read dataset
Dt <- read.csv('Seattle Flu Study Prevalence.csv')
kable(head(Dt))

# Version 1: stacked
ggplot(Dt, aes(x = Index)) +                      # set up x-axis as numerical Index
  geom_area(aes(y = Count,                        # pass Count for y axis
                fill = Virus)) +                  # fill = Virus to set up different colors for viruses
  scale_x_continuous(labels = unique(Dt$Date),    # set up x-axis label as unique calendar time 
                     breaks = unique(Dt$Index)) + 
  ylab('Frequency of Detected Virus') + xlab('Calendar Time (Year-Month)') + 
  scale_fill_tableau("Tableau 10") +              # we will use the color palette from Tableau
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these three are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        axis.line.x = element_line(),                               
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11,     # rotate text in x axis 45 degrees and move 
                                   angle = 45, hjust = 1),          # text to the left side for 1 unit 
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold'))  

# Extract records for Adenovirus and Enterovirus
Dt.Sub <- Dt[which(Dt$Virus %in% c('Adenovirus', 'Enterovirus')), ]
# Check distribution of Count and Prevalence
summary(Dt.Sub[, c('Count', 'Prevalence')])

# Create labels for 'Count' and 'Prevalence' 
Dt.Sub$Virus_Label <- factor(ifelse(Dt.Sub$Virus == 'Adenovirus', 'Diagnoses, Adenovirus', 'Diagnoses, Enterovirus'), 
                             levels = c('Diagnoses, Adenovirus', 'Diagnoses, Enterovirus'))
Dt.Sub$Axis_Label <- factor(ifelse(Dt.Sub$Virus == 'Adenovirus', 'Prevalence, Adenovirus', 'Prevalence, Enterovirus'), 
                            levels = c('Prevalence, Adenovirus', 'Prevalence, Enterovirus'))

# Version 2: bar + shaded area
## preselected color for different labels
Col <- c('gold2', 'purple', 'green4', 'plum3')
## figure
ggplot(Dt.Sub, aes(x = Index)) + 
  geom_bar(aes(y = Count, fill = Virus_Label), stat = 'identity', position = position_dodge()) +
  geom_ribbon(aes(x = Index, 
                  ymin = 0, ymax = Prevalence * 325 / 0.11, 
                  fill = Axis_Label)) + 
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.11/325, name = 'Prevalence')) + 
  scale_x_continuous(labels = unique(Dt$Date),                      # set up x-axis label as unique calendar time 
                     breaks = unique(Dt$Index)) + 
  scale_fill_manual(values = c(Col[1:2], alpha(Col[3], 0.3), alpha(Col[4], 0.6))) + 
  ylab('No. of Cases') + xlab('Calendar Time (Year-Month)') + 
  guides(fill = guide_legend(title = "")) + 
  theme_bw() +
  theme(axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.title.x = element_text(color = 'black', face = "bold"),
        axis.title.y = element_text(color = 'black', face = "bold"),
        axis.text.x = element_text(color = 'black', face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(color = 'black', face = "bold"),
        legend.text = element_text(color = 'black', face = "bold"),
        legend.title = element_text(face = "bold", color = 'black'),
        plot.title = element_text(hjust = 0.5, face = "bold", color = 'black'))





