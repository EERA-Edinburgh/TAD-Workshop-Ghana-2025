# load packages
library(ggplot2)
#OR
library(tidyverse)
library(here)

# import dataset
nba <- read_csv(here("data","NBA.csv"))

# Scatter plot
ggplot(data = nba, aes(x = Weight, y = Height, colour = Pos)) + # added colour = POS
  geom_point() + 
stat_smooth(method = "lm", se = FALSE) + 
scale_colour_brewer(palette="Dark2") # scale maps aes data values (x and y) to aes colour 

ggplot(data = nba, aes(x = Weight, y = Height, colour = Pos)) + 
  geom_point() + 
stat_smooth(method = "lm", se = FALSE) 

ggplot(data = nba, aes(x = Weight, y = Height, colour = Pos)) + 
  geom_point() + 
stat_smooth(method = "lm", se = FALSE) + 
scale_colour_brewer(palette="Dark2") + 
facet_grid(. ~ Pos) # split grid by the variable Pos

# Histogram
p <- ggplot(data=nba, aes(x=Age)) +
  geom_histogram( fill="blue", colour="purple") + 
  labs(title="Histogram of the Age of NBA players", 
       x="Age (years)") +
  theme_bw()
p

# Boxplot
ggplot(data=nba) +
  geom_boxplot(aes(x=Pos, y=Height), fill="lightgrey", 
               colour="purple") +
  labs(x="Position", 
       y="Height (in)",
       title="Boxplot of Height per Position") + 
  theme_bw()

# Boxplot with Scatterplot
ggplot(data=nba) +
geom_boxplot(aes(y=Height, x=Pos), fill="lightgrey", colour="purple") +
geom_jitter(aes(y=Height, x=Pos), colour="blue", size=0.2) + # geom_jitter spreads points 
#out so that they don't overlap
geom_boxplot(aes(y=Height, x=Pos), fill="lightgrey", colour="purple") +
labs(title="Boxplot of Height per Position", y="Height (in)", x="Position") + 
theme_bw()

# Barchart
ggplot(data=nba, aes(x=Pos, fill=Age21)) + 
    geom_bar(colour="black", stat="count",
             position=position_dodge(), # split bars by Age21 variable. 
                                        #try commenting this line out 
             size=.3) +                        # Thinner lines
    scale_fill_discrete(name="Age of player") +      # Set legend title
    labs(x = "Player Position", y = "Number of players", 
       title = "Barchart of player position by Age category") +     # Set labels
    theme_bw() # Set theme


# Likert plots

library(likert)
df <- read.csv(here("data","likert_plot_example.csv"))

# create an object with the order of agreement
scale <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")

# make sure all question columns are factors and have the right levels in the correct order
# simple but repetitive way
df$Q1 <- factor(df$Q1, levels = scale)
df$Q2 <- factor(df$Q2, levels = scale)
df$Q3 <- factor(df$Q3, levels = scale)
df$Q4 <- factor(df$Q4, levels = scale)

# fancy but faster way
df <- df %>%
  mutate(
    across(c(Q1, Q2, Q3, Q4), ~ factor(.x, levels = scale))
  )

likert_simple <- likert(select(df, -ID, -pre_post)) 
plot(likert_simple)

likert_facet <- likert(select(df, -ID, -pre_post), grouping = df$pre_post)
plot(likert_facet)

png(filename = "likert_facet.png", width = 9, height = 7, units = "in", res = 500)
plot(likert_facet, group.order = c('Pre', 'Post')) 
dev.off()

# Original plot
ggplot(nba, aes(x = Weight, y = Height, colour = Pos)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  scale_colour_brewer(palette="Dark2", 
                      name  ="Player \nPosition",
                      breaks=c("C", "F", "G"),
                      labels=c("Centre", "Forward", "Guard")) +
  facet_grid(. ~ Pos) +
  labs(x = "Weight (lbs)", 
       y = "Height (in)", 
       title = "Scatterplot of weight and height of \n NBA players by position") + 
  theme(axis.title = element_text(colour = "black", size = 14, face = "bold.italic"),
        strip.text = element_text(colour = "black", face = "bold.italic", size = 12),
        plot.title = element_text(colour = "black", size = 14, face = "bold.italic", hjust = 0.5),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size = 14, face = "bold") )


