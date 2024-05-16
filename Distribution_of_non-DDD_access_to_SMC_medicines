# Load in data
library(haven)
Nigeria <- read_dta("~/Desktop/SMC/Door_to_door/data/Nigeria_v3.dta")
View(Nigeria)

# 
hist(Nigeria$drug_not_distributor)

library(ggplot2)

# Step 1: Create a dataframe
# Create a data frame with the given data
df <- data.frame(
  Category = c("Family or friend", "Health facility staff", "Fixed point distribution by SMC distributors", 
               "Unofficial fixed point distribution", "Private purchase", "Distribution by SMC distributors in another location", 
               "Other source"),
  Proportion.Freq = c(15.5, 39.5, 9.7, 1.7,3.6, 25.4, 4.6)
)



# Step 3: Create bar plot with reversed order of categories
plot <- ggplot(df, aes(x = reorder(Category, Proportion.Freq), y = Proportion.Freq)) +
  geom_bar(stat = "identity", fill = "#08737f", width = 0.8) +
  xlab("Non-DDD access to SMC medicines") + ylab("Percent") +
  geom_text(aes(label = sprintf("%.1f", Proportion.Freq)),
            vjust = 0.4,
            hjust = -0.1,
            size = 5, color = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", size = 0.2),
        legend.position = "right",
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 18, color = "black"),
        axis.text = element_text(size = 18, color = "black", margin = margin(t = 0, r = 30, b = 30, l = 0))) +
  coord_flip() 


plot

# Step 4: save plot in exact scale and high resolution
pdf(file="non_ddd_access.pdf", width = 16, height = 8)

plot

dev.off()



# Step 5: created bar plot group by door-to-door visit or non-door-to-door visit
df <- data.frame(
  Category = c("Family or friend", "Health facility staff", "Fixed point distribution by SMC distributors", 
               "Unofficial fixed point distribution", "Private purchase", "Distribution by SMC distributors in another location", 
               "Other source", "Family or friend", "Health facility staff", "Fixed point distribution by SMC distributors", 
               "Unofficial fixed point distribution", "Private purchase", "Distribution by SMC distributors in another location", 
               "Other source"),
  Group = rep(c("Visit (N = 76)", "Non-visit (N = 238)"), each = 7),
  Proportion.Freq = c(22.5, 46.0, 6.4, 3.5, 5.4, 10.0, 6.1, 13.5, 37.6, 10.7, 1.1, 3.1, 30.0, 4.1)
)
# Print the data frame
df


# Step 6: Sort data frame
df <- df[order(-df$Proportion.Freq), ]

# Switch the order of levels in the "Group" variable
df$Group <- factor(df$Group, levels = c( "Visit (N = 76)", "Non-visit (N = 238)"))

plot <- ggplot(df, aes(x = reorder(Category, Proportion.Freq), y = Proportion.Freq)) +
  geom_bar(stat = "identity", fill = "#08737f", width = 0.8) +
  xlab("Non-DDD access to SMC medicines") + ylab("Percent (%)") +
  geom_text(aes(label = sprintf("%.1f", Proportion.Freq)),
            vjust = 0.4,
            hjust = -0.1,
            size = 4, color = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", size = 0.2),
        legend.position = "right",
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 23, color = "black"),
        axis.text = element_text(size = 20, color = "black", margin = margin(t = 0, r = 30, b = 30, l = 0)),
        strip.text = element_text(size = 20, color = "white"),
        strip.background = element_rect(fill = "#2a4858")) +
  coord_flip() +
  facet_wrap(~Group)

plot

# Step 7: save plot in exact scale and high resolution
pdf(file="Source_of_SMC_amongest_treated_children_by_visit.pdf", width = 15)

plot

dev.off()


# Step 8: althernative style using position in "dodge" 
plot <- ggplot(df, aes(x = reorder(Category, Proportion.Freq), y = Proportion.Freq, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  xlab("Access of SMC medicines") + ylab("Percent") +
  geom_text(aes(label = sprintf("%.1f", Proportion.Freq)),
            position = position_dodge(width = 0.8),
            vjust = 0.6,
            hjust = -0.1,
            size = 4, color = "black") +
  scale_fill_manual(values = c("#08737f", "#2a4858")) +  # Set fill colors manually
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", size = 0.2),
        legend.position = "right",
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        axis.title = element_text(size = 15, color = "black"),
        axis.text = element_text(size = 13, color = "black", margin = margin(t = 0, r = 30, b = 30, l = 0)),
        strip.text = element_text(size = 13, color = "white"),
        strip.background = element_rect(fill = "#2a4858")) +
  coord_flip()

plot
