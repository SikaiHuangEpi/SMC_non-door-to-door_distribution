options(digits = 2)
# Load in excel file
library(readxl)
Analysis <- read_excel("~/Desktop/SMC/Door_to_door/table/Analysis.xlsx", sheet = "regression_results", range = "A1:F119")

str(Analysis)

# indent the subgroup if there is a number in the placebo column
Analysis$Predictors <- ifelse(is.na(Analysis$`Non-DDD`), 
                      Analysis$Predictors,
                      paste0("   ", Analysis$Predictors))


# NA to blank or NA will be transformed to charachter.
Analysis$`DDD` <- ifelse(is.na(Analysis$`DDD`), "", Analysis$`DDD`)
Analysis$`Non-DDD` <- ifelse(is.na(Analysis$`Non-DDD`), "", Analysis$`Non-DDD`)
Analysis$se <- (log(Analysis$hi) - log(Analysis$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
Analysis$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
Analysis$`Crude OR (95% CI)` <- ifelse(Analysis$est == "1.00", "Ref",
                           sprintf("%s (%s to %s)", Analysis$est, Analysis$low, Analysis$hi))

Analysis$`Crude OR (95% CI)` <- ifelse(is.na(Analysis$`Crude OR (95% CI)`), "", Analysis$`Crude OR (95% CI)`)

# Draw forest plot with theme
library(forestploter) # Reference: https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
tm <- forest_theme(base_size = 8,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")

p_child <- forest(Analysis[c(1:11),c(1:3, 7:8)],
            est = as.numeric(Analysis$est),   # Point estimation
            lower = as.numeric(Analysis$low),  # Lower bound of the confidence interval, same as est.
            upper = as.numeric(Analysis$hi),  # Upper bound of the confidence interval, same as est.
            ci_column = 4, # Column number of the data the CI will be displayed
            ref_line = 1, # X-axis coordinates of zero line, default is 1. 
            arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
            xlim = c(0, 4),
            ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
            footnote = "This is for children.",
            theme = tm)

p_child <- edit_plot(p_child, row = 1, gp = gpar(fontface = "bold"))
p_child

p.caregiver <- Analysis[c(12:35),]
p_caregiver <- forest(p.caregiver[,c(1:3, 7:8)],
                  est = as.numeric(p.caregiver$est),   # Point estimation
                  lower = as.numeric(p.caregiver$low),  # Lower bound of the confidence interval, same as est.
                  upper = as.numeric(p.caregiver$hi),  # Upper bound of the confidence interval, same as est.
                  ci_column = 4, # Column number of the data the CI will be displayed
                  ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                  arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                  xlim = c(0, 4),
                  ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                  footnote = "This is for caregiver.",
                  theme = tm)
p_caregiver <- edit_plot(p_caregiver, row = 1, gp = gpar(fontface = "bold"))
p_caregiver

p.householdhead <- Analysis[c(36:59),]
p_householdhead <- forest(p.householdhead[,c(1:3, 7:8)],
                      est = as.numeric(p.householdhead$est),   # Point estimation
                      lower = as.numeric(p.householdhead$low),  # Lower bound of the confidence interval, same as est.
                      upper = as.numeric(p.householdhead$hi),  # Upper bound of the confidence interval, same as est.
                      ci_column = 4, # Column number of the data the CI will be displayed
                      ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                      arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                      xlim = c(0, 4),
                      ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                      footnote = "This is for household head.",
                      theme = tm)
p_householdhead <- edit_plot(p_householdhead, row = 1, gp = gpar(fontface = "bold"))
p_householdhead

p.household <- Analysis[c(60:76),]
p_household <- forest(p.household[,c(1:3, 7:8)],
                          est = as.numeric(p.household$est),   # Point estimation
                          lower = as.numeric(p.household$low),  # Lower bound of the confidence interval, same as est.
                          upper = as.numeric(p.household$hi),  # Upper bound of the confidence interval, same as est.
                          ci_column = 4, # Column number of the data the CI will be displayed
                          ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                          arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                          xlim = c(0, 4),
                          ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                          footnote = "This is for household.",
                          theme = tm)
p_household <- edit_plot(p_household, row = 1, gp = gpar(fontface = "bold"))
p_household


p.heard <- Analysis[c(77:101),]
p_heard <- forest(p.heard[,c(1:3, 7:8)],
                      est = as.numeric(p.heard$est),   # Point estimation
                      lower = as.numeric(p.heard$low),  # Lower bound of the confidence interval, same as est.
                      upper = as.numeric(p.heard$hi),  # Upper bound of the confidence interval, same as est.
                      ci_column = 4, # Column number of the data the CI will be displayed
                      ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                      arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                      xlim = c(0, 4),
                      ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                      footnote = "This is caregiver information sources toward SMC campagin.",
                      theme = tm)
p_heard <- edit_plot(p_heard, row = 1, gp = gpar(fontface = "bold"))
p_heard

p.knowledge <- Analysis[c(77,102:119),]
p_knowledge <- forest(p.knowledge[,c(1:3, 7:8)],
                  est = as.numeric(p.knowledge$est),   # Point estimation
                  lower = as.numeric(p.knowledge$low),  # Lower bound of the confidence interval, same as est.
                  upper = as.numeric(p.knowledge$hi),  # Upper bound of the confidence interval, same as est.
                  ci_column = 4, # Column number of the data the CI will be displayed
                  ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                  arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                  xlim = c(0, 4),
                  ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                  footnote = "This is for caregiver's knowledge toward SMC campagin.",
                  theme = tm)
p_knowledge <- edit_plot(p_knowledge, row = 1, gp = gpar(fontface = "bold"))
p_knowledge

# Open a new PDF device
pdf("uni_reg.pdf",height = 12, width = 9)

# Plot your forest plot
p_child
p_caregiver
p_householdhead
p_household
p_heard
p_knowledge

# Close the PDF device and save the plot to the file
dev.off()

# overall ####
p_overall <- Analysis[c(1:118),]
p_overall <- forest(p_overall[,c(1:3, 7:8)],
                    est = as.numeric(p_overall$est),   # Point estimation
                    lower = as.numeric(p_overall$low),  # Lower bound of the confidence interval, same as est.
                    upper = as.numeric(p_overall$hi),  # Upper bound of the confidence interval, same as est.
                    ci_column = 4, # Column number of the data the CI will be displayed
                    ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                    arrow_lab = c("Less likely to be non-DDD", "More likely to be non-DDD"),
                    xlim = c(0, 4),
                    ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                    theme = tm)

p_overall <- edit_plot(p_overall, row = c(2, 13, 34,58), which = "background",
               gp = gpar(fill = "#D2E8E8")) 


p_overall <- edit_plot(p_overall, row = c(1, 69, 76), which = "background",
               gp = gpar(fill = "#089f8f")) 

p_overall <- edit_plot(p_overall, row = c(2, 13, 34,58), gp = gpar(fontface = "bold"))

p_overall <- edit_plot(p_overall, row = c(1, 69, 76), gp = gpar(col = "white", fontface = "bold"))

p_overall <- add_border(p_overall, row = c(0,1, 119), where = "top")

pdf("overall.pdf",height = 30, width = 9)
p_overall
dev.off()
# forward selection p<0.20 ####
Analysis <- read_excel("~/Desktop/Door_to_door/table/Analysis.xlsx", sheet = "regression_results", range = "H1:M69")

str(Analysis)


# indent the subgroup if there is a number in the placebo column
Analysis$Predictors <- ifelse(is.na(Analysis$`Non-DDD`), 
                              Analysis$Predictors,
                              paste0("   ", Analysis$Predictors))


# NA to blank or NA will be transformed to charachter.
Analysis$`DDD` <- ifelse(is.na(Analysis$`DDD`), "", Analysis$`DDD`)
Analysis$`Non-DDD` <- ifelse(is.na(Analysis$`Non-DDD`), "", Analysis$`Non-DDD`)
Analysis$se <- (log(Analysis$hi) - log(Analysis$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
Analysis$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
Analysis$`Crude OR (95% CI)` <- ifelse(Analysis$est == "1.00", "Ref",
                                       sprintf("%s (%s to %s)", Analysis$est, Analysis$low, Analysis$hi))

Analysis$`Crude OR (95% CI)` <- ifelse(is.na(Analysis$`Crude OR (95% CI)`), "", Analysis$`Crude OR (95% CI)`)

# Draw forest plot with theme

tm <- forest_theme(base_size = 8,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")


p.child_caregiver_hh <- Analysis[c(1:35),]
p_child_caregiver_hh <- forest(p.child_caregiver_hh[,c(1:3, 7:8)],
                      est = as.numeric(p.child_caregiver_hh$est),   # Point estimation
                      lower = as.numeric(p.child_caregiver_hh$low),  # Lower bound of the confidence interval, same as est.
                      upper = as.numeric(p.child_caregiver_hh$hi),  # Upper bound of the confidence interval, same as est.
                      ci_column = 4, # Column number of the data the CI will be displayed
                      ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                      arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                      xlim = c(0, 4),
                      ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                      footnote = "This is the demo plot.",
                      theme = tm)
p_child_caregiver_hh

p.household <- Analysis[c(36:68),]
p_household <- forest(p.household[,c(1:3, 7:8)],
                      est = as.numeric(p.household$est),   # Point estimation
                      lower = as.numeric(p.household$low),  # Lower bound of the confidence interval, same as est.
                      upper = as.numeric(p.household$hi),  # Upper bound of the confidence interval, same as est.
                      ci_column = 4, # Column number of the data the CI will be displayed
                      ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                      arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                      xlim = c(0, 4),
                      ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                      footnote = "This is the demo plot.",
                      theme = tm)

p_household


# Open a new PDF device
pdf("multi_reg_forward_p2.0.pdf")

# Plot your forest plot
p_child_caregiver_hh
p_household

# Close the PDF device and save the plot to the file
dev.off()




# forward selection p<0.15 ####
Analysis <- read_excel("~/Desktop/SMC/Door_to_door/table/Analysis.xlsx", sheet = "regression_results", range = "O1:T56")

str(Analysis)


# indent the subgroup if there is a number in the placebo column
Analysis$Predictors <- ifelse(is.na(Analysis$`Non-DDD`), 
                              Analysis$Predictors,
                              paste0("   ", Analysis$Predictors))


# NA to blank or NA will be transformed to charachter.
Analysis$`DDD` <- ifelse(is.na(Analysis$`DDD`), "", Analysis$`DDD`)
Analysis$`Non-DDD` <- ifelse(is.na(Analysis$`Non-DDD`), "", Analysis$`Non-DDD`)
Analysis$se <- (log(Analysis$hi) - log(Analysis$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
Analysis$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
Analysis$`Crude OR (95% CI)` <- ifelse(Analysis$est == "1.00", "Ref",
                                       sprintf("%s (%s to %s)", Analysis$est, Analysis$low, Analysis$hi))

Analysis$`Crude OR (95% CI)` <- ifelse(is.na(Analysis$`Crude OR (95% CI)`), "", Analysis$`Crude OR (95% CI)`)

# Draw forest plot with theme

tm <- forest_theme(base_size = 8,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")

p <- forest(Analysis[,c(1:3, 7:8)],
                               est = as.numeric(Analysis$est),   # Point estimation
                               lower = as.numeric(Analysis$low),  # Lower bound of the confidence interval, same as est.
                               upper = as.numeric(Analysis$hi),  # Upper bound of the confidence interval, same as est.
                               ci_column = 4, # Column number of the data the CI will be displayed
                               ref_line = 1, # X-axis coordinates of zero line, default is 1. 
                               arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
                               xlim = c(0, 4),
                               ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
                               footnote = "This is the demo plot.",
                               theme = tm)


# Open a new PDF device
pdf("multi_reg_forward_p0.15.pdf", height = 15, width = 8)

# Plot your forest plot
p

# Close the PDF device and save the plot to the file
dev.off()




# forward selection p<0.10 ####
Analysis <- read_excel("~/Desktop/Door_to_door/table/Analysis.xlsx", sheet = "regression_results", range = "V1:AA36")

str(Analysis)


# indent the subgroup if there is a number in the placebo column
Analysis$Predictors <- ifelse(is.na(Analysis$`Non-DDD`), 
                              Analysis$Predictors,
                              paste0("   ", Analysis$Predictors))


# NA to blank or NA will be transformed to charachter.
Analysis$`DDD` <- ifelse(is.na(Analysis$`DDD`), "", Analysis$`DDD`)
Analysis$`Non-DDD` <- ifelse(is.na(Analysis$`Non-DDD`), "", Analysis$`Non-DDD`)
Analysis$se <- (log(Analysis$hi) - log(Analysis$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
Analysis$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
Analysis$`Crude OR (95% CI)` <- ifelse(Analysis$est == "1.00", "Ref",
                                       sprintf("%s (%s to %s)", Analysis$est, Analysis$low, Analysis$hi))

Analysis$`Crude OR (95% CI)` <- ifelse(is.na(Analysis$`Crude OR (95% CI)`), "", Analysis$`Crude OR (95% CI)`)

# Draw forest plot with theme

tm <- forest_theme(base_size = 8,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")

p <- forest(Analysis[,c(1:3, 7:8)],
            est = as.numeric(Analysis$est),   # Point estimation
            lower = as.numeric(Analysis$low),  # Lower bound of the confidence interval, same as est.
            upper = as.numeric(Analysis$hi),  # Upper bound of the confidence interval, same as est.
            ci_column = 4, # Column number of the data the CI will be displayed
            ref_line = 1, # X-axis coordinates of zero line, default is 1. 
            arrow_lab = c("Less likely to be outside treatment", "More likely to be outside treatment"),
            xlim = c(0, 4),
            ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
            footnote = "This is the demo plot.",
            theme = tm)

# Open a new PDF device
pdf("multi_reg_forward_p0.10.pdf", height = 15, width = 8)

# Plot your forest plot
p

# Close the PDF device and save the plot to the file
dev.off()


# forward selection p<0.05 for viva ####
library(readxl)
Analysis <- read_excel("~/Desktop/SMC/Door_to_door/table/Analysis.xlsx", sheet = "regression_results", range = "AC1:AH39")

str(Analysis)


# indent the subgroup if there is a number in the placebo column
Analysis$Predictors <- ifelse(is.na(Analysis$`Non-DDD`), 
                              Analysis$Predictors,
                              paste0("   ", Analysis$Predictors))


# NA to blank or NA will be transformed to charachter.
Analysis$`DDD` <- ifelse(is.na(Analysis$`DDD`), "", Analysis$`DDD`)
Analysis$`Non-DDD` <- ifelse(is.na(Analysis$`Non-DDD`), "", Analysis$`Non-DDD`)
Analysis$se <- (log(Analysis$hi) - log(Analysis$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
Analysis$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
Analysis$`OR (95% CI)` <- ifelse(Analysis$est == "1.00", "Ref",
                                       sprintf("%s (%s to %s)", Analysis$est, Analysis$low, Analysis$hi))

Analysis$`OR (95% CI)` <- ifelse(is.na(Analysis$`OR (95% CI)`), "", Analysis$`OR (95% CI)`)

# Draw forest plot with theme
library(forestploter)
tm <- forest_theme(base_size = 8,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")

p <- forest(Analysis[,c(1:3, 7:8)],
            est = as.numeric(Analysis$est),   # Point estimation
            lower = as.numeric(Analysis$low),  # Lower bound of the confidence interval, same as est.
            upper = as.numeric(Analysis$hi),  # Upper bound of the confidence interval, same as est.
            ci_column = 4, # Column number of the data the CI will be displayed
            ref_line = 1, # X-axis coordinates of zero line, default is 1. 
            arrow_lab = c("Less likely to be non-DDD", "More likely to be non-DDD"),
            xlim = c(0, 4),
            ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
            theme = tm)

library(forestplot)
p <- edit_plot(p, row = c(2, 12, 20), which = "background",
               gp = gpar(fill = "#D2E8E8")) 


p <- edit_plot(p, row = c(1, 28, 32), which = "background",
               gp = gpar(fill = "#089f8f")) 

p <- edit_plot(p, row = c(2, 12, 20), gp = gpar(fontface = "bold"))

p <- edit_plot(p, row = c(1, 28, 32), gp = gpar(col = "white", fontface = "bold"))

p <- add_border(p, row = c(0,1, 39), where = "top")
p
# Open a new PDF device
pdf("multi_reg_forward_p0.05.pdf", height = 12, width = 9)

# Plot your forest plot
p

# Close the PDF device and save the plot to the file
dev.off()

# forward selection p<0.05 ####
library(readxl)
Analysis <- read_excel("~/Desktop/SMC/Door_to_door/table/Analysis.xlsx", sheet = "regression_results", range = "AJ1:AO18")

str(Analysis)


# indent the subgroup if there is a number in the placebo column
Analysis$Predictors <- ifelse(is.na(Analysis$`Non-DDD`), 
                              Analysis$Predictors,
                              paste0("   ", Analysis$Predictors))


# NA to blank or NA will be transformed to charachter.
Analysis$`DDD` <- ifelse(is.na(Analysis$`DDD`), "", Analysis$`DDD`)
Analysis$`Non-DDD` <- ifelse(is.na(Analysis$`Non-DDD`), "", Analysis$`Non-DDD`)
Analysis$se <- (log(Analysis$hi) - log(Analysis$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
Analysis$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
Analysis$`OR (95% CI)` <- ifelse(Analysis$est == "1.00", "Ref",
                                 sprintf("%s (%s to %s)", Analysis$est, Analysis$low, Analysis$hi))

Analysis$`OR (95% CI)` <- ifelse(is.na(Analysis$`OR (95% CI)`), "", Analysis$`OR (95% CI)`)

# Draw forest plot with theme
library(forestploter)
tm <- forest_theme(base_size = 8,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")

p <- forest(Analysis[,c(1:3, 7:8)],
            est = as.numeric(Analysis$est),   # Point estimation
            lower = as.numeric(Analysis$low),  # Lower bound of the confidence interval, same as est.
            upper = as.numeric(Analysis$hi),  # Upper bound of the confidence interval, same as est.
            ci_column = 4, # Column number of the data the CI will be displayed
            ref_line = 1, # X-axis coordinates of zero line, default is 1. 
            arrow_lab = c("Less likely to be non-DDD", "More likely to be non-DDD"),
            xlim = c(0, 4),
            ticks_at = c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0), # Set X-axis tick-marks point.
            theme = tm)

p

library(forestplot)
p <- edit_plot(p, row = c(2, 6), which = "background",
               gp = gpar(fill = "#D2E8E8")) 


p <- edit_plot(p, row = c(1, 14), which = "background",
               gp = gpar(fill = "#089f8f")) 

p <- edit_plot(p, row = c(2, 6), gp = gpar(fontface = "bold"))

p <- edit_plot(p, row = c(1, 14), gp = gpar(col = "white", fontface = "bold"))

p <- add_border(p, row = c(0,1, 18), where = "top")
p
# Open a new PDF device
pdf("multi_reg_forward_p0.05_viva.pdf", height = 12, width = 9)

# Plot your forest plot
p

# Close the PDF device and save the plot to the file
dev.off()


library(sf)

