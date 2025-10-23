library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)

# Pivot EEG PSD data
your_data_long <- your_data %>%
  pivot_longer(
    cols = c(A_avg_psd_ROI, B_avg_psd_ROI),
    names_to = "Band",
    values_to = "psd"
  )

# Pivot NIH Cognitive Scores
your_data_cog_long <- your_data %>%
  pivot_longer(
    cols = c(NIH_Flanker_Uncorr_Stnd, NIH_Card_Sort_Uncorr_Stnd,
             NIH_List_Sort_Uncorr_Stnd, NIH_Processing_Uncorr_Stnd),
    names_to = "Outcome",
    values_to = "score"
  )

# Save all plots to PDF

pdf("EEG_Cognition_Plots.pdf", width = 7, height = 5)

# Function to apply consistent theme
my_theme <- theme_minimal(base_size = 10) + 
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

add_stats <- function(data, xvar, yvar, groupvar) {
  stats <- data %>%
    group_by(!!sym(groupvar)) %>%
    do(tidy(lm(reformulate(xvar, yvar), data = .))) %>%
    filter(term == xvar) %>%
    mutate(label = sprintf("β = %.3f, p = %.3f", estimate, p.value))
  return(stats)
}

# --------------------------
# EEG PSD Plots
# --------------------------
eeg_colors <- c("B_avg_psd_ROI" = "blue", "A_avg_psd_ROI" = "purple")

p1 <- ggplot(your_data_long, aes(x = PANAS_PositiveAffect, y = psd, color = Band, fill = Band)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Positive Affect (PANAS)", y = "Power Spectral Density (µV²)") +
  scale_color_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  scale_fill_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(10, 45)) +
  scale_y_continuous(limits = c(0, 0.08))
print(p1)

p2 <- ggplot(your_data_long, aes(x = PCEs, y = psd, color = Band, fill = Band)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Positive Childhood Experiences (PCEs)", y = "Power Spectral Density (µV²)") +
  scale_color_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  scale_fill_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(0, 5), breaks = 0:7) +
  scale_y_continuous(limits = c(0, 0.08))
print(p2)

p3 <- ggplot(your_data_long, aes(x = Age, y = psd, color = Band, fill = Band)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Age (years)", y = "Power Spectral Density (µV²)") +
  scale_color_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  scale_fill_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(7, 12.5)) +
  scale_y_continuous(limits = c(0, 0.08))
print(p3)

p4 <- ggplot(your_data_long, aes(x = PPS_combined, y = psd, color = Band, fill = Band)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Pubertal Stage", y = "Power Spectral Density (µV²)") +
  scale_color_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  scale_fill_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(1, 4)) +
  scale_y_continuous(limits = c(0, 0.08))
print(p4)

p10 <- ggplot(your_data_long, aes(x = PBS_Total, y = psd, color = Band, fill = Band)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Positive Behavior Scale (PBS)", y = "Power Spectral Density (µV²)") +
  scale_color_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  scale_fill_manual(values = eeg_colors, labels = c("Beta", "Alpha")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(35, 85)) +
  scale_y_continuous(limits = c(0, 0.08))
print(p10)


# NIH Cognitive Score Plots
cog_colors <- c(
  "NIH_Flanker_Uncorr_Stnd" = "red",
  "NIH_Card_Sort_Uncorr_Stnd" = "darkorange",
  "NIH_List_Sort_Uncorr_Stnd" = "darkgreen",
  "NIH_Processing_Uncorr_Stnd" = "purple"
)

p5 <- ggplot(your_data_cog_long, aes(x = Age, y = score, color = Outcome, fill = Outcome)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Age (years)", y = "NIH Toolbox Cognitive Score") +
  scale_color_manual(values = cog_colors,
                     labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  scale_fill_manual(values = cog_colors,
                    labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(7, 12.5)) +
  scale_y_continuous(limits = c(60, 120))
print(p5)

p6 <- ggplot(your_data_cog_long, aes(x = PANAS_PositiveAffect, y = score, color = Outcome, fill = Outcome)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Positive Affect (PANAS)", y = "NIH Toolbox Cognitive Score") +
  scale_color_manual(values = cog_colors,
                     labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  scale_fill_manual(values = cog_colors,
                    labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(10, 45)) +
  scale_y_continuous(limits = c(60, 120))
print(p6)

p7 <- ggplot(your_data_cog_long, aes(x = PCEs, y = score, color = Outcome, fill = Outcome)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Positive Childhood Experiences (PCEs)", y = "NIH Toolbox Cognitive Score") +
  scale_color_manual(values = cog_colors,
                     labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  scale_fill_manual(values = cog_colors,
                    labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(60, 120))
print(p7)

p8 <- ggplot(your_data_cog_long, aes(x = PPS_combined, y = score, color = Outcome, fill = Outcome)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Pubertal Stage", y = "NIH Toolbox Cognitive Score") +
  scale_color_manual(values = cog_colors,
                     labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  scale_fill_manual(values = cog_colors,
                    labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(1, 4)) +
  scale_y_continuous(limits = c(60, 120))
print(p8)

p9 <- ggplot(your_data_cog_long, aes(x = PBS_Total, y = score, color = Outcome, fill = Outcome)) +
  stat_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Positive Behavior Scale (PBS)", y = "NIH Toolbox Cognitive Score") +
  scale_color_manual(values = cog_colors,
                     labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  scale_fill_manual(values = cog_colors,
                    labels = c("Flanker", "Card Sort", "List Sort", "Processing Speed")) +
  my_theme +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(20, 100)) +
  scale_y_continuous(limits = c(50, 150))
print(p9)

# Close PDF
dev.off()
