library(ggrepel)

# Function to create formatted scatter plots with regression lines
create_model_plot <- function(data, x_var, y_var, model, title, x_label, y_label, 
                              nudge_x = 0, nudge_y = 2, slope_digits = 2) {
  
  # Calculate statistics from model
  model_summary <- summary(model)
  r_squared <- model_summary$r.squared
  p_value <- model_summary$coefficients[2, 4]
  slope <- model_summary$coefficients[2, 1]
  
  # Create the plot
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], label = State)) +
    geom_point(size = 5, color = "#0047AB", alpha = 0.8) +
    geom_text_repel(size = 5, color = "#ffffff", 
                    nudge_x = nudge_x, nudge_y = nudge_y,
                    segment.color = "#ffffff", segment.alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, color = "#f9ca24", 
                fill = "#f9ca24", alpha = 0.2, linewidth = 1.5) +
    labs(
      title = title,
      subtitle = paste0(
        "R² = ", round(r_squared, 3), 
        " | Slope = ", round(slope, slope_digits),
        " | p-value = ", format.pval(p_value, digits = 3)
      ),
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#0a0e27", color = NA),
      panel.background = element_rect(fill = "#1e2742", color = NA),
      text = element_text(color = "#ffffff", size = 20),
      plot.title = element_text(size = 16, face = "bold", color = "#ffffff"),
      plot.subtitle = element_text(size = 14, color = "#f9ca24"),
      axis.text = element_text(color = "#ffffff"),
      axis.title = element_text(size = 13, face = "bold", color = "#ffffff"),
      panel.grid.major = element_line(color = "#2c3e50", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#2c3e50", linewidth = 0.25)
    )
}

