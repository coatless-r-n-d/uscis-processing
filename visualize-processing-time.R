library(ggplot2)
library(dplyr)

processing_times = read.csv(
  file = paste0("data-raw/processing-times-",Sys.Date(),".csv")
)

# Convert to standard unit of measurement
processing_in_months = processing_times %>%
  mutate(
    range_upper =
      case_when(
        range_upper_unit == "Weeks" ~ range_upper * 0.230137,
        TRUE ~ range_upper
      ),
    range_upper_unit =
      case_when(
        range_upper_unit == "Weeks" ~ "Months",
        TRUE ~ range_upper_unit
      ),
    range_lower =
      case_when(
        range_lower_unit == "Weeks" ~ range_lower * 0.230137,
        TRUE ~ range_lower
      ),
    range_lower_unit =
      case_when(
        range_lower_unit == "Weeks" ~ "Months",
        TRUE ~ range_lower_unit
      )
  )


# Create a graph showing form processing overall ----
ggplot(data = processing_in_months) +
  aes(x = form_name, y = range_upper) + 
  geom_boxplot() +
  labs(title = "Upper Range of Time (Months) for Each Form Across Offices", 
       subtitle = "Based on USCIS Processing Times", 
       y = "Months",
       x = "Form") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Create a graph with form processing across office ----
ggplot(data = processing_in_months) +
  aes(x = form_name, y = range_upper, color = office_code) + 
  geom_boxplot() +
  labs(title = "Upper Range of Time (Months) for Each Form by Processing Office", 
       subtitle = "Based on USCIS Processing Times", 
       y = "Months",
       x = "Form",
       color = "Office") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

