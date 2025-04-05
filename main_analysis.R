###############################################################################
# 0. LIBRARIES
###############################################################################
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(httr)
library(jsonlite)
library(ggsci)
library(tidyverse)
library(igraph)
library(wbstats)
library(rvest)
library(tidyr)
library(purrr)
library(pheatmap)
library(writexl)
library(ggbreak)
library(metafor)
library(openxlsx)



###############################################################################
# 1. GROUPING INDICATORS
###############################################################################

# Economic development & education indicators
economy_development_education <- c(
  "Current health expenditure (% of GDP)",
  "GDP per capita (current US$)",
  "Human Development Index",
  "Out-of-pocket expenditure (% of current health expenditure)",
  "Research and development expenditure (% of GDP)",
  "Charges for the use of intellectual property, payments (BoP, current US$",
  "Average years of schooling",
  "Literacy rate, adult total (% of people ages 15 and above)",
  "Literacy rate, youth total (% of people ages 15-24)",
  "Share of population with no formal education"
)

IDs <- LETTERS[1:length(economy_development_education)]

economy_development_education <- data.frame(
  ID = IDs,
  Indicator = economy_development_education,
  Role = rep("i", length(economy_development_education))
)


# General health indicators
general_health <- c(
  "Disability-Adjusted Life Years (DALYs)",
  "Homicide rate",
  "Life expectancy at birth, total (years)",
  "Life expectancy both sexes",
  "Life expectancy of men",
  "Life expectancy of women",
  "Nurses and midwives (per 1,000 people)",
  "Physicians (per 1,000 people)",
  "The Universal Health Coverage (UHC) Service Coverage Index",
  "Child mortality rate",
  "Deaths"
)

IDs <- LETTERS[1:length(general_health)]
general_health <- data.frame(
  ID = IDs,
  Indicator = general_health,
  Role = c(
    "d", "d", "d", "d", "d", "d", "i", "i", "d", "d", "d"
  )
)


# Mental health & well-being indicators
mental_health_well_being <- c(
  "Anxiety disorders (DALYs)",
  "Anxiety disorders (Prevalence)",
  "Beds for mental health in general hospitals (per 100,000)",
  "Beds in community residential facilities (per 100,000)",
  "Beds in mental hospitals (per 100,000)",
  "Bipolar disorder (DALYs)",
  "Bipolar disorder (Prevalence)",
  "Eating disorders (DALYs)",
  "Eating disorders (Prevalence)",
  "Government expenditures on mental health (% of health expenditures)",
  "Mental health day treatment facilities (per 100,000)",
  "Mental health outpatient facilities (per 100,000)",
  "Mental health units in general hospitals (per 100,000)",
  "Mental health units in general hospitals admissions (per 100,000)",
  "Mental hospital admissions (per 100,000)",
  "Nurses in mental health sector (per 100,000)",
  "Outpatient visits (per 100,000)",
  "Psychiatrists in mental health sector (per 100,000)",
  "Psychologists in mental health sector (per 100,000)",
  "Schizophrenia (DALYs)",
  "Schizophrenia (Prevalence)",
  "Social workers in mental health sector (per 100,000)",
  "Self-reported life satisfaction",
  "Share of people who say they are happy"
)

IDs <- LETTERS[1:length(mental_health_well_being)]
mental_health_well_being <- data.frame(
  ID = IDs,
  Indicator = mental_health_well_being,
  Role = c(
    "d", "d", "d", "d", "d", "d", "d", "d", "d",
    "i", "d", "d", "d", "d", "d", "i", "d", "i",
    "i", "d", "d", "d", "d", "d"
  )
)


# Inequality & poverty indicators
inequality_poverty <- c(
  "Share of population living in extreme poverty",
  "Gini index",
  "Income inequality: Atkinson index",
  "Lifespan inequality: Gini coefficient in men",
  "Lifespan inequality: Gini coefficient in women",
  "Multidimensional Poverty Index (MPI)"
)

IDs <- LETTERS[1:length(inequality_poverty)]
inequality_poverty <- data.frame(
  ID = IDs,
  Indicator = inequality_poverty,
  Role = rep("i", length(inequality_poverty))
)


# Governance indicators
governance <- c(
  "Functioning government index",
  "Human rights index",
  "LGBT+ legal equality index",
  "Political corruption index",
  "Private civil liberties index",
  "Rigorous and impartial public administration index",
  "State capacity index",
  "Corruption Perception Index",
  "Percentage of territory effectively controlled by government"
)

IDs <- LETTERS[1:length(governance)]
governance <- data.frame(
  ID = IDs,
  Indicator = governance,
  Role = rep("i", length(governance))
)



###############################################################################
# 2. REGRESSION MODELS
###############################################################################

# Helper function to extract key stats from a linear model
extract_model_info <- function(model) {
  summary_model <- summary(model)
  coef_vals <- summary_model$coefficients
  fstat <- summary_model$fstatistic
  p_value_f <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  
  data.frame(
    beta_1 = coef_vals[2, 1],
    intercept = coef_vals[1, 1],
    p_value = coef_vals[2, 4],
    std_error_beta1 = coef_vals[2, 2],
    residual_std_error = summary_model$sigma,
    t_value = coef_vals[2, 3],
    r_squared = summary_model$r.squared,
    adjusted_r_squared = summary_model$adj.r.squared,
    f_statistic = fstat[1],
    p_value_f = p_value_f,
    DF_residual = summary_model$df[2]
  )
}

# List of dataframes of indicators
df_list <- list(
  economy_development_education = economy_development_education,
  general_health = general_health,
  mental_health_well_being = mental_health_well_being,
  inequality_poverty = inequality_poverty,
  governance = governance
)

# Scientometric variables
scientometric_vars <- c("publications", "citations", "h_index_median")

for (sciento_var in scientometric_vars) {
  
  for (df_name in names(df_list)) {
    indicators_df <- df_list[[df_name]]
    
    # Prepare an empty results dataframe
    results <- data.frame()
    
    # Unique regions from df_region_year (assuming df_region_year exists)
    all_regions <- unique(df_region_year$region)
    
    # Loop across regions
    for (reg in all_regions) {
      
      df_subset <- subset(df_region_year, region == reg)
      
      for (row_i in 1:nrow(indicators_df)) {
        
        indicator_name <- indicators_df$Indicator[row_i]
        role <- indicators_df$Role[row_i]
        indicator_id <- indicators_df$ID[row_i]
        
        # If the indicator is not found in the dataframe subset, skip
        if (!(indicator_name %in% colnames(df_subset))) next
        
        # Determine direction: dependent ("d") or independent ("i")
        if (role == "d") {
          formula <- as.formula(sprintf("`%s` ~ `%s`", indicator_name, sciento_var))
        } else if (role == "i") {
          formula <- as.formula(sprintf("`%s` ~ `%s`", sciento_var, indicator_name))
        } else {
          next
        }
        
        # Initialize a row of NA
        model_info <- data.frame(
          beta_1 = NA, beta_1_standardized = NA, intercept = NA,
          p_value = NA, std_error_beta1 = NA, residual_std_error = NA,
          t_value = NA, r_squared = NA, adjusted_r_squared = NA,
          f_statistic = NA, p_value_f = NA, DF_residual = NA
        )
        
        # Attempt to fit the model if we have at least 3 complete data points
        sufficient_data <- sum(complete.cases(df_subset[, c(indicator_name, sciento_var)])) >= 3
        if (sufficient_data) {
          
          # Raw (non-standardized) model
          model_raw <- lm(formula, data = df_subset)
          model_info <- extract_model_info(model_raw)
          
          # Manual standardization
          df_temp <- df_subset[, c(indicator_name, sciento_var)]
          df_temp <- df_temp[complete.cases(df_temp), ]
          df_temp <- as.data.frame(scale(df_temp))
          colnames(df_temp) <- c("y", "x")
          
          # Fit the standardized model
          formula_std <- y ~ x
          model_std <- lm(formula_std, data = df_temp)
          beta_std <- coef(model_std)[2]
          
          model_info$beta_1_standardized <- beta_std
        } else {
          # Not enough data
          model_info$beta_1_standardized <- NA
        }
        
        # Add region and indicator details
        model_info$region <- reg
        model_info$ID <- indicator_id
        model_info$indicator <- indicator_name
        model_info$indicator_role <- role
        
        # Reorder columns
        model_info <- model_info[, c(
          "region", "ID", "indicator", "indicator_role",
          "beta_1", "beta_1_standardized",
          "intercept", "p_value", "std_error_beta1",
          "residual_std_error", "t_value",
          "r_squared", "adjusted_r_squared",
          "f_statistic", "p_value_f", "DF_residual"
        )]
        
        # Bind to results
        results <- rbind(results, model_info)
      }
    }
    
    # Add significance stars
    results$p_stars <- ifelse(
      results$p_value < 0.001, "***",
      ifelse(
        results$p_value < 0.01, "**",
        ifelse(results$p_value < 0.05, "*", NA)
      )
    )
    
    # Insert 'p_stars' column right after 'p_value'
    p_val_index <- which(names(results) == "p_value")
    stars_col <- results["p_stars"]
    results <- results[, !(names(results) %in% "p_stars")]
    results <- cbind(
      results[, 1:p_val_index, drop = FALSE],
      stars_col,
      results[, (p_val_index + 1):ncol(results), drop = FALSE]
    )
    
    # Assign the final result with a unique name
    assign(paste(sciento_var, df_name, sep = "."), results)
  }
}



###############################################################################
# 3. HEATMAP OF STANDARDIZED COEFFICIENTS
###############################################################################

plot_heatmap_beta <- function(df_result, main_title = NA, number_fontsize = 9) {
  library(pheatmap)
  library(dplyr)
  library(tidyr)
  
  # Mark with "*" if the indicator is an independent variable
  df <- df_result %>%
    mutate(ID_label = ifelse(indicator_role == "i", paste0(ID, "*"), ID))
  
  # Create matrix of standardized coefficients
  heat_data <- df %>%
    select(ID_label, region, beta_1_standardized) %>%
    pivot_wider(names_from = region, values_from = beta_1_standardized) %>%
    column_to_rownames("ID_label")
  
  # Create matrix of significance (stars)
  stars_matrix <- df %>%
    select(ID_label, region, p_stars) %>%
    pivot_wider(names_from = region, values_from = p_stars) %>%
    column_to_rownames("ID_label")
  
  # Remove empty rows
  rows_to_keep <- rowSums(!is.na(heat_data)) > 0
  heat_data <- heat_data[rows_to_keep, ]
  stars_matrix <- stars_matrix[rows_to_keep, ]
  
  # Replace NA with 0 in coefficients
  heat_data[is.na(heat_data)] <- 0
  
  # Sort by descending average effect
  heat_data <- heat_data[order(-rowMeans(heat_data, na.rm = TRUE)), ]
  stars_matrix <- stars_matrix[rownames(heat_data), ]
  
  # Clean up NA in stars matrix
  stars_matrix[is.na(stars_matrix)] <- ""
  
  # Plot heatmap
  pheatmap(
    heat_data,
    cluster_rows = FALSE,
    cluster_cols = TRUE,
    scale = "none",
    display_numbers = stars_matrix,
    number_color = "black",
    fontsize_number = number_fontsize,
    color = colorRampPalette(c("blue", "white", "red"))(100),
    border_color = "0",
    breaks = seq(-1, 1, length.out = 100),
    main = main_title
  )
}


###############################################################################
# 4. PLOTS FOR MENTAL HEALTH DISORDERS (PREVALENCE & DALYs)
###############################################################################

# Example usage: 
# "publications.mental.health.well.being" must be in the environment.
# We'll illustrate how to rename for clarity or keep as-is.

# Subset for prevalence-based indicators
mh_prevalences <- publications.mental.health.well.being %>%
  filter(indicator %in% c(
    "Anxiety disorders (Prevalence)",
    "Bipolar disorder (Prevalence)",
    "Eating disorders (Prevalence)",
    "Schizophrenia (Prevalence)"
  ))

# Order factor levels
mh_prevalences$indicator <- factor(
  mh_prevalences$indicator,
  levels = c(
    "Anxiety disorders (Prevalence)",
    "Bipolar disorder (Prevalence)",
    "Eating disorders (Prevalence)",
    "Schizophrenia (Prevalence)"
  )
)

mh_prevalences$region <- factor(
  mh_prevalences$region,
  levels = c("Americas", "Europe", "Western Pacific",
             "South-East Asia", "Eastern Mediterranean", "Africa")
)

# Define region colors
region_colors <- c(
  "Americas" = "#1d3557",
  "Europe"   = "#457b9d",
  "Western Pacific" = "#a8dadc",
  "South-East Asia" = "#ff99ac",
  "Eastern Mediterranean" = "#ff5c8a",
  "Africa" = "#ff0a54"
)

# Mark asterisks for significance
mh_prevalences$asterisk <- ifelse(!is.na(mh_prevalences$p_stars), "*", NA)

# Remove " (Prevalence)" in factor display
mh_prevalences$indicator <- gsub(" \\(Prevalence\\)", "", mh_prevalences$indicator)

# Prevalences plot
ggplot(mh_prevalences, aes(x = indicator, y = beta_1_standardized, fill = region)) +
  geom_col(position = position_dodge2(reverse = TRUE, width = 0.8), width = 0.7) +
  geom_text(
    aes(
      label = asterisk,
      y = beta_1_standardized + ifelse(beta_1_standardized > 0, 0.07, -0.07)
    ),
    position = position_dodge2(width = 0.7, reverse = TRUE),
    size = 2.7,
    na.rm = TRUE
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
  geom_hline(yintercept = -1, color = "grey50", linetype = "dashed", linewidth = 0.2) +
  geom_hline(yintercept = 1, color = "grey50", linetype = "dashed", linewidth = 0.2) +
  coord_flip() +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(x = NULL, y = NULL, fill = "Region") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    plot.title = element_blank()
  )


# Subset for DALYs
mh_dalys <- publications.mental.health.well.being %>%
  filter(indicator %in% c(
    "Anxiety disorders (DALYs)",
    "Bipolar disorder (DALYs)",
    "Eating disorders (DALYs)",
    "Schizophrenia (DALYs)"
  ))

mh_dalys$indicator <- factor(
  mh_dalys$indicator,
  levels = c(
    "Anxiety disorders (DALYs)",
    "Bipolar disorder (DALYs)",
    "Eating disorders (DALYs)",
    "Schizophrenia (DALYs)"
  )
)
mh_dalys$region <- factor(
  mh_dalys$region,
  levels = c("Americas", "Europe", "Western Pacific",
             "South-East Asia", "Eastern Mediterranean", "Africa")
)

mh_dalys$asterisk <- ifelse(!is.na(mh_dalys$p_stars), "*", NA)
mh_dalys$indicator <- gsub(" \\(DALYs\\)", "", mh_dalys$indicator)

# DALYs plot
ggplot(mh_dalys, aes(x = indicator, y = beta_1_standardized, fill = region)) +
  geom_col(position = position_dodge2(reverse = TRUE, width = 0.8), width = 0.7) +
  geom_text(
    aes(
      label = asterisk,
      y = beta_1_standardized + ifelse(beta_1_standardized > 0, 0.07, -0.07)
    ),
    position = position_dodge2(width = 0.7, reverse = TRUE),
    size = 2.7,
    na.rm = TRUE
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
  geom_hline(yintercept = -1, color = "grey50", linetype = "dashed", linewidth = 0.2) +
  geom_hline(yintercept = 1, color = "grey50", linetype = "dashed", linewidth = 0.2) +
  coord_flip() +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(x = NULL, y = NULL, fill = "Region") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    plot.title = element_blank()
  )


###############################################################################
# 5. EXAMPLE PLOTS OF SPECIFIC INDICATORS
###############################################################################
# The following code provides examples of how to plot specific indicators 
# (e.g., from the 'publications.general.health' or 'publications.economy.development.education' dataframes).

###############################
# Example: Child mortality rate
###############################
publications.general.health %>%
  filter(indicator == "Child mortality rate") %>%
  mutate(
    region = factor(region, levels = c(
      "Americas", "Europe", "Western Pacific",
      "South-East Asia", "Eastern Mediterranean", "Africa"
    )),
    simplified_stars = ifelse(is.na(p_stars), "", "*"),
    label = paste0(round(beta_1, 2), simplified_stars),
    position_label = case_when(
      region == "Africa" ~ beta_1 + 0.03,
      TRUE ~ beta_1 - 0.03
    )
  ) %>%
  ggplot(aes(x = region, y = beta_1, fill = region)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(y = position_label, label = label), size = 2.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
  scale_y_break(c(-0.4, -0.9)) +
  scale_y_continuous(breaks = c(0, -0.1, -0.2, -0.3, -0.4, -0.9)) +
  scale_fill_manual(values = c(  
    "Americas" =               "#1d3557",
    "Europe" =                 "#457b9d",
    "Western Pacific" =        "#a8dadc",
    "South-East Asia" =        "#ff99ac",
    "Eastern Mediterranean" =  "#ff5c8a",
    "Africa" =                 "#ff0a54"
  )) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70", linetype = "dashed", linewidth = 0.2
    ),
    axis.text.x = element_blank(),
    axis.text.y.right = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
    legend.position = "none"
  )


###############################
# Example: Physicians (per 1,000 people)
###############################
publications.general.health %>%
  filter(indicator == "Physicians (per 1,000 people)") %>%
  mutate(
    region = factor(region, levels = c(
      "Americas", "Europe", "Western Pacific",
      "South-East Asia", "Eastern Mediterranean", "Africa"
    )),
    simplified_stars = ifelse(is.na(p_stars), "", "*"),
    label = paste0(round(beta_1, 2), simplified_stars),
    position_label = case_when(
      beta_1 > 0 ~ beta_1 + 80,
      TRUE ~ beta_1 - 50
    )
  ) %>%
  ggplot(aes(x = region, y = beta_1, fill = region)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(y = position_label, label = label), size = 2.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
  scale_y_break(c(1000, 1500)) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 3000)) +
  scale_fill_manual(values = c(  
    "Americas" =               "#1d3557",
    "Europe" =                 "#457b9d",
    "Western Pacific" =        "#a8dadc",
    "South-East Asia" =        "#ff99ac",
    "Eastern Mediterranean" =  "#ff5c8a",
    "Africa" =                 "#ff0a54"
  )) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70", linetype = "dashed", linewidth = 0.2
    ),
    axis.text.x = element_blank(),
    axis.text.y.right = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 50, r = 10, b = 30, l = 10),
    legend.position = "none"
  )


###############################
# Example: Research and development expenditure (% of GDP)
###############################
publications.economy.development.education %>%
  filter(indicator == "Research and development expenditure (% of GDP)") %>%
  mutate(
    region = factor(region, levels = c(
      "Americas", "Europe", "Western Pacific",
      "South-East Asia", "Eastern Mediterranean", "Africa"
    )),
    simplified_stars = ifelse(is.na(p_stars), "", "*"),
    label = paste0(round(beta_1, 2), simplified_stars),
    position_label = case_when(
      beta_1 > 0 ~ beta_1 + 80,
      TRUE ~ beta_1 - 80
    )
  ) %>%
  ggplot(aes(x = region, y = beta_1, fill = region)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(y = position_label, label = label), size = 2.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
  scale_y_break(c(1300, 2000)) +
  scale_y_continuous(breaks = c(0, 500, 1000, 2000, 2500)) +
  scale_fill_manual(values = c(  
    "Americas" =               "#1d3557",
    "Europe" =                 "#457b9d",
    "Western Pacific" =        "#a8dadc",
    "South-East Asia" =        "#ff99ac",
    "Eastern Mediterranean" =  "#ff5c8a",
    "Africa" =                 "#ff0a54"
  )) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70", linetype = "dashed", linewidth = 0.2
    ),
    axis.text.x = element_blank(),
    axis.text.y.right = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
    legend.position = "none"
  )


###############################
# Example: Share of population living in extreme poverty
###############################
publications.inequality.poverty %>%
  filter(indicator == "Share of population living in extreme poverty") %>%
  mutate(
    region = factor(region, levels = c(
      "Americas", "Europe", "Western Pacific",
      "South-East Asia", "Eastern Mediterranean", "Africa"
    )),
    simplified_stars = ifelse(is.na(p_stars), "", "*"),
    label = paste0(round(beta_1, 2), simplified_stars),
    position_label = case_when(
      beta_1 > 0 ~ beta_1 + 12,
      TRUE ~ beta_1 - 10
    )
  ) %>%
  ggplot(aes(x = region, y = beta_1, fill = region)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(y = position_label, label = label), size = 2.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
  scale_y_break(c(15, 1850)) +
  scale_y_break(c(1900, 2370)) +
  scale_y_continuous(breaks = c(-15, 0, 15, 1850, 1900, 2370)) +
  scale_fill_manual(values = c(  
    "Americas" =               "#1d3557",
    "Europe" =                 "#457b9d",
    "Western Pacific" =        "#a8dadc",
    "South-East Asia" =        "#ff99ac",
    "Eastern Mediterranean" =  "#ff5c8a",
    "Africa" =                 "#ff0a54"
  )) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70", linetype = "dashed", linewidth = 0.2
    ),
    axis.text.x = element_blank(),
    axis.text.y.right = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
    legend.position = "none"
  )

print()  # Just to ensure the above plot is rendered


###############################################################################
# 6. META-ANALYSIS
###############################################################################
# Example meta-analysis step for each set of indicators.

# Original dataframes (with region-based regressions).
original_dataframes <- list(
  publications.economy.development.education,
  publications.general.health,
  publications.governance,
  publications.inequality.poverty,
  publications.mental.health.well.being
)

# Corresponding base names
base_names <- c(
  "publications.economy.development.education",
  "publications.general.health",
  "publications.governance",
  "publications.inequality.poverty",
  "publications.mental.health.well.being"
)

# Function to assign significance stars
assign_significance_stars <- function(p_val) {
  if (is.na(p_val)) return(NA)
  else if (p_val < 0.001) return("***")
  else if (p_val < 0.01) return("**")
  else if (p_val < 0.05) return("*")
  else return(NA)
}

# Function to perform meta-analysis per indicator in a dataframe
perform_meta_analysis <- function(df) {
  lapply(split(df, df$indicator), function(sub_df) {
    if (nrow(sub_df) >= 2) {
      # Attempt random-effects meta-analysis with 'metafor'
      result <- tryCatch({
        meta_mod <- rma(
          yi = sub_df$beta_1,
          sei = sub_df$std_error_beta1,
          method = "REML"
        )
        data.frame(
          indicator = sub_df$indicator[1],
          k = meta_mod$k,
          beta_1_meta = meta_mod$b[1],
          ci_lower = meta_mod$ci.lb,
          ci_upper = meta_mod$ci.ub,
          p_value = meta_mod$pval,
          p_stars = assign_significance_stars(meta_mod$pval),
          tau2 = meta_mod$tau2,
          I2 = meta_mod$I2,
          Q = meta_mod$QE,
          Q_p_value = meta_mod$QEp
        )
      }, error = function(e) {
        data.frame(
          indicator = sub_df$indicator[1],
          k = nrow(sub_df),
          beta_1_meta = NA,
          ci_lower = NA,
          ci_upper = NA,
          p_value = NA,
          p_stars = NA,
          tau2 = NA,
          I2 = NA,
          Q = NA,
          Q_p_value = NA
        )
      })
      return(result)
    } else {
      # Not enough data for meta-analysis
      return(data.frame(
        indicator = sub_df$indicator[1],
        k = nrow(sub_df),
        beta_1_meta = NA,
        ci_lower = NA,
        ci_upper = NA,
        p_value = NA,
        p_stars = NA,
        tau2 = NA,
        I2 = NA,
        Q = NA,
        Q_p_value = NA
      ))
    }
  }) %>%
    do.call(rbind, .)
}

# Apply meta-analysis to each of the original dataframes
for (i in seq_along(original_dataframes)) {
  meta_df <- perform_meta_analysis(original_dataframes[[i]])
  assign(paste0(base_names[i], ".meta.analysis"), meta_df)
}

# Combine significant results from each meta-analysis
meta_names <- c(
  "publications.economy.development.education.meta.analysis",
  "publications.general.health.meta.analysis",
  "publications.governance.meta.analysis",
  "publications.inequality.poverty.meta.analysis",
  "publications.mental.health.well.being.meta.analysis"
)

significant_meta <- lapply(meta_names, function(m_name) {
  df_meta <- get(m_name)
  filtered <- subset(df_meta, !is.na(p_value) & p_value < 0.05)
  filtered$source <- m_name
  filtered
})

publications_meta_analysis_significant <- do.call(rbind, significant_meta)
