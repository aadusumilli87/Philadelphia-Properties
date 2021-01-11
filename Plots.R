# Plots
library(ggplot2)
# Sq Footage vs Market Values
total.area <- ggplot(properties_SF.Trim, aes(log(total_livable_area), Log.MktVal)) +
  geom_point(alpha = 0.25) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(x = 'Total Livable Area', y = 'Natural Log of Market Value',
       title = 'Home Size and Property Values in Philadelphia',
       caption = 'Data from Philadelphia Office of Property Assesments') +
  theme_minimal()
total.area

# Map Theme - Maybe Fix Later
windowsFonts(Calibri = 'Calibri')

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      #panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_rect(fill = NA, color = NA),
      ...
    )
}

# Market Value by Police District -----------------------------------------------------------
properties.PD.Avg <- properties_SF.Trim[, .(Mean = mean(market_value)), by = Police.District]
# Rename 
setnames(properties.PD.Avg, old = 'Police.District', new = 'DISTRICT_')
properties.PD.Avg <- properties.PD.Avg %>% 
  mutate(DISTRICT_ = as.integer(as.character(properties.PD.Avg$DISTRICT_)))
# Merge
PPD.Districts <- PPD.Districts %>% 
  left_join(y = properties.PD.Avg, by = 'DISTRICT_')
# Plot
District.MktVal <- ggplot(PPD.Districts) +
  geom_sf(data = PPD.Districts, aes(fill = Mean))
  theme_map() +
  viridis::scale_fill_viridis(name = 'Mean Market Value', 
                              guide = guide_colorbar(
                                direction = 'horizontal',
                                barwidth = unit(85, units = 'mm'),
                                title.position = 'top',
                                title.hjust = 0.5
                              ), labels = scales::comma, option = 'magma', direction = 1) +
  
  theme(legend.position = 'bottom') +
  labs(title = 'Property Values in Philadelphia', 
       subtitle = 'Average Market Value by Police District, 2019')
  
ggsave('Econometrics Project\\Market.Val.By.District.png')

  
# Market Value by Elementary School Catchment ------------------------------------------------
properties.ES.Avg <- properties_SF.Trim[, .(Mean = mean(market_value)), by = ES_ID]
# Merge
ES.Catchment.Area.Plot <- ES.Catchment.Area %>% 
  left_join(y = properties.ES.Avg, by = 'ES_ID')
# Plot
ES.MktVal <- ggplot(ES.Catchment.Area.Plot) +
  geom_sf(aes(fill = Mean)) +
  coord_sf(expand = TRUE) +
  ggthemes::theme_map() +
  viridis::scale_fill_viridis(name = 'Mean Market Value', 
                              guide = guide_colorbar(
                                direction = 'horizontal',
                                barwidth = unit(85, units = 'mm'),
                                title.position = 'top',
                                title.hjust = 0.5
                              ), labels = scales::comma, option = 'magma', direction = 1) +
  theme(legend.position = 'bottom') +
  labs(title = 'Property Values in Philadelphia',
       subtitle = 'Average Market Value by Elementary School Catchment Area, 2019')
ES.MktVal

ggsave('Econometrics Project\\MktVal.png')

# School Scores ----------------------------------------------------------------------------
properties.School.Score <- properties_SF.Trim[, .(Score = mean(Score)), by = ES_ID]
# Merge
ES.Catchment.Area.Score <- ES.Catchment.Area %>% 
  left_join(y = properties.School.Score, by = 'ES_ID')
# Plot
ES.Score <- ggplot(ES.Catchment.Area.Score) +
  geom_sf(aes(fill = Score)) +
  ggthemes::theme_map() +
  viridis::scale_fill_viridis(
    name = 'School Performance Score',
    guide = guide_colorbar(
      direction = 'horizontal',
      barwidth = unit(85, units = 'mm'),
      title.position = 'top',
      title.hjust = 0.5),
    option = 'magma', direction = 1) +
  theme(legend.position = 'bottom') +
  labs(title = 'Performance Score of Philadelphia Elementary Schools',
       subtitle = 'Score by Elementary School Catchment Area, 2017-2018')
ES.Score


# LM Residuals --------------------------------------------------------------
# Map
LM.resid <- properties.Train %>% 
  group_by(ES_Short) %>% 
  summarise(`Mean Residual` = mean(LM.Residuals))
# Merge
ES.LM.Residuals <- ES.Catchment.Area %>% 
  left_join(y = LM.resid, by = 'ES_Short')

LM.Residuals.Plot <- ggplot(ES.LM.Residuals) +
  geom_sf(aes(fill = `Mean Residual`)) +
  ggthemes::theme_map() +
  viridis::scale_fill_viridis(
    name = 'Linear Model Residuals',
    guide = guide_colorbar(
      direction = 'horizontal',
      barwidth = unit(85, units = 'mm'),
      title.position = 'top',
      title.hjust = 0.5
    ),
    option = 'magma', direction = -1) +
  theme(legend.position = 'bottom') +
  labs(title = 'Geographic Dispersion of Prediction Errors',
       subtitle = 'Average Residual by Elementary School Catchment')
LM.Residuals.Plot

ggsave('Econometrics Project\\LMPlot.png')

# Hist of LM Residuals
ggplot(properties.Train) +
  geom_histogram(aes(LM.Residuals), binwidth = 2 * IQR(properties.Train$LM.Residuals, na.rm = TRUE) / length(properties.Train$LM.Residuals)^(1/3)) +
  coord_cartesian(xlim = c(-100000, 100000)) +
  labs(title = 'Distribution of Regression Residuals', x = 'Residuals', y = 'Count') +
  theme_minimal()


# RF Residuals ---------------------------------------------------------------------
# Map
RF.Resid <- properties.Train %>% 
  group_by(ES_Short) %>% 
  summarise(`Mean Residual` = mean(RF.Residuals))
# Merge
ES.RF.Residuals <- ES.Catchment.Area %>% 
  left_join(y = RF.Resid, by = 'ES_Short')
# Plot
RF.Residuals.Plot <- ggplot(ES.RF.Residuals) +
  geom_sf(aes(fill = `Mean Residual`)) +
  ggthemes::theme_map() +
  viridis::scale_fill_viridis(
    name = 'Random Forest Residuals',
    guide = guide_colorbar(
      direction = 'horizontal',
      barwidth = unit(85, units = 'mm'),
      title.position = 'top',
      title.hjust = 0.5
    ),
    option = 'cividis', direction = -1) +
  theme(legend.position = 'bottom') +
  labs(title = 'Geographic Dispersion of Prediction Errors',
       subtitle = 'Average Residual by Elementary School Catchment')
ggsave('Econometrics Project\\RFPlot.png')

# Presentation Plots ---------------------------------------------------------------------------
# Distribution of Market Values
Mkt.Val.Dist <- ggplot(properties_SF.Trim) +
  geom_histogram(aes(market_value), binwidth = 2 * IQR(properties_SF.Trim$market_value, na.rm = TRUE) / length(properties_SF.Trim$market_value)^(1/3)) +
  coord_cartesian(xlim = c(0, 1000000)) +
  scale_x_continuous(breaks = seq(0, 1000000, by = 100000), labels = scales::comma) +
  labs(x = 'Property Value', y = 'Count', title = 'Distribution of Home Values in Philadelphia', subtitle = 'Single Family Homes Only',
       caption = 'Data from the Philadelphia Office of Property Assessment') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
ggsave('Econometrics Project\\MktValDist.jpg')

# Logged Distribution of Market Values
Log.Dist <- ggplot(properties_SF.Trim) +
  geom_histogram(aes(Log.MktVal), binwidth = 2 * IQR(properties_SF.Trim$Log.MktVal, na.rm = TRUE) / length(properties_SF.Trim$Log.MktVal)^(1/3)) +
  coord_cartesian(xlim = c(6, 18)) +
  labs(x = 'Natural Log of Property Values', y = 'Count', 
       title = 'Log Transformed Distribution of Philadelphia Property Values',
       subtitle = 'Single Family Homes Only',
       caption = 'Data from the Philadelphia Office of Property Assessment') +
  theme_minimal()
ggsave('Econometrics Project\\LogMktValDist.jpg')

# QQ Plots
Mkt.Val.QQplot <- qqnorm(properties_SF.Trim$market_value, main = 'QQ Plot of Philadelphia Property Values')
Log.MktVal.QQplot <- qqnorm(properties_SF.Trim$Log.MktVal, main = 'QQ Plot of Log Tranformed Property Values')



# Residual Plots
Resid.Plot <- ggplot(data = properties.Test) +
  geom_point(aes(x = Scaled.LM.Predictions, y = LM.Resid), alpha = 0.2) +
  coord_cartesian(xlim = c(0, 1000000), ylim = c(-500000, 500000)) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Fitted Market Values', y = 'Linear Model Residuals', title = 'Residuals vs Fitted Values') +
  theme_minimal()
ggsave('Econometrics Project\\ResidualsPlot.jpg')

# Variable Importance
LM.Imp <- varImp(Linear.Model)
plot(LM.Imp, top = '15', main = 'Linear Model Variable Importance', ylab = 'Variable')
