plot_data <- data %>%
  filter(YAG == 1, SECTIONNAME == 'Construction', qualification_TR == 'First degree', subject_name == 'All')

ggplot(plot_data, aes(x = region, y = living_in_region)) +
  geom_bar(stat = 'identity')


