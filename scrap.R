tables_data <- read_csv("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SIC analysis/SIC Dashboard 1819 with detailed/tables_data_3digit_FD_PG_threshold.csv")
tables_earnings_data <- read_csv("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SIC analysis/SIC Dashboard 1819 with detailed/tables_data_3digit_FD_PG_threshold.csv")
names(tables_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "earnings_median","threshold", "qualification_TR", "group_name")
names(tables_earnings_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "earnings_median","threshold", "qualification_TR", "group_name")


tables_data$SECTIONNAME[is.na(tables_data$SECTIONNAME) == TRUE] <- 'NOT KNOWN'
tables_earnings_data$SECTIONNAME[is.na(tables_earnings_data$SECTIONNAME) == TRUE] <- 'NOT KNOWN'

tables_data$group_name[is.na(tables_data$group_name) == TRUE] <- 'NOT KNOWN'
tables_earnings_data$group_name[is.na(tables_earnings_data$group_name) == TRUE] <- 'NOT KNOWN'

orange_pal <- function(x){
  if (!is.na(x)){
    rgb(colorRamp(c("#F7FBFF", "#2F75B5"))(x), maxColorValue = 255)
  } else {
    "#e9e9e9" #grey
  }
}

# function which returns background colour based on cell value (using colour map)
# also takes column name as an input, which allows to get max and min
stylefunc <- function(value, index, name) {
  normalized <- (value - min(crosstabs_data %>%
                               select(-SECTIONNAME), na.rm = T)) /
    (max(crosstabs_data %>%
           select(-SECTIONNAME), na.rm = T) - min(crosstabs_data %>%
                                                    select(-SECTIONNAME), na.rm = T))
  color <- orange_pal(normalized)
  list(background = color)
}

crosstabs_data_table <- tables_data %>%
  filter(subject_name == 'All', YAG == 5, ethnicity == 'All', current_region == 'All', FSM == 'All', 
         prior_attainment == 'All', qualification_TR == 'First degree', threshold == 'All', group_name == 'All') %>%
  group_by(sex, SECTIONNAME, group_name) %>%
  summarise(n = sum(count)) %>%
  spread(sex, n) %>%
  arrange(-`F+M`) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate_all(funs(ifelse( . <= 2, 0, .))) %>%
  ungroup()%>%
  mutate_if(is.numeric, funs(./sum(.))) %>%
  mutate_all(funs(ifelse(. == 0, NA, .))) %>%
  mutate_at(c('F', 'M', 'F+M'),
            funs(as.numeric(.))) %>%
  select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
names(crosstabs_data) <- c('SECTIONNAME', 'group_name', 'Female', 'Male', 'Female & Male')

crosstabs_earnings_data <- tables_data %>%
  filter(subject_name == 'All', YAG == 5, ethnicity == 'All', current_region == 'All', FSM == 'All', 
         prior_attainment == 'All', qualification_TR == 'First degree', threshold == 'All', group_name == 'All') %>%
  group_by(sex, SECTIONNAME, group_name) %>%
  summarise(n = earnings_median) %>%
  spread(sex, n) %>%
  arrange(-`F+M`) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate_all(funs(ifelse(. == 0, NA, .))) %>%
  mutate_at(c('F', 'M', 'F+M'),
            funs(as.numeric(.))) %>%
  mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
  select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
names(crosstabs_earnings_data) <- c('SECTIONNAME', 'group_name', 'Female', 'Male', 'Female & Male')

order <- subset(crosstabs_data_table, select = SECTIONNAME)
crosstabs_earnings_data2 <- order %>%
  left_join(crosstabs_earnings_data)
names(crosstabs_earnings_data2) <- c('SECTIONNAME', 'group_name', 'Female', 'Male', 'Female & Male')

if(buttoninput == 'Proportions'){
  colformat <- colFormat(percent = TRUE, digits = 1)
  crosstabs_data <- crosstabs_data_table
} else if(buttoninput == 'Median earnings'){
  colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
  crosstabs_data <- crosstabs_earnings_data2
}


nested <- function(index){
  
  nested_table <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
    filter(subject_name == 'All', YAG == 5, ethnicity == 'All', current_region == 'All', FSM == 'All', 
           prior_attainment == 'All', qualification_TR == 'First degree', threshold == 'All', group_name != 'All') %>%
    group_by(sex, SECTIONNAME, group_name) %>%
    summarise(n = sum(count)) %>%
    spread(sex, n) %>%
    arrange(-`F+M`) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
    mutate_all(funs(ifelse( . <= 2, 0, .))) %>%
    ungroup()%>%
    mutate_if(is.numeric, funs(./sum(.))) %>%
    mutate_all(funs(ifelse(. == 0, NA, .))) %>%
    mutate_at(c('F', 'M', 'F+M'),
              funs(as.numeric(.))) %>%
    select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
  names(nested_table) <- c('SECTIONNAME', 'group_name', 'Female', 'Male', 'Female & Male')
  
  nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
    filter(subject_name == 'All', YAG == 5, ethnicity == 'All', current_region == 'All', FSM == 'All', 
           prior_attainment == 'All', qualification_TR == 'First degree', threshold == 'All', group_name != 'All') %>%
    group_by(sex, SECTIONNAME, group_name) %>%
    summarise(n = earnings_median) %>%
    spread(sex, n) %>%
    arrange(-`F+M`) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
    mutate_all(funs(ifelse(. == 0, NA, .))) %>%
    mutate_at(c('F', 'M', 'F+M'),
              funs(as.numeric(.))) %>%
    mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
    select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
  names(nested_table_earnings) <- c('SECTIONNAME', 'group_name', 'Female', 'Male', 'Female & Male')
  
  nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
  nested_table_earnings2 <- nested_order %>%
    left_join(nested_table_earnings)
  
  #if(buttoninput == 'Proportions'){
    # nested <- nested_table
  #} else if(buttoninput == 'Median earnings'){
   nested <- nested_table_earnings2
  #}
  
  htmltools::div(style = "padding: 16px",
                 reactable(nested, outlined = TRUE, 
                           style = JS(script), columns = c(coldefs, numeric_cols_def_nested),
                           defaultPageSize = 300))
  
}


footer_data <- tables_data %>%
  filter(subject_name == 'All', YAG == 5, ethnicity == 'All', current_region == 'All', FSM == 'All', 
         prior_attainment == 'All', qualification_TR == 'First degree', threshold == 'All', group_name == 'All') %>%
  group_by(sex, SECTIONNAME, group_name) %>%
  summarise(n = sum(count)) %>%
  spread(sex, n) %>%
  arrange(-`F+M`) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate_all(funs(ifelse( . <= 2, 0, .))) %>%
  select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
names(footer_data) <- c('SECTIONNAME', 'group_name', 'Female', 'Male', 'Female & Male')

  max <- crosstabs_data %>%
    ungroup() %>%
    select(-c(group_name, SECTIONNAME))

numeric_cols <- names(max)

numeric_cols_def <- list()
numeric_cols_def_nested <- list()

for (column in numeric_cols){
  script = paste("
          // source: https://glin.github.io/reactable/articles/examples.html#grouped-cell-rendering-1
          function(rowInfo) {
            // source: https://stackoverflow.com/a/44134328/4856719
            function hslToHex(h, s, l) {
              l /= 100;
              const a = s * Math.min(l, 1 - l) / 100;
              const f = n => {
                const k = (n + h / 30) % 12;
                const color = l - a * Math.max(Math.min(k - 3, 9 - k, 1), -1);
                return Math.round(255 * color).toString(16).padStart(2, '0');
              };
              return `#${f(0)}${f(8)}${f(4)}`;
            }
            var value = rowInfo.row['", column, "']
            var max = ", max(max, na.rm = TRUE), "
            var min = ", min(max, na.rm = TRUE), "
            // pct_value = (value - min) * 100 / (max - min)
            pct_value = (Math.min(value, max) - min) * 100 / (max - min)
            // If value equals 0, set font color grey.
            if (value == 0) {
              var color = '#F7FBFF'
              var bg = '#F7FBFF'
            } else {
              var color = '#000000'
              var bg = hslToHex(209, 59, 100 - pct_value / 2)
            }
            return { color: color, backgroundColor: bg}
        }", sep="")
  
  numeric_cols_def_nested[column] <- list(colDef(
    na = 'x', style = JS(script), format = colformat,
    ))
  
    numeric_cols_def[column] <- list(colDef(
      na = 'x', style = JS(script), format = colformat,
      footer = format(round_any(sum(footer_data[column]),5),big.mark = ",", scientific = FALSE, na.m = T)
    ))
  
}

coldefs <- list(
  SECTIONNAME = colDef(na = 'x', name = 'Industry', width = 500, footer = 'TOTAL (N)'),
  group_name = colDef(na = 'x', name = '3 digit SIC code', width = 300, footer = 'TOTAL (N)')
)

  crosstab <- reactable(crosstabs_data, details = nested,
                        defaultPageSize = 22, showSortable = TRUE, columns = c(coldefs, numeric_cols_def),
                        defaultColDef = colDef(footerStyle = list(fontWeight = 'bold'))
  )

  crosstab
  
  
  c(unique(tables_data$subject_name[which(tables_data$qualification_TR == 'First degree')]))
  