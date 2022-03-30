
# CROSSTABS ---------------------------------------------------------------

crosstabs <- function(subjectinput, YAGinput, countinput, qualinput, buttoninput) {
  tables_data$SECTIONNAME[is.na(tables_data$SECTIONNAME) == TRUE] <- "NOT KNOWN"
  tables_data$group_name[is.na(tables_data$group_name) == TRUE] <- "NOT KNOWN"

  orange_pal <- function(x) {
    if (!is.na(x)) {
      rgb(colorRamp(c("#F7FBFF", "#2F75B5"))(x), maxColorValue = 255)
    } else {
      "#e9e9e9" # grey
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


  if (countinput == "ethnicity") {
    crosstabs_data_table <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(ethnicity, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(ethnicity, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`)

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(ethnicity, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(ethnicity, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`) %>%
      ungroup()


    order <- subset(crosstabs_data_table, select = SECTIONNAME)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(ethnicity, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(ethnicity, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`)

    max <- crosstabs_data %>%
      ungroup() %>%
      select(-c(group_name, SECTIONNAME))

    numeric_cols <- names(max)

    numeric_cols_def <- list()
    numeric_cols_def_nested <- list()

    for (column in numeric_cols) {
      script <- paste("
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
        }", sep = "")

      numeric_cols_def_nested[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
      ))

      numeric_cols_def[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
        footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T)
      ))
    }

    nested <- function(index) {
      tables_data_nested <- tables_data %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(ethnicity) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(ethnicity, SECTIONNAME, group_name) %>%
        summarise(n = prop) %>%
        spread(ethnicity, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      if ("All" %in% names(nested_table)) {
        nested_table <- nested_table %>%
          arrange(-All)
      }

      nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(ethnicity, SECTIONNAME, group_name) %>%
        summarise(n = earnings_median) %>%
        spread(ethnicity, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) # %>%
      # mutate_at(c('White', 'Black','Asian', 'Mixed', 'Other', 'Not known'),
      #           funs(as.numeric(.))) %>%
      # mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      # select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`) %>%
      # ungroup()


      nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
      nested_order$SECTIONNAME <- as.character(nested_order$SECTIONNAME)
      nested_order$group_name <- as.character(nested_order$group_name)
      nested_table_earnings2 <- nested_order %>%
        left_join(nested_table_earnings)

      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        nested <- nested_table_earnings2
      }

      for (column in numeric_cols) {
        nested[column] <- if (column %in% colnames(nested)) {
          nested[column]
        } else {
          NA
        }
      }

      nested <- nested %>%
        select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`)

      htmltools::div(
        style = "padding: 16px",
        reactable(nested,
          outlined = TRUE,
          style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
          defaultPageSize = 300
        )
      )
    }
  }

  if (countinput == "current_region") {
    crosstabs_data_table <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(current_region, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c(
          "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands",
          "East of England", "London", "South East", "South West"
        ),
        funs(as.numeric(.))
      ) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(
        SECTIONNAME, group_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(current_region, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c(
          "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands",
          "East of England", "London", "South East", "South West"
        ),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(
        SECTIONNAME, group_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )


    order <- subset(crosstabs_data_table, select = SECTIONNAME)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(current_region, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(
        SECTIONNAME, group_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    max <- crosstabs_data %>%
      ungroup() %>%
      select(-c(group_name, SECTIONNAME))

    numeric_cols <- names(max)

    numeric_cols_def <- list()
    numeric_cols_def_nested <- list()

    for (column in numeric_cols) {
      script <- paste("
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
        }", sep = "")

      numeric_cols_def_nested[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
      ))

      numeric_cols_def[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
        footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T)
      ))
    }

    nested <- function(index) {
      tables_data_nested <- tables_data %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(current_region) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(current_region, SECTIONNAME, group_name) %>%
        summarise(n = prop) %>%
        spread(current_region, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      if ("All" %in% names(nested_table)) {
        nested_table <- nested_table %>%
          arrange(-All)
      }

      nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(current_region, SECTIONNAME, group_name) %>%
        summarise(n = earnings_median) %>%
        spread(current_region, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
      nested_order$SECTIONNAME <- as.character(nested_order$SECTIONNAME)
      nested_order$group_name <- as.character(nested_order$group_name)
      nested_table_earnings2 <- nested_order %>%
        left_join(nested_table_earnings)

      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        nested <- nested_table_earnings2
      }

      for (column in numeric_cols) {
        nested[column] <- if (column %in% colnames(nested)) {
          nested[column]
        } else {
          NA
        }
      }

      nested <- nested %>%
        select(
          SECTIONNAME, group_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
          `East of England`, `London`, `South East`, `South West`
        )

      htmltools::div(
        style = "padding: 16px",
        reactable(nested,
          outlined = TRUE,
          style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
          defaultPageSize = 300
        )
      )
    }
  }

  if (countinput == "FSM") {
    crosstabs_data_table <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(FSM, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(FSM, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)


    order <- subset(crosstabs_data_table, select = SECTIONNAME)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(FSM, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)

    max <- crosstabs_data %>%
      ungroup() %>%
      select(-c(group_name, SECTIONNAME))

    numeric_cols <- names(max)

    numeric_cols_def <- list()
    numeric_cols_def_nested <- list()

    for (column in numeric_cols) {
      script <- paste("
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
        }", sep = "")

      numeric_cols_def_nested[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
      ))

      numeric_cols_def[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
        footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T)
      ))
    }

    nested <- function(index) {
      tables_data_nested <- tables_data %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(FSM) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(FSM, SECTIONNAME, group_name) %>%
        summarise(n = prop) %>%
        spread(FSM, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      if ("All" %in% names(nested_table)) {
        nested_table <- nested_table %>%
          arrange(-All)
      }

      nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(FSM, SECTIONNAME, group_name) %>%
        summarise(n = earnings_median) %>%
        spread(FSM, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
      nested_order$SECTIONNAME <- as.character(nested_order$SECTIONNAME)
      nested_order$group_name <- as.character(nested_order$group_name)
      nested_table_earnings2 <- nested_order %>%
        left_join(nested_table_earnings)

      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        nested <- nested_table_earnings2
      }

      for (column in numeric_cols) {
        nested[column] <- if (column %in% colnames(nested)) {
          nested[column]
        } else {
          NA
        }
      }

      nested <- nested %>%
        select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)

      htmltools::div(
        style = "padding: 16px",
        reactable(nested,
          outlined = TRUE,
          style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
          defaultPageSize = 300
        )
      )
    }
  }

  if (countinput == "sex") {
    crosstabs_data_table <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      group_by(sex, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
    names(crosstabs_data_table) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

    crosstabs_earnings_data <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      group_by(sex, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
    names(crosstabs_earnings_data) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")


    order <- subset(crosstabs_data_table, select = SECTIONNAME)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)
    names(crosstabs_earnings_data2) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      group_by(sex, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
    names(footer_data) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

    max <- crosstabs_data %>%
      ungroup() %>%
      select(-c(group_name, SECTIONNAME))

    numeric_cols <- names(max)

    numeric_cols_def <- list()
    numeric_cols_def_nested <- list()

    for (column in numeric_cols) {
      script <- paste("
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
        }", sep = "")

      numeric_cols_def_nested[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
      ))

      numeric_cols_def[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
        footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T)
      ))
    }

    nested <- function(index) {
      tables_data_nested <- tables_data %>%
        filter(
          subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == qualinput, group_name != "All"
        ) %>%
        group_by(sex) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == qualinput, group_name != "All"
        ) %>%
        group_by(sex, SECTIONNAME, group_name) %>%
        summarise(n = prop) %>%
        spread(sex, n) %>%
        arrange(-`F+M`) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
        mutate_at(
          c("F", "M", "F+M"),
          funs(as.numeric(.))
        ) %>%
        select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
      names(nested_table) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

      nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == qualinput, group_name != "All"
        ) %>%
        group_by(sex, SECTIONNAME, group_name) %>%
        summarise(n = earnings_median) %>%
        spread(sex, n) %>%
        arrange(-`F+M`) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
        mutate_at(
          c("F", "M", "F+M"),
          funs(as.numeric(.))
        ) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
        select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
      names(nested_table_earnings) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

      nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
      nested_table_earnings2 <- nested_order %>%
        left_join(nested_table_earnings)

      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        nested <- nested_table_earnings2
      }

      htmltools::div(
        style = "padding: 16px",
        reactable(nested,
          outlined = TRUE,
          style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
          defaultPageSize = 300
        )
      )
    }
  }

  if (countinput == "prior_attainment") {
    crosstabs_data_table <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")


    order <- subset(crosstabs_data_table, select = SECTIONNAME)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")


    max <- crosstabs_data %>%
      ungroup() %>%
      select(-c(group_name, SECTIONNAME))

    numeric_cols <- names(max)

    numeric_cols_def <- list()
    numeric_cols_def_nested <- list()

    for (column in numeric_cols) {
      script <- paste("
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
        }", sep = "")

      numeric_cols_def_nested[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
      ))

      numeric_cols_def[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
        footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T)
      ))
    }

    nested <- function(index) {
      tables_data_nested <- tables_data %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(prior_attainment) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(prior_attainment, SECTIONNAME, group_name) %>%
        summarise(n = prop) %>%
        spread(prior_attainment, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      if ("All" %in% names(nested_table)) {
        nested_table <- nested_table %>%
          arrange(-All)
      }

      nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", qualification_TR == "First degree", group_name != "All"
        ) %>%
        group_by(prior_attainment, SECTIONNAME, group_name) %>%
        summarise(n = earnings_median) %>%
        spread(prior_attainment, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
      nested_order$SECTIONNAME <- as.character(nested_order$SECTIONNAME)
      nested_order$group_name <- as.character(nested_order$group_name)
      nested_table_earnings2 <- nested_order %>%
        left_join(nested_table_earnings)

      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        nested <- nested_table_earnings2
      }

      for (column in numeric_cols) {
        nested[column] <- if (column %in% colnames(nested)) {
          nested[column]
        } else {
          NA
        }
      }

      nested <- nested %>%
        select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

      htmltools::div(
        style = "padding: 16px",
        reactable(nested,
          outlined = TRUE,
          style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
          defaultPageSize = 300
        )
      )
    }
  }

  if (countinput == "subject_name") {
    crosstabs_data_table <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      group_by(subject_name, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(subject_name, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      select(-All)

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      group_by(subject_name, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(subject_name, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(-All)


    order <- subset(crosstabs_data_table, select = SECTIONNAME)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      group_by(subject_name, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(subject_name, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(-All)

    max <- crosstabs_data %>%
      ungroup() %>%
      select(-c(group_name, SECTIONNAME))

    numeric_cols <- names(max)

    numeric_cols_def <- list()
    numeric_cols_def_nested <- list()

    for (column in numeric_cols) {
      script <- paste("
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
        }", sep = "")

      numeric_cols_def_nested[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
      ))

      numeric_cols_def[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
        footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T)
      ))
    }

    nested <- function(index) {
      tables_data_nested <- tables_data %>%
        filter(
          sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == qualinput, group_name != "All"
        ) %>%
        group_by(subject_name) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == qualinput, group_name != "All"
        ) %>%
        group_by(subject_name, SECTIONNAME, group_name) %>%
        summarise(n = prop) %>%
        spread(subject_name, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), (funs(ifelse(. == 0, NA, .))))

      if ("All" %in% names(nested_table)) {
        nested_table <- nested_table %>%
          arrange(-All)
      }

      nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == qualinput, group_name != "All"
        ) %>%
        group_by(subject_name, SECTIONNAME, group_name) %>%
        summarise(n = earnings_median) %>%
        spread(subject_name, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
      nested_order$SECTIONNAME <- as.character(nested_order$SECTIONNAME)
      nested_order$group_name <- as.character(nested_order$group_name)
      nested_table_earnings2 <- nested_order %>%
        left_join(nested_table_earnings)

      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        nested <- nested_table_earnings2
      }

      for (column in numeric_cols) {
        nested[column] <- if (column %in% colnames(nested)) {
          nested[column]
        } else {
          NA
        }
      }


      htmltools::div(
        style = "padding: 16px",
        reactable(nested,
          outlined = TRUE,
          style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
          defaultPageSize = 300
        )
      )
    }
  }

  if (countinput == "qualification_TR") {
    crosstabs_data_table <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      group_by(qualification_TR, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      group_by(qualification_TR, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)


    order <- subset(crosstabs_data_table, select = SECTIONNAME)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      group_by(qualification_TR, SECTIONNAME, group_name) %>%
      summarise(n = sum(count)) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)

    max <- crosstabs_data %>%
      ungroup() %>%
      select(-c(group_name, SECTIONNAME))

    numeric_cols <- names(max)

    numeric_cols_def <- list()
    numeric_cols_def_nested <- list()

    for (column in numeric_cols) {
      script <- paste("
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
        }", sep = "")

      numeric_cols_def_nested[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
      ))

      numeric_cols_def[column] <- list(colDef(
        na = "x", style = JS(script), format = colformat,
        footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T)
      ))
    }

    nested <- function(index) {
      tables_data_nested <- tables_data %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", prior_attainment == "All", group_name != "All"
        ) %>%
        group_by(qualification_TR) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", prior_attainment == "All", group_name != "All"
        ) %>%
        group_by(qualification_TR, SECTIONNAME, group_name) %>%
        summarise(n = prop) %>%
        spread(qualification_TR, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), (funs(ifelse(. == 0, NA, .))))

      if ("All" %in% names(nested_table)) {
        nested_table <- nested_table %>%
          arrange(-All)
      }

      nested_table_earnings <- tables_data[tables_data$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", prior_attainment == "All", group_name != "All"
        ) %>%
        group_by(qualification_TR, SECTIONNAME, group_name) %>%
        summarise(n = earnings_median) %>%
        spread(qualification_TR, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      nested_order <- subset(nested_table, select = c(SECTIONNAME, group_name))
      nested_order$SECTIONNAME <- as.character(nested_order$SECTIONNAME)
      nested_order$group_name <- as.character(nested_order$group_name)
      nested_table_earnings2 <- nested_order %>%
        left_join(nested_table_earnings)

      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        nested <- nested_table_earnings2
      }

      for (column in numeric_cols) {
        nested[column] <- if (column %in% colnames(nested)) {
          nested[column]
        } else {
          NA
        }
      }

      nested <- nested %>%
        select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)


      htmltools::div(
        style = "padding: 16px",
        reactable(nested,
          outlined = TRUE,
          style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
          defaultPageSize = 300
        )
      )
    }
  }


  coldefs <- list(
    SECTIONNAME = colDef(na = "x", name = "Industry", width = 500, footer = "TOTAL (N)"),
    group_name = colDef(na = "x", name = "3 digit SIC code", width = 300, footer = "TOTAL (N)")
  )
  coldefs_nested <- list(
    SECTIONNAME = colDef(na = "x", name = "Industry", width = 500),
    group_name = colDef(na = "x", name = "3 digit SIC code", width = 300)
  )


  crosstab <- reactable(crosstabs_data,
    details = nested,
    defaultPageSize = 22, showSortable = TRUE, columns = c(coldefs, numeric_cols_def),
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
  )

  return(crosstab)
}

crosstab_title <- function(subjectinput, YAGinput, countinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )



  if (countinput %in% c("FSM", "prior_attainment")) {
    crosstab_title <- paste("<h4>Industry of graduate employment for graduates of", subjecttext, " by", countinput, ",", YAGinput, "years after
                          graduation, young (under 21 at start of course) male and female first degree
                          graduates from English HEIs, APs and FECs, 2018/19 tax year.</h4>")
  }

  if (countinput %in% c("sex")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates of", subjecttext, "by", countinput, ",", YAGinput, "years after
                          graduation, male and female ", qualinput, " graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>")
  }

  if (countinput %in% c("qualification_TR")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates of", subjecttext, "by qualification,", YAGinput, "years after
                          graduation, male and female graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>")
  }

  if (countinput %in% c("current_region", "ethnicity")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates of", subjecttext, "by", countinput, ",", YAGinput, "years after
                          graduation, male and female first degree graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>")
  }

  if (countinput %in% c("subject_name")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates by subject,", YAGinput, "years after
                          graduation, male and female ", qualinput, " graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>")
  }

  return(crosstab_title)
}
