col_formats <- function(data, footer_data, colformat) {
  max <- data %>%
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
  return(list(numeric_cols = numeric_cols, numeric_cols_def = numeric_cols_def, numeric_cols_def_nested = numeric_cols_def_nested, script = script))
}

# CROSSTABS ---------------------------------------------------------------

subjbyind_grouped_summary <- function(subjectinput, YAGinput, countinput, qualinput) {
  # This function pulls all the necessary aggregation for the crosstab into a
  # single place. The output from this is then called in later by the
  # pre-existing summary text and table function that now just tidy up the data
  # a bit and output the bits that get rendered.
  # This bit should collapse the number of group_by/summarise calls per input
  # variation from 2 to 1 compared to the original code, so hoping for a decent
  # speed up from this.
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )

  if (countinput == "sex") {
    tables_data_grouped <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      select(sex, SECTIONNAME, count, earnings_median)
  } else if (countinput == "FSM") {
    tables_data_grouped <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      select(FSM, SECTIONNAME, count, earnings_median)
  } else if (countinput == "ethnicity") {
    tables_data_grouped <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      select(ethnicity, SECTIONNAME, count, earnings_median)
  } else if (countinput == "current_region") {
    tables_data_grouped <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      select(current_region, SECTIONNAME, count, earnings_median)
  } else if (countinput == "prior_attainment") {
    tables_data_grouped <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", group_name == "All"
      ) %>%
      select(prior_attainment, SECTIONNAME, count, earnings_median)
  } else if (countinput == "subject_name") {
    tables_data_grouped <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name == "All"
      ) %>%
      select(subject_name, SECTIONNAME, count, earnings_median)
  }
  return(tables_data_grouped %>% as.data.frame())
}

crosstab_text <- function(tables_data_grouped, subjectinput, YAGinput, countinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )

  if (countinput == "sex") {
    crosstabs_data <- tables_data_grouped %>%
      select(-earnings_median, n = count) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, `F`, `M`, `F+M`) %>%
      mutate(
        diff = `M` - `F`,
        abs = abs(`M` - `F`)
      )
    names(crosstabs_data) <- c("SECTIONNAME", "Female", "Male", "Female & Male", "diff", "abs")

    crosstabs_earnings_data <- tables_data_grouped %>%
      select(-count, n = earnings_median) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, `F`, `M`, `F+M`) %>%
      mutate(
        diff = `M` - `F`,
        abs = abs(`M` - `F`)
      )
    names(crosstabs_earnings_data) <- c("SECTIONNAME", "Female", "Male", "Female & Male", "diff", "abs")

    top_industry <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME)) %>%
      mutate_if(is.numeric, funs(format(., big.mark = ",", scientific = FALSE)))

    top_industry_female <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female)) %>%
      mutate_if(is.numeric, funs(format(., big.mark = ",", scientific = FALSE)))

    top_industry_male <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Male)) %>%
      mutate_if(is.numeric, funs(format(., big.mark = ",", scientific = FALSE)))

    ifelse(first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female) == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Male),
      sectiontext <- paste("graduates is the same for both female and male graduates (<b>", first(crosstabs_data$SECTIONNAME), "</b>). The median
                                earnings for females in this industry  were <b>£", top_industry$Female, "</b> and for males were <b>£",
        top_industry$Male, "</b>.",
        sep = ""
      ),
      sectiontext <- paste("female graduates is <b>", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female), "</b>, and the
                                median earnings of females in this industry were <b>£", top_industry_female$Female, "</b>. The industry
                                with the highest proportion of male graduates was <b>",
        first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Male), "</b> and the median earnings of males
                                in this industry were <b>£", top_industry_male$Male, "</b>.",
        sep = ""
      )
    )

    ifelse(first(crosstabs_data$diff, order_by = -crosstabs_data$abs) > 0,
      sextext <- paste(
        "the proportion of male graduates is <b>", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        " percentage points higher</b> than the proportion of female graduates."
      ),
      sextext <- paste(
        "the proportion of female graduates is <b>", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        " percentage points higher</b> than the proportion of male graduates."
      )
    )

    ifelse(first(crosstabs_earnings_data$diff, order_by = -crosstabs_earnings_data$abs) > 0,
      sextextearnings <- paste("the median earnings of male graduates were <b>£",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher</b> than the medain earnings of female graduates.",
        sep = ""
      ),
      sextextearnings <- paste("the median earnings of female graduates were <b>£",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher</b> than the median earnings of male graduates.",
        sep = ""
      )
    )

    ifelse(abs(round(sum(crosstabs_data$Female[1:2]) * 100, digits = 1) - round(sum(crosstabs_data$Male[1:2] * 100), digits = 1)) > 5,
      sextext2 <- paste("<b>", round(sum(crosstabs_data$Female[1:2]) * 100, digits = 1), "%</b> of female graduates are concentrated in the top 2
                           industries (either <b>", first(crosstabs_data$SECTIONNAME), "</b> or <b>", crosstabs_data$SECTIONNAME[2], "</b>),
                           whereas for male graduates this is ", round(sum(crosstabs_data$Male[1:2] * 100), digits = 1), "%.", sep = ""),
      sextext2 <- paste("")
    )

    ifelse(first(crosstabs_earnings_data$Male, order_by = -crosstabs_earnings_data$Male) > first(crosstabs_earnings_data$Female, order_by = -crosstabs_earnings_data$Female),
      sextextearnings2 <- paste("The group with the highest earnings was male graduates in the <b>",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$Male), "</b> industry
           (median earnings of <b>£", format(first(crosstabs_earnings_data$Male, order_by = -crosstabs_earnings_data$Male), big.mark = ",", scientific = FALSE), "</b>).",
        sep = ""
      ),
      sextextearnings2 <- paste("The group with the highest earnings was female graduates in the <b>",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$Female),
        " </b> industry (median earnings of <b>£", format(first(crosstabs_earnings_data$Female, order_by = -crosstabs_earnings_data$Female), big.mark = ",", scientific = FALSE),
        "</b>).",
        sep = ""
      )
    )

    crosstab_text <- paste("For ", qualinput, " graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ",
      "the industry with the highest proportion of ", sectiontext, br(), br(),
      "The biggest difference in proportions is seen in <b>", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$abs),
      "</b> where ", sextext, br(), br(),
      "The biggest difference in median earnings is seen in <b>", first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$abs),
      "</b> where ", sextextearnings, br(), br(),
      sextextearnings2, br(), br(),
      sextext2,
      sep = ""
    )
  }

  if (countinput == "FSM") {
    crosstabs_data <- tables_data_grouped %>%
      select(-earnings_median, n = count) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, `non-FSM`, FSM, `Not known`) %>%
      mutate(
        diff = `non-FSM` - FSM,
        abs = abs(`non-FSM` - FSM)
      )

    crosstabs_earnings_data <- tables_data_grouped %>%
      select(-count, n = earnings_median) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, `non-FSM`, FSM, `Not known`) %>%
      mutate(
        diff = `non-FSM` - FSM,
        abs = abs(`non-FSM` - FSM)
      )

    top_industry <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME)) %>%
      mutate_if(is.numeric, funs(format(., big.mark = ",", scientific = FALSE)))

    top_industry_nonFSM <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$`non-FSM`)) %>%
      mutate_if(is.numeric, funs(format(., big.mark = ",", scientific = FALSE)))

    top_industry_FSM <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$FSM)) %>%
      mutate_if(is.numeric, funs(format(., big.mark = ",", scientific = FALSE)))

    ifelse(first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$`non-FSM`) == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$FSM),
      sectiontext <- paste("graduates was the same for both non-FSM and FSM graduates (<b>", first(crosstabs_data$SECTIONNAME), "</b>), where median
                                earnings for non-FSM graduates were <b>£", top_industry$`non-FSM`, "</b> and for FSM graduates were <b>£", top_industry$FSM, "</b>.",
        sep = ""
      ),
      sectiontext <- paste("non-FSM graduates was <b>", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$`non-FSM`), "</b> and the median
                                earnings of non-FSM graduates in this industry were <b>£", top_industry_nonFSM$`non-FSM`, "</b>. The
                                industry with the highest proportion of FSM graduates was <b>",
        first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$FSM), "</b> and the median earnings of FSM
                                graduates in this industry were <b>£", top_industry_FSM$FSM, "</b>.",
        sep = ""
      )
    )

    ifelse(first(crosstabs_data$diff, order_by = -crosstabs_data$abs) > 0,
      FSMtext <- paste(
        "the proportion of non-FSM graduates is <b>", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        "percentage points higher </b> than the proportion of FSM graduates."
      ),
      FSMtext <- paste(
        "the proportion of FSM graduates is <b>", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        "percentage points higher </b> than the proportion of non-FSM graduates."
      )
    )

    ifelse(first(crosstabs_earnings_data$diff, order_by = -crosstabs_earnings_data$abs) > 0,
      FSMearningstext <- paste("the median earnings of non-FSM graduates were <b>£",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher </b> than the medain earnings of FSM graduates.",
        sep = ""
      ),
      FSMearningstext <- paste("the median earnings of FSM graduates were <b>£",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher</b> than the median earnings of non-FSM graduates.",
        sep = ""
      )
    )

    ifelse(first(crosstabs_earnings_data$`non-FSM`, order_by = -crosstabs_earnings_data$`non-FSM`) > first(crosstabs_earnings_data$FSM, order_by = -crosstabs_earnings_data$FSM),
      FSMearningstext2 <- paste("The group with the highest earnings was non-FSM graduates in the <b>",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$`non-FSM`), "</b>
                                     industry (median earnings of <b>£",
        format(first(crosstabs_earnings_data$`non-FSM`, order_by = -crosstabs_earnings_data$`non-FSM`), big.mark = ",", scientific = FALSE), "</b>).",
        sep = ""
      ),
      FSMearningstext2 <- paste("The group with the highest earnings was FSM graduates in the <b>",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$FSM), "</b>
                                     industry (median earnings of <b>£",
        format(first(crosstabs_earnings_data$FSM, order_by = -crosstabs_earnings_data$FSM), big.mark = ",", scientific = FALSE), "</b>).",
        sep = ""
      )
    )

    crosstab_text <- paste("For first degree graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ",
      "the industry with the highest proportion of ", sectiontext, br(), br(),
      "The biggest difference in proportions is seen in <b>", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$abs),
      "</b> where ", FSMtext, br(), br(),
      "The biggest difference in median earnings was seen in <b>", first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$abs),
      "</b> where ", FSMearningstext, br(), br(),
      FSMearningstext2,
      sep = ""
    )
  }

  if (countinput == "ethnicity") {
    crosstabs_data <- tables_data_grouped %>%
      select(-earnings_median, n = count) %>%
      spread(ethnicity, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, White, Black, Asian, Mixed, Other, `Not known`)

    crosstabs_earnings_data <- tables_data_grouped %>%
      select(-count, n = earnings_median) %>%
      spread(ethnicity, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, White, Black, Asian, Mixed, Other, `Not known`)




    ethnicityfirst <- function(ethnicity) {
      first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data[ethnicity])
    }

    ethnicityfirstdata <- c(
      ethnicityfirst("White"), ethnicityfirst("Black"), ethnicityfirst("Asian"),
      ethnicityfirst("Mixed"), ethnicityfirst("Other"), ethnicityfirst("Not known")
    )
    ethnicityfirstdata <- data.frame(ethnicityfirstdata)
    ethnicityfirstdata$ethnicity <- c("White", "Black", "Asian", "Mixed", "Other", "Not known")

    uniqueethnicity <- unique(ethnicityfirstdata$ethnicityfirstdata)

    textprod <- function(data) {
      if (length(data) != 1) {
        x <- paste(data$ethnicity[1:nrow(data) - 1], collapse = ", ")
        y <- paste(" and ", data$ethnicity[nrow(data)], sep = "")
        paste(x, y)
      } else if (length(data) == 1) {
        paste(data$ethnicity[1])
      }
    }

    if (length(uniqueethnicity) == 1) {
      ethnicitytext <- paste("<b>", uniqueethnicity, "</b> is the most common industry for all ethnicities.")
    } else if (length(uniqueethnicity) == 2) {
      data1 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[1])
      data2 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[2])

      ethnicitytext <- paste("<b>", uniqueregions[1], "</b> was the most common industry for ", textprod(data1), " ethnicity graduates,
                      and <b>", uniqueregions[2], "</b> was the most common industry for ", textprod(data2), " ethnicity graduates.")
    } else if (length(uniqueethnicity) == 3) {
      data1 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[1])
      data2 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[2])
      data3 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[3])

      ethnicitytext <- paste(
        "<b>",
        uniqueethnicity[1], "</b> was the most common industry for ", textprod(data1), " ethnicity graduates,
                      <b>", uniqueethnicity[2], "</b> was the most common industry for ", textprod(data2), " ethnicity graduates, ,and <b>",
        uniqueethnicity[3], "</b> was the most common industry for ", textprod(data3), " ethnicity graduates."
      )
    } else if (length(uniqueethnicity) == 4) {
      data1 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[1])
      data2 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[2])
      data3 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[3])
      data4 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[4])

      ethnicitytext <- paste(
        "<b>",
        uniqueethnicity[1], "</b> was the most common industry for ", textprod(data1), " ethnicity graduates,
                      <b>", uniqueethnicity[2], "</b> was the most common industry for ", textprod(data2), " ethnicity graduates, <b>",
        uniqueethnicity[3], "</b> was the most common industry for ", textprod(data3), " ethnicity graduates, and <b>",
        uniqueethnicity[4], "</b> was the most common industry for ", textprod(data4), " ethnicity graduates."
      )
    } else if (length(uniqueethnicity) == 5) {
      data1 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[1])
      data2 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[2])
      data3 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[3])
      data4 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[4])
      data5 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[5])

      ethnicitytext <- paste(
        "<b>",
        uniqueethnicity[1], "</b> was the most common industry for ", textprod(data1), " ethnicity graduates,
                      <b>", uniqueethnicity[2], "</b> was the most common industry for ", textprod(data2), " ethnicity graduates, <b>",
        uniqueethnicity[3], "</b> was the most common industry for ", textprod(data3), " ethnicity graduates, <b>",
        uniqueethnicity[4], "</b> was the most common industry for ", textprod(data4), " ethnicity graduates, and <b>",
        uniqueethnicity[5], "</b> was the most common industry for ", textprod(data5), " ethnicity graduates."
      )
    } else if (length(uniqueethnicity) == 6) {
      data1 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[1])
      data2 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[2])
      data3 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[3])
      data4 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[4])
      data5 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[5])
      data6 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[6])

      ethnicitytext <- paste(
        "<b>",
        uniqueethnicity[1], "</b> was the most common industry for ", textprod(data1), " ethnicity graduates,
                      <b>", uniqueethnicity[2], "</b> was the most common industry for ", textprod(data2), " ethnicity graduates, <b>",
        uniqueethnicity[3], "</b> was the most common industry for ", textprod(data3), " ethnicity graduates, <b>",
        uniqueethnicity[4], "</b> was the most common industry for ", textprod(data4), " ethnicity graduates, <b>",
        uniqueethnicity[5], "</b> was the most common industry for ", textprod(data5), " ethnicity graduates, and <b>",
        uniqueethnicity[6], "</b> was the most common industry for ", textprod(data6), " ethnicity graduates."
      )
    }


    biggestdiff <- rowMaxs(as.matrix(crosstabs_data[, 2:length(crosstabs_data)])) - rowMins(as.matrix(crosstabs_data[, 2:length(crosstabs_data)]))
    biggestdiff <- crosstabs_data %>%
      select(SECTIONNAME) %>%
      mutate(range = biggestdiff) %>%
      arrange(-range)

    biggestdiff2 <- crosstabs_data %>%
      filter(SECTIONNAME == first(biggestdiff$SECTIONNAME)) %>%
      select(-SECTIONNAME) %>%
      t() %>%
      data.frame() %>%
      arrange(-.)

    biggestdiffearnings <- rowMaxs(as.matrix(crosstabs_earnings_data[, 2:length(crosstabs_earnings_data)]), na.rm = TRUE) - rowMins(as.matrix(crosstabs_earnings_data[, 2:length(crosstabs_earnings_data)]), na.rm = TRUE)
    biggestdiffearnings <- crosstabs_earnings_data %>%
      select(SECTIONNAME) %>%
      mutate(range = biggestdiffearnings) %>%
      arrange(-range)

    biggestdiffearnings2 <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(biggestdiffearnings$SECTIONNAME)) %>%
      select(-SECTIONNAME) %>%
      t() %>%
      data.frame() %>%
      arrange(-.)
    biggestdiffearnings2 <- biggestdiffearnings2 %>%
      filter(is.na(.) != TRUE)

    crosstabs_earnings_data2 <- crosstabs_earnings_data[, -1]
    crosstabs_earnings_data2 <- crosstabs_earnings_data2 %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .)))

    result <- which(crosstabs_earnings_data2 == max(crosstabs_earnings_data2), arr.ind = TRUE)

    result

    crosstabs_earnings_data2[result[1], result[2]]

    crosstab_text <- paste("For first degree graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ",
      ethnicitytext,
      br(), br(), "The industry with the largest range in proportions was <b>", first(biggestdiff$SECTIONNAME), "</b>
                           where ", first(row.names(biggestdiff2)), " ethnicity graduates had the highest proportion and ", last(row.names(biggestdiff2)), "
                           ethnicity graduates had the lowest proportion.", br(), br(),
      "The industry with the largest range in median earnings was <b>", first(biggestdiffearnings$SECTIONNAME), "</b>
                           where ", first(row.names(biggestdiffearnings2)), " ethnicity graduates the highest median earnings (£",
      format(first(biggestdiffearnings2$.), big.mark = ",", scientific = FALSE), ") and ", last(row.names(biggestdiffearnings2)),
      " ethnicity graduates had the lowest median earnings (£", format(last(biggestdiffearnings2$.), big.mark = ",", scientific = FALSE), ").", br(), br(),
      "The group with the highest median earnings was <b>", colnames(crosstabs_earnings_data2)[result[2]], "</b> ethnicity graduates in
                           the <b>", crosstabs_earnings_data[result[1], ]$SECTIONNAME, "</b> industry (median earnings of <b>£",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), "</b>).",
      sep = ""
    )
  }

  if (countinput == "current_region") {
    crosstabs_data <- tables_data_grouped %>%
      select(-earnings_median, n = count) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
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
        SECTIONNAME, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    crosstabs_earnings_data <- tables_data_grouped %>%
      select(-count, n = earnings_median) %>%
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
        SECTIONNAME, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    regionfirst <- function(current_region) {
      first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data[current_region])
    }

    regionfirstdata <- c(
      regionfirst("North East"), regionfirst("North West"), regionfirst("Yorkshire and the Humber"), regionfirst("East Midlands"),
      regionfirst("West Midlands"), regionfirst("East of England"), regionfirst("London"), regionfirst("South East"),
      regionfirst("South West")
    )
    regionfirstdata <- data.frame(regionfirstdata)
    regionfirstdata$region <- c(
      "the North East", "the North West", "Yorkshire and the Humber", "the East Midlands", "the West Midlands",
      " the East of England", "London", "the South East", "the South West"
    )

    uniqueregions <- unique(regionfirstdata$regionfirstdata)

    textprod <- function(data) {
      if (length(data) != 1) {
        x <- paste(data$region[1:nrow(data) - 1], collapse = ", ")
        y <- paste(" and ", data$region[nrow(data)], sep = "")
        paste(x, y)
      } else if (length(data) == 1) {
        paste(data$region[1])
      }
    }


    print(length(uniqueregions))
    if (length(uniqueregions) == 1) {
      regiontext <- paste("<b>", uniqueregions, "</b> is the most common industry for all current regions.")
    } else if (length(uniqueregions) == 2) {
      data1 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[1])
      data2 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[2])

      regiontext <- paste(
        "<b>", uniqueregions[1], "</b> was the most common industry for those currently living in ", textprod(data1), ",
      and <b>", uniqueregions[2], "</b> was the most common industry for those living in ", textprod(data2), "."
      )
    } else if (length(uniqueregions) == 3) {
      data1 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[1])
      data2 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[2])
      data3 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[3])

      regiontext <- paste(
        "<b>",
        uniqueregions[1], "</b> was the most common industry for those currently living in ", textprod(data1), ",
                      <b>", uniqueregions[2], "</b> was the most common industry for those living in ", textprod(data2), " and <b>",
        uniqueregions[3], "</b> was the most common industry for those living in ", textprod(data3), "."
      )
    } else if (length(uniqueregions) == 4) {
      data1 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[1])
      data2 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[2])
      data3 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[3])
      data4 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[4])

      regiontext <- paste(
        "<b>",
        uniqueregions[1], "</b> was the most common industry for those currently living in ", textprod(data1), ",
                      <b>", uniqueregions[2], "</b> was the most common industry for those living in ", textprod(data2), ", <b>",
        uniqueregions[3], "</b> was the most common industry for those living in ", textprod(data3), " and <b>",
        uniqueregions[4], "</b> was the most common industry for those living in ", textprod(data4), "."
      )
    } else if (length(uniqueregions) == 5) {
      data1 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[1])
      data2 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[2])
      data3 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[3])
      data4 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[4])
      data5 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[5])

      regiontext <- paste(
        "<b>",
        uniqueregions[1], "</b> was the most common industry for those currently living in ", textprod(data1), ",
                      <b>", uniqueregions[2], "</b> was the most common industry for those living in ", textprod(data2), ", <b>",
        uniqueregions[3], "</b> was the most common industry for those living in ", textprod(data3), ", <b>",
        uniqueregions[4], "</b> was the most common industry for those living in ", textprod(data4), " and <b>",
        uniqueregions[5], "</b> was the most common industry for those living in ", textprod(data5), "."
      )
    } else if (length(uniqueregions) == 6) {
      data1 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[1])
      data2 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[2])
      data3 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[3])
      data4 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[4])
      data5 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[5])
      data6 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[6])

      regiontext <- paste(
        "<b>",
        uniqueregions[1], "</b> was the most common industry for those currently living in ", textprod(data1), ",
                      <b>", uniqueregions[2], "</b> was the most common industry for those living in ", textprod(data2), ", <b>",
        uniqueregions[3], "</b> was the most common industry for those living in ", textprod(data3), ", <b>",
        uniqueregions[4], "</b> was the most common industry for those living in ", textprod(data4), ", <b>",
        uniqueregions[5], "</b> was the most common industry for those living in ", textprod(data5), " and <b>",
        uniqueregions[6], "</b> was the most common industry for those living in ", textprod(data6), "."
      )
    } else {
      # Just going to recreate the above with the collapse flag. Leaving it in
      # the else area for now, but should cover all of the above if statements in one go.
      print("### Using new code for region text...")
      data <- regionfirstdata %>% filter(regionfirstdata %in% uniqueregions)
      textarray <- c()
      for (i in 1:nrow(data)) {
        textarray <- c(textarray, textprod(data[i, ]))
      }
      regiontext <- paste0(paste0("<b>", uniqueregions,
        "</b> was the most common industry for those living in ",
        textarray,
        collapse = ", "
      ), ".")
    }

    crosstabs_earnings_data2 <- crosstabs_earnings_data[, -1]
    crosstabs_earnings_data2 <- crosstabs_earnings_data2 %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .)))

    result <- which(crosstabs_earnings_data2 == max(crosstabs_earnings_data2), arr.ind = TRUE)

    crosstab_text <- paste(
      "For first degree graduates of ", subjecttext, ", ",
      YAGinput, " years after graduation, ", regiontext,
      br(), br(),
      "The group with the highest earnings was graduates currently living in ",
      colnames(crosstabs_earnings_data2)[result[2]], " working in the ",
      crosstabs_earnings_data[result[1], ]$SECTIONNAME, " industry (median earnings of £",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), ").",
      sep = ""
    )
  }

  if (countinput == "prior_attainment") {
    crosstabs_data <- tables_data_grouped %>%
      select(-earnings_median, n = count) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    crosstabs_earnings_data <- tables_data_grouped %>%
      select(-count, n = earnings_median) %>%
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
      select(SECTIONNAME, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    footer_data <- tables_data_grouped %>%
      select(-earnings_median, n = count) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    grad_numbers <- c(
      sum(footer_data$`1`), sum(footer_data$`2`), sum(footer_data$`3`), sum(footer_data$`4`),
      sum(footer_data$`5`), sum(footer_data$`6`), sum(footer_data$`7`), sum(footer_data$`8`),
      sum(footer_data$`9`)
    )

    grad_numbers <- data.frame(grad_numbers)
    grad_numbers$prior_attainment <- c(
      "4 As or more", "360 points", "300-359 points", "240-299 points", "180-239 points", "Below 180 points", "1 or 2 A level passes",
      "BTEC", "Other"
    )
    grad_numbers$band <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")

    topindustry <- crosstabs_data %>%
      select(SECTIONNAME, first(grad_numbers$band, order_by = -grad_numbers$grad_numbers))

    crosstabs_earnings_data2 <- crosstabs_earnings_data %>%
      filter(SECTIONNAME == first(topindustry$SECTIONNAME, order_by = -topindustry[2])) %>%
      select(first(grad_numbers$band, order_by = -grad_numbers$grad_numbers))

    crosstabs_earnings_data3 <- crosstabs_earnings_data[, -1]
    crosstabs_earnings_data3 <- crosstabs_earnings_data3 %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      select(-All)

    result <- which(crosstabs_earnings_data3 == max(crosstabs_earnings_data3), arr.ind = TRUE)
    names(crosstabs_earnings_data3) <- c(
      "4 As or more", "360 points", "300-359 points", "240-299 points", "180-239 points", "Below 180 points", "1 or 2 A level passes",
      "BTEC", "Other", "Not known"
    )

    crosstab_text <- paste("For first degree graduates of ", subjecttext, ", ", YAGinput, " years after graduation, the prior attainment band
                           with the highest number of graduates was `", first(grad_numbers$prior_attainment, order_by = -grad_numbers$grad_numbers), "`.
                           Within this prior attainment band, the most common industry was <b>",
      first(topindustry$SECTIONNAME, order_by = -topindustry[2]), "</b>, and the median earnings for graduates with this prior
                       attainment band working in this industry were <b>£", format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE),
      "</b>.", br(), br(),
      "The group with the highest median earnings was graduates in the ", colnames(crosstabs_earnings_data3[result[2]]),
      " prior attainment band who worked in the <b>", crosstabs_earnings_data[result[1], ]$SECTIONNAME, "</b> industry (median
                           earnings of <b>£", format(max(crosstabs_earnings_data3), big.mark = ",", scientific = FALSE), "</b>).",
      sep = ""
    )
  }

  if (countinput == "subject_name") {
    crosstabs_earnings_data <- tables_data_grouped %>%
      select(-count, n = earnings_median) %>%
      spread(subject_name, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(-All)

    crosstabs_earnings_data2 <- crosstabs_earnings_data[, -1]
    crosstabs_earnings_data2 <- crosstabs_earnings_data2 %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .)))

    result <- which(crosstabs_earnings_data2 == max(crosstabs_earnings_data2), arr.ind = TRUE)

    crosstab_text <- paste("When splitting by subject for ", qualinput, " graduates, ", YAGinput, " years after graduation,
                           the highest earning group was graduates of ", colnames(crosstabs_earnings_data2)[result[2]], " who
                           worked in the <b>", crosstabs_earnings_data[result[1], ]$SECTIONNAME, "</b> industry (median earnings of <b>£",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), "</b>).",
      sep = ""
    )
  }

  if (countinput == "qualification_TR") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      select(qualification_TR, SECTIONNAME, n = count) %>%
      spread(qualification_TR, n) %>%
      as.data.frame() %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      group_by(qualification_TR, SECTIONNAME) %>%
      summarise(n = earnings_median) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .)))

    qualfirst <- function(qualification_TR) {
      first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data[qualification_TR])
    }

    qualfirstdata <- c(
      qualfirst("First degree"), qualfirst("Level 7 (taught)"), qualfirst("Level 7 (research)"),
      qualfirst("Level 8")
    )
    qualfirstdata <- data.frame(qualfirstdata)
    qualfirstdata$qual <- c("First degree", "Level 7 (taught)", "Level 7 (research)", "Level 8")

    uniquequal <- unique(qualfirstdata$qualfirstdata)

    textprod <- function(data) {
      if (length(data) != 1) {
        x <- paste(data$qual[1:nrow(data) - 1], collapse = ", ")
        y <- paste(" and ", data$qual[nrow(data)], sep = "")
        paste(x, y)
      } else if (length(data) == 1) {
        paste(data$qual[1])
      }
    }

    if (length(uniquequal) == 1) {
      qualtext <- paste("<b>", uniquequal, "</b> is the most common industry for all qualification levels.")
    } else if (length(uniquequal) == 2) {
      data1 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[1])
      data2 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[2])

      qualtext <- paste("<b>", uniquequal[1], "</b> was the most common industry for ", textprod(data1), " graduates,
                      and <b>", uniquequal[2], ",/b> was the most common industry for ", textprod(data2), " graduates.")
    } else if (length(uniquequal) == 3) {
      data1 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[1])
      data2 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[2])
      data3 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[3])

      qualtext <- paste(
        "<b>",
        uniquequal[1], "</b> was the most common industry for ", textprod(data1), " graduates,
                      <b>", uniquequal[2], "</b> was the most common industry for ", textprod(data2), " graduates, and <b>",
        uniquequal[3], "</b> was the most common industry for ", textprod(data3), " graduates."
      )
    } else if (length(uniquequal) == 4) {
      data1 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[1])
      data2 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[2])
      data3 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[3])
      data4 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[4])

      qualtext <- paste(
        "<b>",
        uniquequal[1], "</b> was the most common industry for ", textprod(data1), " graduates,
                      <b>", uniquequal[2], "</b> was the most common industry for ", textprod(data2), " graduates, <b>",
        uniquequal[3], "</b> was the most common industry for ", textprod(data3), " graduates, and <b>",
        uniquequal[4], "</b> was the most common industry for ", textprod(data4), " graduates."
      )
    }

    crosstabs_earnings_data2 <- crosstabs_earnings_data[, -1]
    crosstabs_earnings_data2 <- crosstabs_earnings_data2 %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .)))

    result <- which(crosstabs_earnings_data2 == max(crosstabs_earnings_data2), arr.ind = TRUE)


    crosstab_text <- paste("For graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ", qualtext, br(), br(),
      "The highest earning group was ", colnames(crosstabs_earnings_data2)[result[2]], " graduates
                           working in the <b>", crosstabs_data[result[1], ]$SECTIONNAME, "</b> industry (median earnings of <b>£",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), "</b>).",
      sep = ""
    )
  }

  return(crosstab_text)
}


# 2. Function to create the crosstabs table.
# ==========================================
crosstabs <- function(tables_data_grouped, subjectinput, YAGinput, countinput, qualinput, buttoninput) {
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
    crosstabs_basedata <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree"
      ) %>%
      as.data.frame()

    crosstabs_data_table <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(ethnicity, SECTIONNAME, group_name, n = count) %>%
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

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(ethnicity, SECTIONNAME, group_name, n = earnings_median) %>%
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


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
      crosstabs_data <- crosstabs_data_table[,c(1,2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(ethnicity, SECTIONNAME, group_name, n = count) %>%
      spread(ethnicity, n) %>%
      as.data.frame() %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`)

    column_defs <- col_formats(crosstabs_data, footer_data, colformat)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    # Now create the nested tables data
    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(ethnicity) %>%
      mutate(prop = count / sum(count, na.rm = TRUE))

    nested_table <- tables_data_nested %>%
      select(ethnicity, SECTIONNAME, group_name, n = prop) %>%
      spread(ethnicity, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

    if ("All" %in% names(nested_table)) {
      nested_table <- nested_table %>%
        arrange(-All)
    }

    nested_table_earnings <- tables_data_nested %>%
      select(ethnicity, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(ethnicity, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) # %>%
    # mutate_at(c('White', 'Black','Asian', 'Mixed', 'Other', 'Not known'),
    #           funs(as.numeric(.))) %>%
    # mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
    # select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`) %>%
    # ungroup()


    if (buttoninput == "Proportions") {
      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
      nested <- nested_table[,c(1,2)] %>% left_join(nested_table_earnings, by = c("SECTIONNAME", "group_name")) 
    }
    
    for (column in column_defs$numeric_cols) {
      nested[column] <- if (column %in% colnames(nested)) {
        nested[column]
      } else {
        NA
      }
    }

    nested <- nested %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`)
  }

  if (countinput == "current_region") {
    crosstabs_basedata <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree"
      ) %>%
      as.data.frame()

      crosstabs_data_table <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(current_region, SECTIONNAME, group_name, n = count) %>%
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

      crosstabs_earnings_data <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(current_region, SECTIONNAME, group_name, n = earnings_median) %>%
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
      if (buttoninput == "Proportions") {
        colformat <- colFormat(percent = TRUE, digits = 1)
        crosstabs_data <- crosstabs_data_table
      } else if (buttoninput == "Median earnings") {
        colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
        # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
        crosstabs_data <- crosstabs_data_table[,c(1,2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
      }
      
    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(current_region, SECTIONNAME, group_name, n = count) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(
        SECTIONNAME, group_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    column_defs <- col_formats(crosstabs_data, footer_data, colformat)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(current_region) %>%
      mutate(prop = count / sum(count, na.rm = TRUE))

    if (buttoninput == "Proportions") {
      nested_table <- tables_data_nested %>%
        select(current_region, SECTIONNAME, group_name, n = prop) %>%
        spread(current_region, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      nested_table_earnings <- crosstabs_basedata %>%
        filter(group_name != "All") %>%
        select(current_region, SECTIONNAME, group_name, n = earnings_median) %>%
        spread(current_region, n) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))
      nested <- nested_table_earnings
    }
    if ("All" %in% names(nested)) {
      nested <- nested %>%
        arrange(-All)
    }
    
    for (column in column_defs$numeric_cols) {
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
  }

  if (countinput == "FSM") {
    crosstabs_basedata <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree"
      ) %>%
      as.data.frame()

    crosstabs_data_table <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(FSM, SECTIONNAME, group_name, n = count) %>%
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

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(FSM, SECTIONNAME, group_name, n = earnings_median) %>%
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


    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
      crosstabs_data <- crosstabs_data_table[,c(1,2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(FSM, SECTIONNAME, group_name, n = count) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)

    column_defs <- col_formats(crosstabs_data, footer_data, colformat)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
      ) %>%
      group_by(FSM) %>%
      mutate(prop = count / sum(count, na.rm = TRUE))

    nested_table <- tables_data_nested %>%
      filter(group_name != "All") %>%
      select(FSM, SECTIONNAME, group_name, n = prop) %>%
      spread(FSM, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

    if ("All" %in% names(nested_table)) {
      nested_table <- nested_table %>%
        arrange(-All)
    }

    nested_table_earnings <- tables_data_nested %>%
      filter(group_name != "All") %>%
      select(FSM, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(FSM, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

    if (buttoninput == "Proportions") {
      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      nested <- nested_table_earnings
    }

    for (column in column_defs$numeric_cols) {
      nested[column] <- if (column %in% colnames(nested)) {
        nested[column]
      } else {
        NA
      }
    }

    nested <- nested %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)
  }

  if (countinput == "sex") {
    crosstabs_basedata <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput
      ) %>%
      as.data.frame()

      crosstabs_data_table <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(sex, SECTIONNAME, group_name, n = count) %>%
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

      crosstabs_earnings_data <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(sex, SECTIONNAME, group_name, n = earnings_median) %>%
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

      if (buttoninput == "Proportions") {
        colformat <- colFormat(percent = TRUE, digits = 1)
        crosstabs_data <- crosstabs_data_table
      } else if (buttoninput == "Median earnings") {
        colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
        # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
        crosstabs_data <- crosstabs_data_table[,c(1,2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(sex, SECTIONNAME, group_name, n = count) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`) %>%
      as.data.frame()
    names(footer_data) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

    column_defs <- col_formats(crosstabs_data, footer_data, colformat)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    table_group_proportions <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, group_name != "All"
      ) %>%
      group_by(sex) %>%
      mutate(proportion = count / sum(count, na.rm = TRUE))

      nested_table <- table_group_proportions %>%
        select(sex, SECTIONNAME, group_name, n = proportion) %>%
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
      nested_table_earnings <- table_group_proportions %>%
        select(sex, SECTIONNAME, group_name, n = earnings_median) %>%
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
      if (buttoninput == "Proportions") {
        nested <- nested_table
      } else if (buttoninput == "Median earnings") {
        # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
        nested <- nested_table[,c(1,2)] %>% left_join(nested_table_earnings, by = c("SECTIONNAME", "group_name")) 
    }
  }

  if (countinput == "prior_attainment") {
    crosstabs_basedata <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree"
      ) %>%
      as.data.frame()

      crosstabs_data_table <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(prior_attainment, SECTIONNAME, group_name, n = count) %>%
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

      crosstabs_earnings_data <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(prior_attainment, SECTIONNAME, group_name, n = earnings_median) %>%
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

    if (buttoninput == "Proportions") {
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
      crosstabs_data <- crosstabs_data_table[,c(1,2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }
    
    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(prior_attainment, SECTIONNAME, group_name, n = count) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    column_defs <- col_formats(crosstabs_data, footer_data, colformat)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(prior_attainment) %>%
      mutate(prop = count / sum(count, na.rm = TRUE))

    nested_table <- tables_data_nested %>%
      select(prior_attainment, SECTIONNAME, group_name, n = prop) %>%
      spread(prior_attainment, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))
    nested_table_earnings <- tables_data_nested %>%
      select(prior_attainment, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(prior_attainment, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

    if (buttoninput == "Proportions") {
      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
      nested <- nested_table[,c(1,2)] %>% left_join(nested_table_earnings) 
    }
    
    if ("All" %in% names(nested)) {
      nested <- nested %>%
        arrange(-All)
    }
    
    
    for (column in column_defs$numeric_cols) {
      nested[column] <- if (column %in% colnames(nested)) {
        nested[column]
      } else {
        NA
      }
    }

    nested <- nested %>%
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")
  }

  if (countinput == "subject_name") {
    crosstabs_basedata <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput
      ) %>%
      as.data.frame()

      crosstabs_data_table <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(subject_name, SECTIONNAME, group_name, n = count) %>%
        spread(subject_name, n) %>%
        arrange(-All) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
        ungroup() %>%
        mutate_if(is.numeric, funs(. / sum(.))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
        select(-All)


            crosstabs_earnings_data <- crosstabs_basedata %>%
        filter(group_name == "All") %>%
        select(subject_name, SECTIONNAME, group_name, n = earnings_median) %>%
        spread(subject_name, n) %>%
        arrange(-All) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
        select(-All)
      if (buttoninput == "Proportions") {
        colformat <- colFormat(percent = TRUE, digits = 1)
        crosstabs_data <- crosstabs_data_table
      } else if (buttoninput == "Median earnings") {
        colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
        # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
        crosstabs_data <- crosstabs_data_table[,c(1,2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
      }
      
    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(subject_name, SECTIONNAME, group_name, n = count) %>%
      spread(subject_name, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(-All)

    column_defs <- col_formats(crosstabs_data, footer_data, colformat)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(subject_name) %>%
      mutate(prop = count / sum(count, na.rm = TRUE))

    nested_table <- tables_data_nested %>%
      group_by(subject_name, SECTIONNAME, group_name) %>%
      summarise(n = prop) %>%
      spread(subject_name, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), (funs(ifelse(. == 0, NA, .))))

    if ("All" %in% names(nested_table)) {
      nested_table <- nested_table %>%
        arrange(-All)
    }

    nested_table_earnings <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(subject_name, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(subject_name, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

    if (buttoninput == "Proportions") {
      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      nested <- nested_table_earnings
    }

    for (column in column_defs$numeric_cols) {
      nested[column] <- if (column %in% colnames(nested)) {
        nested[column]
      } else {
        NA
      }
    }
  }

  if (countinput == "qualification_TR") {
      crosstabs_data_table <- tables_data %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", prior_attainment == "All", group_name == "All"
        ) %>%
        select(qualification_TR, SECTIONNAME, group_name, n = count) %>%
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
      if (buttoninput == "Proportions") {
        colformat <- colFormat(percent = TRUE, digits = 1)
        crosstabs_data <- crosstabs_data_table
      } else if (buttoninput == "Median earnings") {
        colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
        # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table. 
        crosstabs_data <- crosstabs_data_table[,c(1,2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
      }
      
    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      select(qualification_TR, SECTIONNAME, group_name, n = count) %>%
      spread(qualification_TR, n) %>%
      as.data.frame() %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)


    column_defs <- col_formats(crosstabs_data, footer_data, colformat)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    table_group_proportions <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name != "All"
      ) %>%
      group_by(qualification_TR) %>%
      mutate(proportion = count / sum(count, na.rm = TRUE))
    if (buttoninput == "Proportions") {
      nested <- table_group_proportions %>%
        select(qualification_TR, SECTIONNAME, group_name, n = proportion) %>%
        spread(qualification_TR, n)
    } else if (buttoninput == "Median earnings") {
      nested <- table_group_proportions %>%
        select(qualification_TR, SECTIONNAME, group_name, n = earnings_median) %>%
        spread(qualification_TR, n)
    }
    if ("All" %in% names(nested)) {
      nested <- nested %>% arrange(-All)
    }
    
    for (column in column_defs$numeric_cols) {
      nested[column] <- if (column %in% colnames(nested)) {
        nested[column]
      } else {
        NA
      }
    }
    nested <- nested %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)
  }


  coldefs <- list(
    SECTIONNAME = colDef(na = "x", name = "Industry", width = 500, footer = "TOTAL (N)"),
    group_name = colDef(na = "x", name = "3 digit SIC code", width = 300, footer = "TOTAL (N)")
  )
  coldefs_nested <- list(
    SECTIONNAME = colDef(na = "x", name = "Industry", width = 500),
    group_name = colDef(na = "x", name = "3 digit SIC code", width = 300)
  )

  nested_groups <- function(index) {
    nested <- nested %>% filter(SECTIONNAME == crosstabs_data$SECTIONNAME[index])
    htmltools::div(
      style = "padding: 16px",
      reactable(nested,
        outlined = TRUE,
        style = JS(script), columns = c(coldefs_nested, numeric_cols_def_nested),
        defaultPageSize = 300
      )
    )
  }

  crosstab <- reactable(crosstabs_data,
    details = nested_groups,
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
