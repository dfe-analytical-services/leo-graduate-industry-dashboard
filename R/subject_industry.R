col_formats <- function(data, footer_data, cellfunc, minWidth = NULL) {
  max <- data %>%
    ungroup() %>%
    select(-c(group_name, SECTIONNAME)) %>%
    mutate_all(funs(ifelse(. < 0, NA, .)))
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
              // If value equals 0, x, or c, set background to white.
              if (value < 0.001 || isNaN(value)) {
                var color = '#000000'
                var bg = '#FFFFFF'
              } else {
                var color = '#000000'
                var bg = hslToHex(209, 59, 100 - pct_value / 2)
              }
              return { color: color, backgroundColor: bg}
          }", sep = "")

    numeric_cols_def_nested[column] <- list(colDef(
      na = "x", style = JS(script), cell = cellfunc,
      minWidth = minWidth
    ))

    numeric_cols_def[column] <- list(colDef(
      na = "x", style = JS(script), cell = cellfunc,
      footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T),
      minWidth = minWidth
    ))
  }
  return(list(numeric_cols = numeric_cols, numeric_cols_def = numeric_cols_def, numeric_cols_def_nested = numeric_cols_def_nested, script = script))
}

topIndustries <- function(data, filter_value) {
  dfMaxLines <- data %>%
    filter(filter == filter_value, count > 0) %>%
    filter(count == max(count, na.rm = TRUE))
  return(format_filtervalues(dfMaxLines$SECTIONNAME))
}

format_filtervalues <- function(filtervalues) {
  filtervalues <- sort(unique(filtervalues))
  if (length(filtervalues) == 1) {
    return(filtervalues)
  } else {
    return(paste0(paste0(filtervalues[1:length(filtervalues) - 1], collapse = ", "), " and ", filtervalues[length(filtervalues)]))
  }
}

funcRangeEarnings <- function(dfGroupedData, allcat = "All", prefix = " ", suffix = " graduates",
                              exclNK = TRUE, fs = TRUE) {
  ifelse(exclNK, strNK <- "Not known", strNK <- "")
  dfFiltered <- dfGroupedData %>%
    filter(
      earnings_median > 0, !is.na(earnings_median),
      filter != allcat, filter != strNK, SECTIONNAME != strNK, !is.na(filter)
    )
  dfRangesEarnings <- dfFiltered %>%
    group_by(SECTIONNAME) %>%
    summarise(
      earnings_range = max(earnings_median, na.rm = TRUE) - min(earnings_median, na.rm = TRUE),
      earnings_max = max(earnings_median, na.rm = TRUE),
      earnings_min = min(earnings_median, na.rm = TRUE)
    ) %>%
    left_join(dfFiltered, by = c("SECTIONNAME" = "SECTIONNAME", "earnings_max" = "earnings_median")) %>%
    rename(max_filter = filter) %>%
    left_join(dfFiltered, by = c("SECTIONNAME" = "SECTIONNAME", "earnings_min" = "earnings_median")) %>%
    rename(min_filter = filter) %>%
    mutate(strEarningsRange = paste0("£", format(earnings_range, big.mark = ",", scientific = FALSE))) %>%
    mutate(strEarningsMax = paste0("£", format(earnings_max, big.mark = ",", scientific = FALSE))) %>%
    mutate(strEarningsMin = paste0("£", format(earnings_min, big.mark = ",", scientific = FALSE))) %>%
    arrange(-earnings_range) %>%
    distinct() %>%
    filter(max_filter != min_filter)
  if (nrow(dfRangesEarnings) > 0 & max(dfRangesEarnings$earnings_range) > 0) {
    dfWidest <- dfRangesEarnings %>% filter(earnings_range == max(earnings_range, na.rm = TRUE))
    if (nrow(dfWidest) == 1 | length(unique(dfWidest$SECTIONNAME)) == 1) {
      text <- paste0(
        " The industry with the largest range in median earnings was <b>",
        unique(dfWidest$SECTIONNAME), "</b>",
        " where ", format_filtervalues(dfWidest$max_filter), suffix,
        " had the highest median earnings (<b>",
        format_filtervalues(dfWidest$strEarningsMax),
        "</b>) and ", format_filtervalues(dfWidest$min_filter), suffix,
        " had the lowest median earnings (<b>",
        format_filtervalues(dfWidest$strEarningsMin), "</b>)"
      )
    } else {
      text <- paste0(
        " The industries with the largest range in median earnings were <b>",
        paste(dfWidest$SECTIONNAME, collapse = " and "), "</b>."
      )
      for (i in 1:nrow(dfWidest)) {
        text <- paste0(
          text,
          " For the ", dfWidest$SECTIONNAME[i], " industry, ", dfWidest$max_filter[i], suffix,
          " had the highest median earnings (<b>",
          dfWidest$strEarningsMax[i],
          "</b>) and ", dfWidest$min_filter[i], suffix,
          " had the lowest median earnings (<b>",
          dfWidest$strEarningsMin[i], "</b>)"
        )
        if (i < nrow(dfWidest)) {
          paste0(text, ". ")
        }
      }
    }
    if (fs) {
      text <- paste0(text, ". ")
    }
  } else {
    text <- ""
  }
  return(text)
}



funcHighestEarnings <- function(dfGroupedData, allcat = "All", prefix = " ", suffix = " graduates",
                                exclNK = TRUE, fs = TRUE) {
  ifelse(exclNK, strNK <- "Not known", strNK <- "")
  dfHighestEarnings <- dfGroupedData %>%
    filter(
      earnings_median > 0, !is.na(earnings_median),
      filter != allcat, filter != strNK, !is.na(filter)
    ) %>%
    mutate(strMedianEarn = paste0("£", format(earnings_median, big.mark = ",", scientific = FALSE))) %>%
    arrange(-earnings_median)
  if (nrow(dfHighestEarnings) > 0) {
    dfHighestEarnings <- dfHighestEarnings %>% filter(earnings_median == max(earnings_median, na.rm = TRUE))
    text <- paste0(
      "The group with the highest earnings was ", prefix, "<b>",
      dfHighestEarnings$filter[1], "</b>", suffix, " who worked in the <b>",
      dfHighestEarnings$SECTIONNAME[1],
      "</b> industry (median earnings of <b>",
      dfHighestEarnings$strMedianEarn[1], "</b>)"
    )
    if (fs) {
      text <- paste0(text, ". ")
    }
  } else {
    text <- ""
  }
  return(text)
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
  } else if (countinput == "qualification_TR") {
    tables_data_grouped <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      select(qualification_TR, SECTIONNAME, count, earnings_median)
  }

  return(tables_data_grouped %>% as.data.frame())
}

crosstab_text <- function(tables_data_grouped, subjectinput, YAGinput, countinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )
  
  if (YAGinput == 1) {
    YAGtext <- "one year"
  } else if (YAGinput == 3) {
    YAGtext <- "three years"
  } else if (YAGinput == 5) {
    YAGtext <- "five years"
  } else if (YAGinput == 10) {
    YAGtext <- "ten years"
  }
  
  if (nrow(tables_data_grouped %>% filter(!is.na(count), count > 0)) == 0) {
    crosstab_text <- ""
  } else {
    if (countinput == "sex") {
      # !!! Set Not known to be top group for testing:
      # tables_data_grouped[tables_data_grouped$SECTIONNAME=='Not known' & tables_data_grouped$sex=='F','count'] <- 10*max(tables_data_grouped$count,na.rm=TRUE)
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      crosstabs_data <- tables_data_grouped %>%
        select(-earnings_median, n = count) %>%
        spread(sex, n) %>%
        colorders(countinput) %>%
        arrange(-`F+M`) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_if(is.numeric, funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))) %>%
        # mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
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
        colorders(countinput) %>%
        arrange(-`F+M`) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        # mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
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

      # ! Test data
      # crosstabs_earnings_data[crosstabs_earnings_data$SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female),]$Female <- -10000.
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      top_industry <- crosstabs_earnings_data %>%
        filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME)) %>%
        mutate_if(is.numeric, funs(format(., big.mark = ",", scientific = FALSE)))

      top_industry_female <- crosstabs_earnings_data %>%
        filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female)) %>%
        mutate_if(is.numeric, funs(paste0("£", format(., big.mark = ",", scientific = FALSE)))) %>%
        mutate_all(funs(gsub("£-10,000", "suppressed", .))) %>%
        mutate_all(funs(ifelse(is.na(.), "not available", .)))

      top_industry_male <- crosstabs_earnings_data %>%
        filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Male)) %>%
        mutate_if(is.numeric, funs(paste0("£", format(., big.mark = ",", scientific = FALSE)))) %>%
        mutate_all(funs(gsub("£-10,000", "suppressed", .))) %>%
        mutate_all(funs(ifelse(is.na(.), "not available", .)))


      if (first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female) == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Male)) {
        sectiontext <- paste("the industry with the highest proportion of graduates is the same for both female and male graduates (<b>",
          first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female), "</b>). The median
                                earnings for females in this industry  were <b>", top_industry_female$Female, "</b> and for males were <b>",
          top_industry_male$Male, "</b>.",
          sep = ""
        )
      } else {
        top_section_female <- first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female)
        if (top_section_female != "Not known") {
          section_text_female <- paste0(
            "the industry with the highest proportion of female graduates is <b>",
            top_section_female, "</b>, and the median earnings of females in this industry were <b>",
            top_industry_female$Female, "</b>."
          )
        } else {
          top_section_female_exclnk <- (crosstabs_data %>% filter(SECTIONNAME != "Not known") %>% arrange(-Female))[1, ]
          top_earnings_female_exclnk <- crosstabs_earnings_data %>%
            filter(SECTIONNAME == top_section_female_exclnk$SECTIONNAME) %>%
            mutate_if(is.numeric, funs(paste0("£", format(., big.mark = ",", scientific = FALSE)))) %>%
            mutate_all(funs(gsub("£-10,000", "suppressed", .)))

          section_text_female <- paste0(
            "the group with the highest proportion of female graduates is where the <b>industry is not known</b> and the median earnings of females in this group was <b>",
            top_industry_female$Female, "</b>. The industry with the highest proportion of female graduates after excluding the Not known category is ",
            top_section_female_exclnk$SECTIONNAME, " where the median earnings were <b>", top_earnings_female_exclnk$Female, "</b>. "
          )
        }
        top_section_male <- first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Male)
        if (top_section_male != "Not known") {
          section_text_male <- paste0(
            "The industry with the highest proportion of male graduates was <b>",
            top_section_male,
            "</b> and the median earnings of males in this industry were <b>",
            top_industry_male$Male, "</b>."
          )
        } else {
          top_section_male_exclnk <- (crosstabs_data %>%
            filter(SECTIONNAME != "Not known") %>%
            arrange(-Male))[1, ]
          top_earnings_male_exclnk <- crosstabs_earnings_data %>%
            filter(SECTIONNAME == top_section_male_exclnk$SECTIONNAME) %>%
            mutate_if(is.numeric, funs(paste0("£", format(., big.mark = ",", scientific = FALSE)))) %>%
            mutate_all(funs(gsub("£-10,000", "suppressed", .)))

          section_text_male <- paste0(
            "The group with the highest proportion of male graduates is where the <b>industry is not known</b> (the median earnings of males in which was <b>£",
            top_industry_male$Male, "</b>). The industry with the highest proportion of male graduates after excluding the not known category is <b>",
            top_section_male_exclnk$SECTIONNAME, "</b> where the median earnings were <b>£", top_earnings_male_exclnk$Male, "</b>."
          )
        }
        sectiontext <- paste(section_text_female, section_text_male)
      }

      sex_prop_text <- function(crosstabs, position = "first") {
        # Determine text for the difference in proportion of grads of different sex in a given industry
        if (position == "first") {
          line <- (crosstabs %>% arrange(-abs))[1, ]
        } else {
          line <- crosstabs %>% filter(SECTIONNAME == position)
        }
        ifelse(line$diff > 0,
          sextext <- paste(
            "the proportion of male graduates is <b>", round(line$abs * 100, digits = 1),
            " percentage points higher</b> than the proportion of female graduates."
          ),
          sextext <- paste(
            "the proportion of female graduates is <b>", round(line$abs * 100, digits = 1),
            " percentage points higher</b> than the proportion of male graduates."
          )
        )
      }

      # Calculate the text for industry with biggest gender difference between pay:
      dfEarningsTopDiff <- crosstabs_earnings_data %>%
        filter(Female > 0, Male > 0, !is.na(Female), !is.na(Male)) %>%
        arrange(-abs) %>%
        filter(abs == max(abs))
      if (nrow(dfEarningsTopDiff) == 0) {
        # Don't output anything if all the rows are masked out as suppressed.
        sextextearnings <- ""
      } else {
        text_sexdiff_base <- paste0(
          " The biggest difference in median earnings is seen in <b>",
          paste(dfEarningsTopDiff$SECTIONNAME, collapse = " and "),
          "</b> where "
        )
        if (all(dfEarningsTopDiff$diff > 0)) {
          sextextearnings <- paste0(
            text_sexdiff_base,
            "the median earnings of male graduates were <b>£",
            format(unique(dfEarningsTopDiff$abs), big.mark = ",", scientific = FALSE),
            "  higher</b> than the median earnings of female graduates."
          )
        } else if (all(dfEarningsTopDiff$diff < 0)) {
          sextextearnings <- paste0(
            text_sexdiff_base,
            "the median earnings of female graduates were <b>£",
            format(unique(dfEarningsTopDiff$abs), big.mark = ",", scientific = FALSE),
            " higher</b> than the median earnings of male graduates."
          )
        }
      }


      # Create text for proportion of grads in top two industries:
      ifelse(abs(round(sum(crosstabs_data$Female[1:2]) * 100, digits = 1) - round(sum(crosstabs_data$Male[1:2] * 100), digits = 1)) > 5,
        sextext2 <- paste("<b>", round(sum(crosstabs_data$Female[1:2]) * 100, digits = 1), "%</b> of female graduates are concentrated in the top 2
                           industries (either <b>", first(crosstabs_data$SECTIONNAME), "</b> or <b>", crosstabs_data$SECTIONNAME[2], "</b>),
                           whereas for male graduates this is ", round(sum(crosstabs_data$Male[1:2] * 100), digits = 1), "%.", sep = ""),
        sextext2 <- paste("")
      )

      # Create the highest earnings text.
      textHighestEarnings <- funcHighestEarnings(tables_data_grouped %>%
        mutate(filter = case_when(sex == "F" ~ "Female", sex == "M" ~ "Male", sex == "F+M" ~ "All", TRUE ~ "NA")))

      top_prop_industry <- first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$abs)
      if (top_prop_industry == "Not known") {
        prop_preamble <- " The biggest difference in proportions is seen for the group where the industry is <b>Not known</b> where "
        extra_industry <- first((crosstabs_earnings_data %>% filter(SECTIONNAME != "Not known"))$SECTIONNAME,
          order_by = -crosstabs_earnings_data$abs
        )
        sex_prop_extra <- paste0(
          "The industry with the biggest difference in proportions after excluding the Not known category is <b>",
          extra_industry, "</b> where ",
          sex_prop_text(crosstabs_data, position = extra_industry)
        )
      } else {
        prop_preamble <- paste0(
          " The biggest difference in proportions is seen in <b>",
          top_prop_industry, "</b> where "
        )
        sex_prop_extra <- ""
      }

      # Here's the final output text:
      crosstab_text <- paste("For ", tolower(qualinput), " graduates of ", subjecttext, ", ",
        YAGtext, " after graduation, ",
        sectiontext,
        br(), br(),
        prop_preamble,
        sex_prop_text(crosstabs_data),
        sex_prop_extra,
        sextextearnings,
        textHighestEarnings,
        sextext2, br(), br(),
        sep = ""
      )
    }

    if (countinput == "FSM") {
      crosstabs_data <- tables_data_grouped %>%
        select(-earnings_median, n = count) %>%
        spread(FSM, n) %>%
        colorders(countinput) %>%
        arrange(-All) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
        mutate_if(is.numeric, funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))) %>%
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
        colorders(countinput) %>%
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
        mutate_if(is.numeric, funs(paste0("£", format(., big.mark = ",", scientific = FALSE)))) %>%
        mutate_all(funs(gsub("£-10,000", "suppressed", .))) %>%
        mutate_all(funs(ifelse(is.na(.), "not available", .)))

      top_industry_FSM <- crosstabs_earnings_data %>%
        filter(SECTIONNAME == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$FSM)) %>%
        mutate_if(is.numeric, funs(paste0("£", format(., big.mark = ",", scientific = FALSE)))) %>%
        mutate_all(funs(gsub("£-10,000", "suppressed", .))) %>%
        mutate_all(funs(ifelse(is.na(.), "not available", .)))

      ifelse(first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$`non-FSM`) == first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$FSM),
        sectiontext <- paste0(
          "the industry with the highest proportion of graduates was the same for both non-FSM and FSM graduates (<b>",
          top_industry_FSM$SECTIONNAME, "</b>), where median earnings for non-FSM graduates were <b>",
          top_industry_nonFSM$`non-FSM`, "</b> and for FSM graduates were <b>", top_industry_FSM$FSM, "</b>."
        ),
        sectiontext <- paste("the industry with the highest proportion of non-FSM graduates was <b>", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$`non-FSM`), "</b> and the median
                                earnings of non-FSM graduates in this industry were <b>",
          top_industry_nonFSM$`non-FSM`, "</b>. The
                                industry with the highest proportion of FSM graduates was <b>",
          first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$FSM), "</b> and the median earnings of FSM
                                graduates in this industry were <b>", top_industry_FSM$FSM, "</b>.",
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
          "  higher </b> than the median earnings of FSM graduates.",
          sep = ""
        ),
        FSMearningstext <- paste("the median earnings of FSM graduates were <b>£",
          format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
          "  higher</b> than the median earnings of non-FSM graduates.",
          sep = ""
        )
      )

      textWidestEarnings <- funcRangeEarnings(
        tables_data_grouped %>% mutate(filter = FSM),
        suffix = " graduates"
      )

      crosstab_text <- paste0(
        "For first degree graduates of ", subjecttext, ", ", YAGtext, " after graduation, ",
        sectiontext, br(), br(),
        "The biggest difference in proportions is seen in <b>", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$abs),
        "</b> where ", FSMtext,
        textWidestEarnings,
        funcHighestEarnings(tables_data_grouped %>% mutate(filter = FSM)), br(), br()
      )
    }

    if (countinput == "ethnicity") {
      crosstabs_data <- tables_data_grouped %>%
        select(-earnings_median, n = count) %>%
        spread(ethnicity, n) %>%
        colorders(countinput) %>%
        arrange(-All) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
        mutate_if(is.numeric, funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
        mutate_at(
          c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
          funs(as.numeric(.))
        ) %>%
        select(SECTIONNAME, White, Black, Asian, Mixed, Other, `Not known`)

      crosstabs_earnings_data <- tables_data_grouped %>%
        select(-count, n = earnings_median) %>%
        spread(ethnicity, n) %>%
        colorders(countinput) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
        mutate_at(
          c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
          funs(as.numeric(.))
        ) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
        select(SECTIONNAME, White, Black, Asian, Mixed, Other, `Not known`)

      dfDataGrouped <- tables_data_grouped %>% rename(filter = ethnicity)
      ethnicityfirstdata <- c(
        topIndustries(dfDataGrouped, "White"), topIndustries(dfDataGrouped, "Black"),
        topIndustries(dfDataGrouped, "Asian"), topIndustries(dfDataGrouped, "Mixed"),
        topIndustries(dfDataGrouped, "Other"), topIndustries(dfDataGrouped, "Not known")
      )
      ethnicityfirstdata <- data.frame(ethnicityfirstdata)
      ethnicityfirstdata$ethnicity <- c("White", "Black", "Asian", "Mixed", "Other", "Not known")

      uniqueethnicity <- unique(ethnicityfirstdata$ethnicityfirstdata)

      textprod <- function(data) {
        if (nrow(data) != 1) {
          x <- paste(data$ethnicity[1:nrow(data) - 1], collapse = ", ")
          y <- paste(" and ", data$ethnicity[nrow(data)], sep = "")
          paste(x, y)
        } else if (nrow(data) == 1) {
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

        ethnicitytext <- paste("<b>", uniqueethnicity[1], "</b> was the most common industry for ", textprod(data1), " ethnicity graduates,
                      and <b>", uniqueethnicity[2], "</b> was the most common industry for ", textprod(data2), " ethnicity graduates.")
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
                      <b>", uniqueethnicity[2], "</b> was the most common industry for ", textprod(data2), " ethnicity graduates, and <b>",
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

      textWidestEarnings <- funcRangeEarnings(
        tables_data_grouped %>% mutate(filter = ethnicity),
        suffix = " ethnicity graduates"
      )
      textHighestEarnings <- funcHighestEarnings(
        tables_data_grouped %>% mutate(filter = ethnicity),
        suffix = " ethnicity graduates"
      )

      crosstab_text <- paste0(
        "For first degree graduates of ", subjecttext, ", ", YAGtext, " after graduation, ",
        ethnicitytext, br(), br(),
        "The industry with the largest range in proportions was <b>", first(biggestdiff$SECTIONNAME),
        "</b> where ", first(row.names(biggestdiff2)), " ethnicity graduates had the highest proportion and ",
        last(row.names(biggestdiff2)), " ethnicity graduates had the lowest proportion.",
        textWidestEarnings,
        textHighestEarnings, br(), br()
      )
    }

    if (countinput == "current_region") {
      crosstabs_data <- tables_data_grouped %>%
        select(-earnings_median, n = count) %>%
        spread(current_region, n) %>%
        colorders(countinput) %>%
        arrange(-All) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
        mutate_if(is.numeric, funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))) %>%
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
        colorders(countinput) %>%
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

      dfDataGrouped <- tables_data_grouped %>% rename(filter = current_region)
      regionfirstdata <- c(
        topIndustries(dfDataGrouped, "North East"),
        topIndustries(dfDataGrouped, "North West"),
        topIndustries(dfDataGrouped, "Yorkshire and the Humber"),
        topIndustries(dfDataGrouped, "East Midlands"),
        topIndustries(dfDataGrouped, "West Midlands"),
        topIndustries(dfDataGrouped, "East of England"),
        topIndustries(dfDataGrouped, "London"),
        topIndustries(dfDataGrouped, "South East"),
        topIndustries(dfDataGrouped, "South West")
      )
      regionfirstdata <- data.frame(regionfirstdata)
      regionfirstdata$region <- c(
        "the North East", "the North West", "Yorkshire and the Humber", "the East Midlands", "the West Midlands",
        " the East of England", "London", "the South East", "the South West"
      )

      uniqueregions <- unique(regionfirstdata$regionfirstdata)

      textprod <- function(data) {
        if (nrow(data) != 1) {
          x <- paste(data$region[1:nrow(data) - 1], collapse = ", ")
          y <- paste(" and ", data$region[nrow(data)], sep = "")
          paste(x, y)
        } else if (nrow(data) == 1) {
          paste(data$region[1])
        }
      }


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
        YAGtext, " after graduation, ", regiontext,
        br(),
        funcHighestEarnings(
          tables_data_grouped %>% mutate(filter = current_region),
          prefix = "graduates currently living in the ", suffix = ""
        )
      )
    }

    if (countinput == "prior_attainment") {
      crosstabs_data <- tables_data_grouped %>%
        select(-earnings_median, n = count) %>%
        spread(prior_attainment, n) %>%
        colorders(countinput) %>%
        arrange(-All) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
        mutate_if(is.numeric, funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))) %>%
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
        colorders(countinput) %>%
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
        colorders(countinput) %>%
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

      crosstab_text <- paste("For first degree graduates of ", subjecttext, ", ", YAGtext, " after graduation, the prior attainment band
                           with the highest number of graduates was `", first(grad_numbers$prior_attainment, order_by = -grad_numbers$grad_numbers), "`.
                           Within this prior attainment band, the most common industry was <b>",
        first(topindustry$SECTIONNAME, order_by = -topindustry[2]), "</b>, and the median earnings for graduates with this prior
                       attainment band working in this industry were <b>£", format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE),
        "</b>.", br(),
        funcHighestEarnings(
          tables_data_grouped %>%
            mutate(filter = case_when(
              prior_attainment == "1" ~ "4 As or more",
              prior_attainment == "2" ~ "360 points",
              prior_attainment == "3" ~ "300-359 points",
              prior_attainment == "4" ~ "240-299 points",
              prior_attainment == "5" ~ "180-239 points",
              prior_attainment == "6" ~ "Below 180 points",
              prior_attainment == "7" ~ "1 or 2 A level passes",
              prior_attainment == "8" ~ "BTEC",
              prior_attainment == "9" ~ "Other",
              TRUE ~ "Not known"
            )),
          prefix = "graduates in the ",
          suffix = " prior attainment band"
        ),
        br(), br(),
        sep = ""
      )
    }

    if (countinput == "subject_name") {
      crosstabs_earnings_data <- tables_data_grouped %>%
        select(-count, n = earnings_median) %>%
        spread(subject_name, n) %>%
        colorders(countinput) %>%
        arrange(-All) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
        select(-All)

      crosstabs_earnings_data2 <- crosstabs_earnings_data[, -1]
      crosstabs_earnings_data2 <- crosstabs_earnings_data2 %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .)))

      result <- which(crosstabs_earnings_data2 == max(crosstabs_earnings_data2), arr.ind = TRUE)

      crosstab_text <- paste0(
        funcHighestEarnings(tables_data_grouped %>% mutate(filter = subject_name),
          prefix = "graduates of ", suffix = "", fs = FALSE
        ),
        " when splitting by subject for ", tolower(qualinput), " graduates, ", YAGtext, " after graduation.",
        br(), br()
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
        colorders(countinput) %>%
        as.data.frame() %>%
        arrange(-`First degree`) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
        mutate_if(is.numeric, funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))) %>%
        mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

      crosstabs_earnings_data <- tables_data %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", prior_attainment == "All", group_name == "All"
        ) %>%
        group_by(qualification_TR, SECTIONNAME) %>%
        summarise(n = earnings_median) %>%
        spread(qualification_TR, n) %>%
        colorders(countinput) %>%
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
        if (nrow(data) != 1) {
          x <- paste(data$qual[1:nrow(data) - 1], collapse = ", ")
          y <- paste(" and ", data$qual[nrow(data)], sep = "")
          paste(x, y)
        } else if (nrow(data) == 1) {
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
                      and <b>", uniquequal[2], "</b> was the most common industry for ", textprod(data2), " graduates.")
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

      crosstab_text <- paste0(
        "For graduates of ", subjecttext, ", ", YAGtext, " after graduation, ", qualtext,
        funcHighestEarnings(tables_data_grouped %>% mutate(filter = qualification_TR)),
        br(), br()
      )
    }
  }
  return(crosstab_text)
}


# 2. Function to create the crosstabs data frame
# ==============================================
crosstabs <- function(tables_data_grouped, subjectinput, YAGinput, countinput, qualinput, buttoninput) {
  tables_data$SECTIONNAME[is.na(tables_data$SECTIONNAME) == TRUE] <- "Not known"
  tables_data$group_name[is.na(tables_data$group_name) == TRUE] <- "Not known"

  orange_pal <- function(x) {
    if (!is.na(x)) {
      rgb(colorRamp(c("#F7FBFF", "#317ABF"))(x), maxColorValue = 255)
    } else {
      "#e9e9e9" # grey
    }
  }

  # function which returns background colour based on cell value (using colour map)
  # also takes column name as an input, which allows to get max and min
  stylefunc <- function(value, index, name) {
    if (value >= 0 && !is.na(value)) {
      data <- crosstabs_data %>%
        mutate_if(
          is.numeric,
          funs(ifelse(. < 0, NA, .))
        )

      normalized <- (value - min(data %>%
        select(-SECTIONNAME), na.rm = T)) /
        (max(data %>%
          select(-SECTIONNAME), na.rm = T) - min(data %>%
          select(-SECTIONNAME), na.rm = T))
      color <- orange_pal(normalized)
      list(background = color)
    }
  }

  cellfunc <- function(value) {
    if (is.na(value)) {
      "x"
    } else if (value < 0) "c" else cellformat(value)
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
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))
      ) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`)

    # Ensure Not known is always at the bottom
    crosstabs_data_table <- crosstabs_data_table %>%
      filter(SECTIONNAME != "Not known") %>%
      full_join(crosstabs_data_table %>%
        filter(SECTIONNAME == "Not known"))

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(ethnicity, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(ethnicity, n) %>%
      colorders(countinput) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`) %>%
      ungroup()


    if (buttoninput == "Proportions") {
      cellformat <- function(value) {
        paste0(format(round(value * 100, 1), nsmall = 1), "%")
      }
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      cellformat <- function(value) {
        paste0("£", format(value, big.mark = ","))
      }
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      crosstabs_data <- crosstabs_data_table[, c(1, 2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(ethnicity, SECTIONNAME, group_name, n = count) %>%
      spread(ethnicity, n) %>%
      colorders(countinput) %>%
      as.data.frame() %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, White, Black, Asian, Mixed, Other, `Not known`)

    column_defs <- col_formats(crosstabs_data, footer_data, cellfunc)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    # Now create the nested tables data
    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(ethnicity) %>%
      mutate(prop = ifelse(count == 0, 0, count / sum(count, na.rm = TRUE)))

    nested_table <- tables_data_nested %>%
      select(ethnicity, SECTIONNAME, group_name, n = prop) %>%
      spread(ethnicity, n) %>%
      colorders(countinput)

    if ("All" %in% names(nested_table)) {
      nested_table <- nested_table %>%
        arrange(-All)
    }

    nested_table_earnings <- tables_data_nested %>%
      select(ethnicity, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(ethnicity, n) %>%
      colorders(countinput)


    if (buttoninput == "Proportions") {
      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      nested <- nested_table[, c(1, 2)] %>% left_join(nested_table_earnings, by = c("SECTIONNAME", "group_name"))
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
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))
      ) %>%
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

    # Ensure Not known is always at the bottom
    crosstabs_data_table <- crosstabs_data_table %>%
      filter(SECTIONNAME != "Not known") %>%
      full_join(crosstabs_data_table %>%
        filter(SECTIONNAME == "Not known"))

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(current_region, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(current_region, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
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
      cellformat <- function(value) {
        paste0(format(round(value * 100, 1), nsmall = 1), "%")
      }
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      cellformat <- function(value) {
        paste0("£", format(value, big.mark = ","))
      }
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      crosstabs_data <- crosstabs_data_table[, c(1, 2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(current_region, SECTIONNAME, group_name, n = count) %>%
      spread(current_region, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(
        SECTIONNAME, group_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    column_defs <- col_formats(crosstabs_data, footer_data, cellfunc)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(current_region) %>%
      mutate(prop = ifelse(count == 0, 0, count / sum(count, na.rm = TRUE)))

    if (buttoninput == "Proportions") {
      nested_table <- tables_data_nested %>%
        select(current_region, SECTIONNAME, group_name, n = prop) %>%
        spread(current_region, n) %>%
        colorders(countinput)

      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      nested_table_earnings <- crosstabs_basedata %>%
        filter(group_name != "All") %>%
        select(current_region, SECTIONNAME, group_name, n = earnings_median) %>%
        spread(current_region, n) %>%
        colorders(countinput)
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
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))
      ) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)

    # Ensure Not known is always at the bottom
    crosstabs_data_table <- crosstabs_data_table %>%
      filter(SECTIONNAME != "Not known") %>%
      full_join(crosstabs_data_table %>%
        filter(SECTIONNAME == "Not known"))

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(FSM, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(FSM, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)


    if (buttoninput == "Proportions") {
      cellformat <- function(value) {
        paste0(format(round(value * 100, 1), nsmall = 1), "%")
      }
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      cellformat <- function(value) {
        paste0("£", format(value, big.mark = ","))
      }
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      crosstabs_data <- crosstabs_data_table[, c(1, 2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(FSM, SECTIONNAME, group_name, n = count) %>%
      spread(FSM, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `non-FSM`, FSM, `Not known`)

    column_defs <- col_formats(crosstabs_data, footer_data, cellfunc)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", group_name != "All"
      ) %>%
      group_by(FSM) %>%
      mutate(prop = ifelse(count == 0, 0, count / sum(count, na.rm = TRUE)))

    nested_table <- tables_data_nested %>%
      filter(group_name != "All") %>%
      select(FSM, SECTIONNAME, group_name, n = prop) %>%
      spread(FSM, n) %>%
      colorders(countinput)

    if ("All" %in% names(nested_table)) {
      nested_table <- nested_table %>%
        arrange(-All)
    }

    nested_table_earnings <- tables_data_nested %>%
      filter(group_name != "All") %>%
      select(FSM, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(FSM, n) %>%
      colorders(countinput)

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
      colorders(countinput) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))
      ) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
    names(crosstabs_data_table) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

    # Ensure Not known is always at the bottom
    crosstabs_data_table <- crosstabs_data_table %>%
      filter(SECTIONNAME != "Not known") %>%
      full_join(crosstabs_data_table %>%
        filter(SECTIONNAME == "Not known"))

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(sex, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(sex, n) %>%
      colorders(countinput) %>%
      arrange(-`F+M`) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
    names(crosstabs_earnings_data) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

    if (buttoninput == "Proportions") {
      cellformat <- function(value) {
        paste0(format(round(value * 100, 1), nsmall = 1), "%")
      }
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      cellformat <- function(value) {
        paste0("£", format(value, big.mark = ","))
      }
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      crosstabs_data <- crosstabs_data_table[, c(1, 2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(sex, SECTIONNAME, group_name, n = count) %>%
      spread(sex, n) %>%
      colorders(countinput) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`) %>%
      as.data.frame()
    names(footer_data) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")

    column_defs <- col_formats(crosstabs_data, footer_data, cellfunc)
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
      colorders(countinput) %>%
      arrange(-`F+M`) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, group_name, `F`, `M`, `F+M`)
    names(nested_table) <- c("SECTIONNAME", "group_name", "Female", "Male", "Female & Male")
    nested_table_earnings <- table_group_proportions %>%
      select(sex, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(sex, n) %>%
      colorders(countinput) %>%
      arrange(-`F+M`) %>%
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
      nested <- nested_table[, c(1, 2)] %>% left_join(nested_table_earnings, by = c("SECTIONNAME", "group_name"))
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
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))
      ) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known") %>%
      rename(
        "4 As or more" = "1", "360 points" = "2", "300-359 points" = "3", "240-299 points" = "4",
        "180-239 points" = "5", "Below 180 points" = "6", "1 or 2 A level passes" = "7", "BTEC" = "8",
        "Other" = "9"
      )

    # Ensure Not known is always at the bottom
    crosstabs_data_table <- crosstabs_data_table %>%
      filter(SECTIONNAME != "Not known") %>%
      full_join(crosstabs_data_table %>%
        filter(SECTIONNAME == "Not known"))

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(prior_attainment, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(prior_attainment, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known") %>%
      rename(
        "4 As or more" = "1", "360 points" = "2", "300-359 points" = "3", "240-299 points" = "4",
        "180-239 points" = "5", "Below 180 points" = "6", "1 or 2 A level passes" = "7", "BTEC" = "8",
        "Other" = "9"
      )

    if (buttoninput == "Proportions") {
      cellformat <- function(value) {
        paste0(format(round(value * 100, 1), nsmall = 1), "%")
      }
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      cellformat <- function(value) {
        paste0("£", format(value, big.mark = ","))
      }
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      crosstabs_data <- crosstabs_data_table[, c(1, 2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(prior_attainment, SECTIONNAME, group_name, n = count) %>%
      spread(prior_attainment, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known") %>%
      rename(
        "4 As or more" = "1", "360 points" = "2", "300-359 points" = "3", "240-299 points" = "4",
        "180-239 points" = "5", "Below 180 points" = "6", "1 or 2 A level passes" = "7", "BTEC" = "8",
        "Other" = "9"
      )

    column_defs <- col_formats(crosstabs_data, footer_data, cellfunc)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(prior_attainment) %>%
      mutate(prop = ifelse(count == 0, 0, count / sum(count, na.rm = TRUE)))

    nested_table <- tables_data_nested %>%
      select(prior_attainment, SECTIONNAME, group_name, n = prop) %>%
      spread(prior_attainment, n) %>%
      colorders(countinput)
    nested_table_earnings <- tables_data_nested %>%
      select(prior_attainment, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(prior_attainment, n)

    if (buttoninput == "Proportions") {
      nested <- nested_table
    } else if (buttoninput == "Median earnings") {
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      nested <- nested_table[, c(1, 2)] %>% left_join(nested_table_earnings)
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
      select(SECTIONNAME, group_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known") %>%
      rename(
        "4 As or more" = "1", "360 points" = "2", "300-359 points" = "3", "240-299 points" = "4",
        "180-239 points" = "5", "Below 180 points" = "6", "1 or 2 A level passes" = "7", "BTEC" = "8",
        "Other" = "9"
      )
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
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))
      ) %>%
      select(-All)

    # Ensure Not known is always at the bottom
    crosstabs_data_table <- crosstabs_data_table %>%
      filter(SECTIONNAME != "Not known") %>%
      full_join(crosstabs_data_table %>%
        filter(SECTIONNAME == "Not known"))

    crosstabs_earnings_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(subject_name, SECTIONNAME, group_name, n = earnings_median) %>%
      spread(subject_name, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(-All)
    if (buttoninput == "Proportions") {
      cellformat <- function(value) {
        paste0(format(round(value * 100, 1), nsmall = 1), "%")
      }
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      cellformat <- function(value) {
        paste0("£", format(value, big.mark = ","))
      }
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      crosstabs_data <- crosstabs_data_table[, c(1, 2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- crosstabs_basedata %>%
      filter(group_name == "All") %>%
      select(subject_name, SECTIONNAME, group_name, n = count) %>%
      spread(subject_name, n) %>%
      colorders(countinput) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(-All)

    column_defs <- col_formats(crosstabs_data, footer_data, cellfunc, minWidth = 320)
    numeric_cols_def <- column_defs$numeric_cols_def
    numeric_cols_def_nested <- column_defs$numeric_cols_def_nested
    script <- column_defs$script

    tables_data_nested <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(subject_name) %>%
      mutate(prop = ifelse(count == 0, 0, count / sum(count, na.rm = TRUE)))

    nested_table <- tables_data_nested %>%
      group_by(subject_name, SECTIONNAME, group_name) %>%
      summarise(n = prop) %>%
      spread(subject_name, n) %>%
      colorders(countinput)

    if ("All" %in% names(nested_table)) {
      nested_table <- nested_table %>%
        arrange(-All)
    }

    nested_table_earnings <- crosstabs_basedata %>%
      filter(group_name != "All") %>%
      group_by(subject_name, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(subject_name, n) %>%
      colorders(countinput)

    if (buttoninput == "Proportions") {
      nested <- nested_table %>%
        select(-All)
    } else if (buttoninput == "Median earnings") {
      nested <- nested_table_earnings %>%
        select(-All)
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
      colorders(countinput) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      ungroup() %>%
      mutate_if(
        is.numeric,
        funs(ifelse(. == 0, 0, . / sum(., na.rm = TRUE)))
      ) %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)

    # Ensure Not known is always at the bottom
    crosstabs_data_table <- crosstabs_data_table %>%
      filter(SECTIONNAME != "Not known") %>%
      full_join(crosstabs_data_table %>%
        filter(SECTIONNAME == "Not known"))


    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      group_by(qualification_TR, SECTIONNAME, group_name) %>%
      summarise(n = earnings_median) %>%
      spread(qualification_TR, n) %>%
      colorders(countinput) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)
    if (buttoninput == "Proportions") {
      cellformat <- function(value) {
        paste0(format(round(value * 100, 1), nsmall = 1), "%")
      }
      crosstabs_data <- crosstabs_data_table
    } else if (buttoninput == "Median earnings") {
      cellformat <- function(value) {
        paste0("£", format(value, big.mark = ","))
      }
      # Note the left_join here is intended to make sure the proportions table is initially ordered identically to the proportions table.
      crosstabs_data <- crosstabs_data_table[, c(1, 2)] %>% left_join(crosstabs_earnings_data, by = c("SECTIONNAME", "group_name"))
    }

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", group_name == "All"
      ) %>%
      select(qualification_TR, SECTIONNAME, group_name, n = count) %>%
      spread(qualification_TR, n) %>%
      colorders(countinput) %>%
      as.data.frame() %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, group_name, `First degree`, `Level 7 (taught)`, `Level 7 (research)`, `Level 8`)


    column_defs <- col_formats(crosstabs_data, footer_data, cellfunc)
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
  if (buttoninput == "Proportions") {
    crosstabs_data <- crosstabs_data %>% mutate_if(is.numeric, funs(round(., digits = 3)))
    nested <- nested %>% mutate_if(is.numeric, funs(round(., digits = 3)))
  }
  return(list(
    crosstabs_data = crosstabs_data,
    footer_crosstabs = footer_data,
    nested_crosstabs = nested,
    numeric_cols_def = numeric_cols_def,
    nested_numeric_cols_def = numeric_cols_def_nested,
    script = script
  ))
}

crosstabs_reactable <- function(crosstabs_data, nested, numeric_cols_def, numeric_cols_def_nested, script) {
  coldefs <- list(
    SECTIONNAME = colDef(na = "x", name = "Industry", width = 500, footer = "TOTAL (N)"),
    group_name = colDef(na = "x", name = "3 digit SIC code", width = 300)
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
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold")), height = 600
  )

  return(crosstab)
}

crosstab_title <- function(subjectinput, YAGinput, countinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )

  if (YAGinput == 1) {
    YAGtext <- "one year"
  } else if (YAGinput == 3) {
    YAGtext <- "three years"
  } else if (YAGinput == 5) {
    YAGtext <- "five years"
  } else if (YAGinput == 10) {
    YAGtext <- "ten years"
  }

  if (countinput == "FSM") {
    counttext <- "FSM status"
  } else if (countinput == "prior_attainment") {
    counttext <- "prior attainment"
  } else if (countinput == "current_region") {
    counttext <- "current region"
  } else if (countinput == "ethnicity") {
    counttext <- "ethnicity"
  }


  if (countinput %in% c("FSM", "prior_attainment")) {
    crosstab_title <- paste("<h4>Industry of graduate employment for graduates of ", subjecttext, " by ", counttext, ", ", YAGtext, " after
                          graduation, young (under 21 at start of course) male and female first degree
                          graduates from English HEIs, APs and FECs, 2018/19 tax year.</h4>",
      sep = ""
    )
  }

  if (countinput %in% c("sex")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates of ", subjecttext, " by ", countinput, ", ", YAGtext, " after
                          graduation, male and female ", tolower(qualinput), " graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>",
      sep = ""
    )
  }

  if (countinput %in% c("qualification_TR")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates of ", subjecttext, " by qualification, ", YAGtext, " after
                          graduation, male and female graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>",
      sep = ""
    )
  }

  if (countinput %in% c("current_region", "ethnicity")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates of ", subjecttext, " by ", counttext, ", ", YAGtext, " after
                          graduation, male and female first degree graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>",
      sep = ""
    )
  }

  if (countinput %in% c("subject_name")) {
    crosstab_title <- paste("<h4> Industry of graduate employment for graduates by subject, ", YAGtext, " after
                          graduation, male and female ", tolower(qualinput), " graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>",
      sep = ""
    )
  }

  return(crosstab_title)
}
