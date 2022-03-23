backwards_crosstab_title <- function(sectioninput, YAGinput, countinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )



  if (countinput %in% c("FSM", "prior_attainment")) {
    crosstab_title <- paste("<h4>Graduates working in the ", sectioninput, " industry ", YAGinput, " years after
                            graduation by the subject they studied and ", countinput, ", young (under 21 at start of course)
                            male and female first degree graduates from English HEIs, APs and FECs, 2018/19 tax year.</h4>")
  }

  if (countinput %in% c("sex")) {
    crosstab_title <- paste("<h4>Graduates working in the ", sectioninput, " industry ", YAGinput, " years after
                            graduation by the subject they studied and ", countinput, ", ", qualinput, " graduates from English HEIs,
                            APs and FECs, 2018/19 tax year.</h4>")
  }

  if (countinput %in% c("qualification_TR")) {
    crosstab_title <- paste("<h4>Graduates working in the ", sectioninput, " industry ", YAGinput, " years after
                            graduation by the subject they studied and qualification level, male and female graduates from English HEIs,
                            APs and FECs, 2018/19 tax year.</h4>")
  }

  if (countinput %in% c("current_region", "ethnicity")) {
    crosstab_title <- paste("<h4>Graduates working in the ", sectioninput, " industry ", YAGinput, " years after
                            graduation by the subject they studied and ", countinput, ", ", "male and female first degree graduates from
                            English HEIs, APs and FECs, 2018/19 tax year.</h4>")
  }

  if (countinput %in% c("SECTIONNAME")) {
    crosstab_title <- paste("<h4>Graduates working in each industry by subject studied, ", YAGinput, " years after
                          graduation, male and female ", qualinput, " graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>")
  }

  return(crosstab_title)
}

crosstab_text <- function(subjectinput, YAGinput, countinput, qualinput, thresholdinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )

  if (countinput == "sex") {
    crosstabs_data <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(sex, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
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

    crosstabs_earnings_data <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(sex, SECTIONNAME) %>%
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
      sectiontext <- paste("graduates is the same for both female and male graduates (", first(crosstabs_data$SECTIONNAME), "). The median
                                earnings for females in this industry  were £", top_industry$Female, " and for males were £",
        top_industry$Male, ".",
        sep = ""
      ),
      sectiontext <- paste("female graduates is ", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Female), ", and the
                                median earnings of females in this industry were £", top_industry_female$Female, ". The industry
                                with the highest proportion of male graduates was ",
        first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$Male), " and the median earnings of males
                                in this industry were £", top_industry_male$Male,
        sep = ""
      )
    )

    ifelse(first(crosstabs_data$diff, order_by = -crosstabs_data$abs) > 0,
      sextext <- paste(
        "the proportion of male graduates is ", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        " percentage points higher than the proportion of female graduates."
      ),
      sextext <- paste(
        "the proportion of female graduates is ", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        " percentage points higher than the proportion of male graduates."
      )
    )

    ifelse(first(crosstabs_earnings_data$diff, order_by = -crosstabs_earnings_data$abs) > 0,
      sextextearnings <- paste("the median earnings of male graduates were £",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher than the medain earnings of female graduates.",
        sep = ""
      ),
      sextextearnings <- paste("the median earnings of female graduates were £",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher than the median earnings of male graduates.",
        sep = ""
      )
    )

    ifelse(abs(round(sum(crosstabs_data$Female[1:2]) * 100, digits = 1) - round(sum(crosstabs_data$Male[1:2] * 100), digits = 1)) > 5,
      sextext2 <- paste(round(sum(crosstabs_data$Female[1:2]) * 100, digits = 1), "% of female graduates are concentrated in the top 2
                           industries (either ", first(crosstabs_data$SECTIONNAME), " or ", crosstabs_data$SECTIONNAME[2], "),
                           whereas for male graduates this is ", round(sum(crosstabs_data$Male[1:2] * 100), digits = 1), "%.", sep = ""),
      sextext2 <- paste("")
    )

    ifelse(first(crosstabs_earnings_data$Male, order_by = -crosstabs_earnings_data$Male) > first(crosstabs_earnings_data$Female, order_by = -crosstabs_earnings_data$Female),
      sextextearnings2 <- paste("The group with the highest earnings was male graduates in the ",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$Male), " industry
           (median earnings of £", format(first(crosstabs_earnings_data$Male, order_by = -crosstabs_earnings_data$Male), big.mark = ",", scientific = FALSE), ").",
        sep = ""
      ),
      sextextearnings2 <- paste("The group with the highest earnings was female graduates in the ",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$Female),
        "industry (median earnings of £", format(first(crosstabs_earnings_data$Female, order_by = -crosstabs_earnings_data$Female), big.mark = ",", scientific = FALSE),
        ").",
        sep = ""
      )
    )

    crosstab_text <- paste("For ", qualinput, " graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ",
      "the industry with the highest proportion of ", sectiontext, br(), br(),
      "The biggest difference in proportions is seen in ", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$abs),
      " where ", sextext, br(), br(),
      "The biggest difference in median earnings is seen in ", first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$abs),
      " where ", sextextearnings, br(), br(),
      sextextearnings2, br(), br(),
      sextext2,
      sep = ""
    )
  }

  if (countinput == "FSM") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(FSM, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
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

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(FSM, SECTIONNAME) %>%
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
      sectiontext <- paste("graduates was the same for both non-FSM and FSM graduates (", first(crosstabs_data$SECTIONNAME), "), where median
                                earnings for non-FSM graduates were £", top_industry$`non-FSM`, " and for FSM graduates were £", top_industry$FSM, ".",
        sep = ""
      ),
      sectiontext <- paste("non-FSM graduates was", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$`non-FSM`), " and the median
                                earnings of non-FSM graduates in this industry were £", top_industry_nonfsm$`non-FSM`, ". The
                                industry with the highest proportion of FSM graduates was ",
        first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$FSM), " and the median earnings of FSM
                                graduates in this industry were £", top_industry_FSM$FSM, ".",
        sep = ""
      )
    )

    ifelse(first(crosstabs_data$diff, order_by = -crosstabs_data$abs) > 0,
      FSMtext <- paste(
        "the proportion of non-FSM graduates is", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        "percentage points higher than the proportion of FSM graduates."
      ),
      FSMtext <- paste(
        "the proportion of FSM graduates is", round(first(crosstabs_data$abs, order_by = -crosstabs_data$abs) * 100, digits = 1),
        "percentage points higher than the proportion of non-FSM graduates."
      )
    )

    ifelse(first(crosstabs_earnings_data$diff, order_by = -crosstabs_earnings_data$abs) > 0,
      FSMearningstext <- paste("the median earnings of non-FSM graduates were £",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher than the medain earnings of FSM graduates.",
        sep = ""
      ),
      FSMearningstext <- paste("the median earnings of FSM graduates were £",
        format(first(crosstabs_earnings_data$abs, order_by = -crosstabs_earnings_data$abs), big.mark = ",", scientific = FALSE),
        "  higher than the median earnings of non-FSM graduates.",
        sep = ""
      )
    )

    ifelse(first(crosstabs_earnings_data$`non-FSM`, order_by = -crosstabs_earnings_data$`non-FSM`) > first(crosstabs_earnings_data$FSM, order_by = -crosstabs_earnings_data$FSM),
      FSMearningstext2 <- paste("The group with the highest earnings was non-FSM graduates in the ",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$`non-FSM`), "
                                     industry (median earnings of £",
        format(first(crosstabs_earnings_data$`non-FSM`, order_by = -crosstabs_earnings_data$`non-FSM`), big.mark = ",", scientific = FALSE), ").",
        sep = ""
      ),
      FSMearningstext2 <- paste("The group with the highest earnings was FSM graduates in the ",
        first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$FSM), "
                                     industry (median earnings of £",
        format(first(crosstabs_earnings_data$FSM, order_by = -crosstabs_earnings_data$FSM), big.mark = ",", scientific = FALSE), ").",
        sep = ""
      )
    )

    crosstab_text <- paste("For first degree graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ",
      "the industry with the highest proportion of ", sectiontext, br(), br(),
      "The biggest difference in proportions is seen in ", first(crosstabs_data$SECTIONNAME, order_by = -crosstabs_data$abs),
      " where ", FSMtext, br(), br(),
      "The biggest difference in median earnings was seen in ", first(crosstabs_earnings_data$SECTIONNAME, order_by = -crosstabs_earnings_data$abs),
      " where ", FSMearningstext, br(), br(),
      FSMearningstext2,
      sep = ""
    )
  }

  if (countinput == "ethnicity") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(ethnicity, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
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

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(ethnicity, SECTIONNAME) %>%
      summarise(n = earnings_median) %>%
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
      ethnicitytext <- paste(uniqueethnicity, " is the most common industry for all ethnicities.")
    } else if (length(uniqueethnicity) == 2) {
      data1 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[1])
      data2 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[2])

      ethnicitytext <- paste(uniqueregions[1], " was the most common industry for ", textprod(data1), " ethnicity graduates,
                      and ", uniqueregions[2], " was the most common industry for ", textprod(data2), " ethnicity graduates.")
    } else if (length(uniqueethnicity) == 3) {
      data1 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[1])
      data2 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[2])
      data3 <- ethnicityfirstdata %>%
        filter(ethnicityfirstdata == uniqueethnicity[3])

      ethnicitytext <- paste(
        uniqueethnicity[1], " was the most common industry for ", textprod(data1), " ethnicity graduates,
                      ", uniqueethnicity[2], " was the most common industry for ", textprod(data2), " ethnicity graduates, ,and ",
        uniqueethnicity[3], " was the most common industry for ", textprod(data3), " ethnicity graduates."
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
        uniqueethnicity[1], " was the most common industry for ", textprod(data1), " ethnicity graduates,
                      ", uniqueethnicity[2], " was the most common industry for ", textprod(data2), " ethnicity graduates, ",
        uniqueethnicity[3], " was the most common industry for ", textprod(data3), " ethnicity graduates, and ",
        uniqueethnicity[4], " was the most common industry for ", textprod(data4), " ethnicity graduates."
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
        uniqueethnicity[1], " was the most common industry for ", textprod(data1), " ethnicity graduates,
                      ", uniqueethnicity[2], " was the most common industry for ", textprod(data2), " ethnicity graduates, ",
        uniqueethnicity[3], " was the most common industry for ", textprod(data3), " ethnicity graduates, ",
        uniqueethnicity[4], " was the most common industry for ", textprod(data4), " ethnicity graduates, and ",
        uniqueethnicity[5], " was the most common industry for ", textprod(data5), " ethnicity graduates."
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
        uniqueethnicity[1], " was the most common industry for ", textprod(data1), " ethnicity graduates,
                      ", uniqueethnicity[2], " was the most common industry for ", textprod(data2), " ethnicity graduates, ",
        uniqueethnicity[3], " was the most common industry for ", textprod(data3), " ethnicity graduates, ",
        uniqueethnicity[4], " was the most common industry for ", textprod(data4), " ethnicity graduates, ",
        uniqueethnicity[5], " was the most common industry for ", textprod(data5), " ethnicity graduates, and ",
        uniqueethnicity[6], " was the most common industry for ", textprod(data6), " ethnicity graduates."
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
      br(), br(), "The industry with the largest range in proportions was ", first(biggestdiff$SECTIONNAME), "
                           where ", first(row.names(biggestdiff2)), " ethnicity graduates had the highest proportion and ", last(row.names(biggestdiff2)), "
                           ethnicity graduates had the lowest proportion.", br(), br(),
      "The industry with the largest range in median earnings was ", first(biggestdiffearnings$SECTIONNAME), "
                           where ", first(row.names(biggestdiffearnings2)), " ethnicity graduates the highest median earnings (£",
      format(first(biggestdiffearnings2$.), big.mark = ",", scientific = FALSE), ") and ", last(row.names(biggestdiffearnings2)),
      " ethnicity graduates had the lowest median earnings (£", format(last(biggestdiffearnings2$.), big.mark = ",", scientific = FALSE), ").", br(), br(),
      "The group with the highest median earnings was ", colnames(crosstabs_earnings_data2[, result[2]]), " ethnicity graduates in
                           the ", crosstabs_earnings_data[result[1], ]$SECTIONNAME, " industry (median earnings of £",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), ").",
      sep = ""
    )
  }

  if (countinput == "current_region") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(current_region, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
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

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(current_region, SECTIONNAME) %>%
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

    if (length(uniqueregions) == 1) {
      regiontext <- paste(uniqueregions, " is the most common industry for all current regions.")
    } else if (length(uniqueregions) == 2) {
      data1 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[1])
      data2 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[2])

      regiontext <- paste(uniqueregions[1], " was the most common industry for those currently living in ", textprod(data1), ",
                      and ", uniqueregions[2], " was the most common industry for those living in ", textprod(data2), ".")
    } else if (length(uniqueregions) == 3) {
      data1 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[1])
      data2 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[2])
      data3 <- regionfirstdata %>%
        filter(regionfirstdata == uniqueregions[3])

      regiontext <- paste(
        uniqueregions[1], " was the most common industry for those currently living in ", textprod(data1), ",
                      ", uniqueregions[2], " was the most common industry for those living in ", textprod(data2), " and ",
        uniqueregions[3], " was the most common industry for those living in ", textprod(data3), "."
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
        uniqueregions[1], " was the most common industry for those currently living in ", textprod(data1), ",
                      ", uniqueregions[2], " was the most common industry for those living in ", textprod(data2), ", ",
        uniqueregions[3], " was the most common industry for those living in ", textprod(data3), " and ",
        uniqueregions[4], " was the most common industry for those living in ", textprod(data4), "."
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
        uniqueregions[1], " was the most common industry for those currently living in ", textprod(data1), ",
                      ", uniqueregions[2], " was the most common industry for those living in ", textprod(data2), ", ",
        uniqueregions[3], " was the most common industry for those living in ", textprod(data3), ", ",
        uniqueregions[4], " was the most common industry for those living in ", textprod(data4), " and ",
        uniqueregions[5], " was the most common industry for those living in ", textprod(data5), "."
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
        uniqueregions[1], " was the most common industry for those currently living in ", textprod(data1), ",
                      ", uniqueregions[2], " was the most common industry for those living in ", textprod(data2), ", ",
        uniqueregions[3], " was the most common industry for those living in ", textprod(data3), ", ",
        uniqueregions[4], " was the most common industry for those living in ", textprod(data4), ", ",
        uniqueregions[5], " was the most common industry for those living in ", textprod(data5), " and ",
        uniqueregions[6], " was the most common industry for those living in ", textprod(data6), "."
      )
    }

    crosstabs_earnings_data2 <- crosstabs_earnings_data[, -1]
    crosstabs_earnings_data2 <- crosstabs_earnings_data2 %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .)))

    result <- which(crosstabs_earnings_data2 == max(crosstabs_earnings_data2), arr.ind = TRUE)

    crosstab_text <- paste("For first degree graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ", regiontext, br(), br(),
      "The group with the highest earnings was graduates currently living in ",
      colnames(crosstabs_earnings_data2[, result[2]]), " working in the ",
      crosstabs_earnings_data[result[1], ]$SECTIONNAME, " industry (median earnings of £",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), ").",
      sep = ""
    )
  }

  if (countinput == "prior_attainment") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
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

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME) %>%
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
      select(SECTIONNAME, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
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
                           Within this prior attainment band, the most common industry was ",
      first(topindustry$SECTIONNAME, order_by = -topindustry[2]), ", and the median earnings for graduates with this prior
                       attainment band working in this industry were £", format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE),
      ".", br(), br(),
      "The group with the highest median earnings was graduates in the ", colnames(crosstabs_earnings_data3[result[2]]),
      " prior attainment band who worked in the ", crosstabs_earnings_data[result[1], ]$SECTIONNAME, " industry (median
                           earnings of £", format(max(crosstabs_earnings_data3), big.mark = ",", scientific = FALSE), ").",
      sep = ""
    )
  }

  if (countinput == "subject_name") {
    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(subject_name, SECTIONNAME) %>%
      summarise(n = earnings_median) %>%
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
                           the highest earning group was graduates of ", colnames(crosstabs_earnings_data2[, result[2]]), " who
                           worked in the ", crosstabs_earnings_data[result[1], ]$SECTIONNAME, " industry (median earnings of £",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), ").",
      sep = ""
    )
  }

  if (countinput == "qualification_TR") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(qualification_TR, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name == "All"
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
      qualtext <- paste(uniquequal, " is the most common industry for all qualification levels.")
    } else if (length(uniquequal) == 2) {
      data1 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[1])
      data2 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[2])

      qualtext <- paste(uniquequal[1], " was the most common industry for ", textprod(data1), " graduates,
                      and ", uniquequal[2], " was the most common industry for ", textprod(data2), " graduates.")
    } else if (length(uniquequal) == 3) {
      data1 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[1])
      data2 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[2])
      data3 <- qualfirstdata %>%
        filter(qualfirstdata == uniquequal[3])

      qualtext <- paste(
        uniquequal[1], " was the most common industry for ", textprod(data1), " graduates,
                      ", uniquequal[2], " was the most common industry for ", textprod(data2), " graduates, and ",
        uniquequal[3], " was the most common industry for ", textprod(data3), " graduates."
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
        uniquequal[1], " was the most common industry for ", textprod(data1), " graduates,
                      ", uniquequal[2], " was the most common industry for ", textprod(data2), " graduates, ",
        uniquequal[3], " was the most common industry for ", textprod(data3), " graduates, and ",
        uniquequal[4], " was the most common industry for ", textprod(data4), " graduates."
      )
    }

    crosstabs_earnings_data2 <- crosstabs_earnings_data[, -1]
    crosstabs_earnings_data2 <- crosstabs_earnings_data2 %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .)))

    result <- which(crosstabs_earnings_data2 == max(crosstabs_earnings_data2), arr.ind = TRUE)


    crosstab_text <- paste("For graduates of ", subjecttext, ", ", YAGinput, " years after graduation, ", qualtext, br(), br(),
      "The highest earning group was ", colnames(crosstabs_earnings_data2[, result[2]]), " graduates
                           working in the ", crosstabs_data[result[1], ]$SECTIONNAME, " industry (median earnings of £",
      format(max(crosstabs_earnings_data2), big.mark = ",", scientific = FALSE), ").",
      sep = ""
    )
  }

  return(crosstab_text)
}


downloadcrosstabs <- function(subjectinput, YAGinput, countinput, qualinput, thresholdinput) {
  tables_data$SECTIONNAME[tables_data$SECTIONNAME == ""] <- "NOT KNOWN"

  if (countinput == "ethnicity") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(ethnicity, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(ethnicity, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(round((. / sum(.)) * 100, digits = 1))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, White, Black, Asian, Mixed, Other, `Not known`)

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(ethnicity, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(ethnicity, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, White, Black, Asian, Mixed, Other, `Not known`)

    footers <- c(
      "TOTAL (N)", format(round_any(sum(footer_data$White), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data$Black), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data$Asian), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data$Mixed), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data$Other), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data$`Not known`), 5), big.mark = ",", scientific = FALSE)
    )

    crosstabs_data2 <- rbind(crosstabs_data, footers)
  }

  if (countinput == "current_region") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(current_region, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(round((. / sum(.)) * 100, digits = 1))) %>%
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

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(current_region, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(
        SECTIONNAME, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    footers <- c(
      "TOTAL (N)", format(round_any(sum(footer_data[2]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[3]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[4]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[5]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[6]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[7]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[8]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[9]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[10]), 5), big.mark = ",", scientific = FALSE)
    )

    crosstabs_data2 <- rbind(crosstabs_data, footers)
  }
  if (countinput == "FSM") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(FSM, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(round((. / sum(.)) * 100, digits = 1))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, `non-FSM`, FSM, `Not known`)

    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(FSM, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, `non-FSM`, FSM, `Not known`)

    footers <- c(
      "TOTAL (N)", format(round_any(sum(footer_data[2]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[3]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[4]), 5), big.mark = ",", scientific = FALSE)
    )

    crosstabs_data2 <- rbind(crosstabs_data, footers)
  }

  if (countinput == "sex") {
    crosstabs_data <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(sex, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(round((. / sum(.)) * 100, digits = 1))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      select(SECTIONNAME, `F`, `M`, `F+M`)
    names(crosstabs_data) <- c("SECTIONNAME", "Female", "Male", "Female & Male")

    footer_data <- tables_data %>%
      filter(
        subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(sex, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(SECTIONNAME, `F`, `M`, `F+M`)
    names(footer_data) <- c("SECTIONNAME", "Female", "Male", "Female & Male")

    footers <- c(
      "TOTAL (N)", format(round_any(sum(footer_data[2]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[3]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[4]), 5), big.mark = ",", scientific = FALSE)
    )

    crosstabs_data2 <- rbind(crosstabs_data, footers)
  }

  if (countinput == "prior_attainment") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(round((. / sum(.)) * 100, digits = 1))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")


    footer_data <- tables_data %>%
      filter(
        sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(prior_attainment, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(SECTIONNAME, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    footers <- c(
      "TOTAL (N)", format(round_any(sum(footer_data[2]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[3]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[4]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[5]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[6]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[7]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[8]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[9]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[10]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[11]), 5), big.mark = ",", scientific = FALSE),
      format(round_any(sum(footer_data[12]), 5), big.mark = ",", scientific = FALSE)
    )

    crosstabs_data2 <- rbind(crosstabs_data, footers)
  }

  if (countinput == "subject_name") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(subject_name, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(subject_name, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(round((. / sum(.)) * 100, digits = 1))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      select(-All)


    footer_data <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
      ) %>%
      group_by(subject_name, SECTIONNAME) %>%
      summarise(n = sum(count)) %>%
      spread(subject_name, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .)))

    footers <- c()
    footers[1] <- "TOTAL (N)"

    for (i in 2:37) {
      footers[i] <- format(round_any(sum(footer_data[i]), 5), big.mark = ",", scientific = FALSE)
    }

    crosstabs_data2 <- rbind(crosstabs_data, footers)
  }

  return(crosstabs_data2)
}


backwards_crosstabs <- function(sectioninput, YAGinput, countinput, qualinput, buttoninput) {

  # tables_data <- tables_data %>%
  #   filter(subject_name != 'All')

  tables_data$SECTIONNAME[is.na(tables_data$SECTIONNAME) == TRUE] <- "NOT KNOWN"

  tables_data <- tables_data %>%
    filter(group_name == "All")

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
      select(-subject_name), na.rm = T)) /
      (max(crosstabs_data %>%
        select(-subject_name), na.rm = T) - min(crosstabs_data %>%
        select(-subject_name), na.rm = T))
    color <- orange_pal(normalized)
    list(background = color)
  }

  footerfunc <- function(value, index, name) {
    footer <- format(round_any(sum(footer_data[name]), 5), big.mark = ",", scientific = FALSE, na.m = T)
    return(footer)
  }

  colformat <- colFormat(percent = TRUE, digits = 1)


  if (countinput == "ethnicity") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(ethnicity, subject_name) %>%
      summarise(n = sum(count)) %>%
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
      select(subject_name, White, Black, Asian, Mixed, Other, `Not known`)

    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(ethnicity, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(ethnicity, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(subject_name, White, Black, Asian, Mixed, Other, `Not known`)

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_data
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- footerdata %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", threshold == "All"
      ) %>%
      group_by(ethnicity, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(ethnicity, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(subject_name, White, Black, Asian, Mixed, Other, `Not known`)

    coldefs <- list(
      subject_name = colDef(name = "Subject area", width = 600, footer = "TOTAL (N)"),
      White = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$White), 5), big.mark = ",", scientific = FALSE)),
      Black = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$Black), 5), big.mark = ",", scientific = FALSE)),
      Asian = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$Asian), 5), big.mark = ",", scientific = FALSE)),
      Mixed = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$Mixed), 5), big.mark = ",", scientific = FALSE)),
      Other = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$Other), 5), big.mark = ",", scientific = FALSE)),
      `Not known` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Not known`), 5), big.mark = ",", scientific = FALSE))
    )
  }

  if (countinput == "current_region") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(current_region, subject_name) %>%
      summarise(n = sum(count)) %>%
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
        subject_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )


    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(current_region, subject_name) %>%
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
        subject_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)

    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_data
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- footerdata %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(current_region, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(
        subject_name, `North East`, `North West`, `Yorkshire and the Humber`, `East Midlands`, `West Midlands`,
        `East of England`, `London`, `South East`, `South West`
      )

    coldefs <- list(
      subject_name = colDef(
        na = "c", name = "Subject area", width = 600, footer = "TOTAL (N)",
        style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
        headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
        footerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, fontWeight = "bold")
      ),
      `North East` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`North East`), 5), big.mark = ",", scientific = FALSE)),
      `North West` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`North West`), 5), big.mark = ",", scientific = FALSE)),
      `Yorkshire and the Humber` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Yorkshire and the Humber`), 5), big.mark = ",", scientific = FALSE)),
      `East Midlands` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`East Midlands`), 5), big.mark = ",", scientific = FALSE)),
      `West Midlands` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`West Midlands`), 5), big.mark = ",", scientific = FALSE)),
      `East of England` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`East of England`), 5), big.mark = ",", scientific = FALSE)),
      `London` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`London`), 5), big.mark = ",", scientific = FALSE)),
      `South East` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`South East`), 5), big.mark = ",", scientific = FALSE)),
      `South West` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`South West`), 5), big.mark = ",", scientific = FALSE))
    )
  }

  if (countinput == "FSM") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(FSM, subject_name) %>%
      summarise(n = sum(count)) %>%
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
      select(subject_name, `non-FSM`, FSM, `Not known`)


    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(FSM, subject_name) %>%
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
      select(subject_name, `non-FSM`, FSM, `Not known`)

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)

    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_data
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- footerdata %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(FSM, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(subject_name, `non-FSM`, FSM, `Not known`)


    coldefs <- list(
      subject_name = colDef(na = "c", name = "Subject area", width = 600, footer = "TOTAL (N)"),
      `non-FSM` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`non-FSM`), 5), big.mark = ",", scientific = FALSE)),
      FSM = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$FSM), 5), big.mark = ",", scientific = FALSE)),
      `Not known` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Not known`), 5), big.mark = ",", scientific = FALSE))
    )
  }

  if (countinput == "sex") {
    crosstabs_data <- tables_data %>%
      filter(
        SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, subject_name != "All", threshold == "All"
      ) %>%
      group_by(sex, subject_name) %>%
      summarise(n = sum(count)) %>%
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
      select(subject_name, `F`, `M`, `F+M`)
    names(crosstabs_data) <- c("subject_name", "Female", "Male", "Female & Male")



    crosstabs_earnings_data <- tables_data %>%
      filter(
        SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, subject_name != "All", threshold == "All"
      ) %>%
      group_by(sex, subject_name) %>%
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
      select(subject_name, `F`, `M`, `F+M`)
    names(crosstabs_earnings_data) <- c("subject_name", "Female", "Male", "Female & Male")


    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)
    names(crosstabs_earnings_data2) <- c("subject_name", "Female", "Male", "Female & Male")

    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_data
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- footerdata %>%
      filter(
        SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, subject_name != "All", threshold == "All"
      ) %>%
      group_by(sex, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      select(subject_name, `F`, `M`, `F+M`)
    names(footer_data) <- c("subject_name", "Female", "Male", "Female & Male")

    coldefs <- list(
      subject_name = colDef(na = "c", name = "Subject area", width = 600, footer = "TOTAL (N)"),
      Female = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$Female), 5), big.mark = ",", scientific = FALSE)),
      Male = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$Male), 5), big.mark = ",", scientific = FALSE)),
      `Female & Male` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Female & Male`), 5), big.mark = ",", scientific = FALSE))
    )
  }

  if (countinput == "prior_attainment") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(prior_attainment, subject_name) %>%
      summarise(n = sum(count)) %>%
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
      select(subject_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")


    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(prior_attainment, subject_name) %>%
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
      select(subject_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)

    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_data
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- footerdata %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(prior_attainment, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(subject_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")

    coldefs <- list(
      subject_name = colDef(
        name = "Subject area", width = 600, footer = "TOTAL (N)",
        style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
        headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
        footerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, fontWeight = "bold")
      ),
      `All` = colDef(na = "c", name = "All", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`All`), 5), big.mark = ",", scientific = FALSE)),
      `1` = colDef(na = "c", name = "4 As or more", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`1`), 5), big.mark = ",", scientific = FALSE)),
      `2` = colDef(na = "c", name = "360 points", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`2`), 5), big.mark = ",", scientific = FALSE)),
      `3` = colDef(na = "c", name = "300-359 points", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`3`), 5), big.mark = ",", scientific = FALSE)),
      `4` = colDef(na = "c", name = "240-299 points", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`4`), 5), big.mark = ",", scientific = FALSE)),
      `5` = colDef(na = "c", name = "180-239 points", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`5`), 5), big.mark = ",", scientific = FALSE)),
      `6` = colDef(na = "c", name = "Below 180 points", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`6`), 5), big.mark = ",", scientific = FALSE)),
      `7` = colDef(na = "c", name = "1 or 2 A level passes", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`7`), 5), big.mark = ",", scientific = FALSE)),
      `8` = colDef(na = "c", name = "BTEC", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`8`), 5), big.mark = ",", scientific = FALSE)),
      `9` = colDef(na = "c", name = "Other", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`9`), 5), big.mark = ",", scientific = FALSE)),
      `Not known` = colDef(na = "c", name = "Not known", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Not known`), 5), big.mark = ",", scientific = FALSE))
    )
  }

  if (countinput == "SECTIONNAME") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All", prior_attainment == "All",
        qualification_TR == qualinput, threshold == "All"
      ) %>%
      group_by(SECTIONNAME, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(SECTIONNAME, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))


    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == "All"
      ) %>%
      group_by(SECTIONNAME, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(SECTIONNAME, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .)))

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_data
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- footerdata %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == "All"
      ) %>%
      group_by(SECTIONNAME, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(SECTIONNAME, n) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .)))

    coldefs <- list(
      reactable::colDef(style = stylefunc, format = colformat, na = "c")
    )

    # get names of numerical cols
    numcols <- crosstabs_data %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()
    # replicate list to required length
    coldefs <- rep(coldefs, length(numcols))
    # name elements of list according to cols
    names(coldefs) <- numcols

    coldefs$subject_name <- colDef(
      name = "Subject area", width = 600, footer = "TOTAL (N)",
      style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
      headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
      footerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, fontWeight = "bold")
    )
  }

  if (countinput == "qualification_TR") {
    crosstabs_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", subject_name != "All", threshold == "All"
      ) %>%
      group_by(qualification_TR, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .)))


    crosstabs_earnings_data <- tables_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", subject_name != "All", threshold == "All"
      ) %>%
      group_by(qualification_TR, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .)))

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_data
      colformat <- colFormat(prefix = "£", separators = TRUE, digits = 0)
      crosstabs_data <- crosstabs_earnings_data2
    }

    footer_data <- footerdata %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", subject_name != "All", threshold == "All"
      ) %>%
      group_by(qualification_TR, subject_name) %>%
      summarise(n = sum(count)) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(is.na(.), 0, .))) %>%
      mutate_at(vars(-group_cols()), funs(ifelse(. <= 2, 0, .)))


    coldefs <- list(
      subject_name = colDef(na = "c", name = "Subject area", width = 600, footer = "TOTAL (N)"),
      `First degree` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`First degree`), 5), big.mark = ",", scientific = FALSE)),
      `Level 7 (taught)` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Level 7 (taught)`), 5), big.mark = ",", scientific = FALSE)),
      `Level 7 (research)` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Level 7 (research)`), 5), big.mark = ",", scientific = FALSE)),
      `Level 8` = colDef(na = "c", style = stylefunc, format = colformat, footer = format(round_any(sum(footer_data$`Level 8`), 5), big.mark = ",", scientific = FALSE))
    )
  }

  crosstab <- reactable(crosstabs_data,
    defaultPageSize = 37, showSortable = TRUE, columns = coldefs,
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
  )


  return(crosstab)
}
