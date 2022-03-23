backwards_crosstabs <- function(sectioninput, YAGinput, countinput, qualinput, buttoninput) {

  # tables_data <- tables_data %>%
  #   filter(subject_name != 'All')

  tables_data$SECTIONNAME[is.na(tables_data$SECTIONNAME) == TRUE] <- "NOT KNOWN"

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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(subject_name, White, Black, Asian, Mixed, Other, `Not known`)

    crosstabs_earnings_data <- tables_earnings_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(ethnicity, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(ethnicity, n) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("White", "Black", "Asian", "Mixed", "Other", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(subject_name, White, Black, Asian, Mixed, Other, `Not known`)

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_earnings_data
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
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


    crosstabs_earnings_data <- tables_earnings_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(current_region, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(current_region, n) %>%
      arrange(-All) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c(
          "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands",
          "East of England", "London", "South East", "South West"
        ),
        funs(as.numeric(.))
      ) %>%
      mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
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
      footerdata <- tables_earnings_data
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      select(subject_name, `non-FSM`, FSM, `Not known`)


    crosstabs_earnings_data <- tables_earnings_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(FSM, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(FSM, n) %>%
      arrange(-All) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("non-FSM", "FSM", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
      select(subject_name, `non-FSM`, FSM, `Not known`)

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)

    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_earnings_data
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      select(subject_name, `F`, `M`, `F+M`)
    names(crosstabs_data) <- c("subject_name", "Female", "Male", "Female & Male")



    crosstabs_earnings_data <- tables_earnings_data %>%
      filter(
        SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
        prior_attainment == "All", qualification_TR == qualinput, subject_name != "All", threshold == "All"
      ) %>%
      group_by(sex, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(sex, n) %>%
      arrange(-`F+M`) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("F", "M", "F+M"),
        funs(as.numeric(.))
      ) %>%
      mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
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
      footerdata <- tables_earnings_data
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      # We can show all regions (including Abroad, Scotland, Wales and Northern Ireland) if we want too.
      select(subject_name, "All", `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, "Not known")


    crosstabs_earnings_data <- tables_earnings_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", qualification_TR == "First degree", subject_name != "All", threshold == "All"
      ) %>%
      group_by(prior_attainment, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(prior_attainment, n) %>%
      arrange(-All) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_at(
        c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Not known"),
        funs(as.numeric(.))
      ) %>%
      mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
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
      footerdata <- tables_earnings_data
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .)))


    crosstabs_earnings_data <- tables_earnings_data %>%
      filter(
        sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
        prior_attainment == "All", qualification_TR == qualinput, threshold == "All"
      ) %>%
      group_by(SECTIONNAME, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(SECTIONNAME, n) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .)))

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_earnings_data
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .)))

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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .))) %>%
      mutate_if(is.numeric, funs(. / sum(.))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .)))


    crosstabs_earnings_data <- tables_earnings_data %>%
      filter(
        sex == "F+M", SECTIONNAME == sectioninput, YAG == YAGinput, ethnicity == "All", FSM == "All",
        current_region == "All", prior_attainment == "All", subject_name != "All", threshold == "All"
      ) %>%
      group_by(qualification_TR, subject_name) %>%
      summarise(n = earnings_median) %>%
      spread(qualification_TR, n) %>%
      arrange(-`First degree`) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. == 0, NA, .))) %>%
      mutate_all(funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .)))

    order <- subset(crosstabs_data, select = subject_name)
    crosstabs_earnings_data2 <- order %>%
      left_join(crosstabs_earnings_data)


    if (buttoninput == "Proportions") {
      footerdata <- tables_data
      colformat <- colFormat(percent = TRUE, digits = 1)
      crosstabs_data <- crosstabs_data
    } else if (buttoninput == "Median earnings") {
      footerdata <- tables_earnings_data
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
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      mutate_all(funs(ifelse(. <= 2, 0, .)))


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
