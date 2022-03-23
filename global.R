library(plyr)
library(leaflet)
library(sp)
library(data.table)
library(RColorBrewer)
library(pander)
library(dplyr)
library(tidyr)
library(shinycssloaders)
library(plotly)
library(DT)
library(ggalt)
library(magrittr)
library(scales)
library(openxlsx)
library(sf)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(networkD3)
library(reactable)
library(knitr)
library(Hmisc)
library(matrixStats)
library(sourcetools)
library(shinyBS)
library(readr)
library(metathis)
library(DescTools)

cohort1 <- read.csv("data/pg_sankey_data_1_3_yag_dummy.csv")
cohort2 <- read.csv("data/pg_sankey_data_3_5_yag_dummy.csv")
cohort3 <- read.csv("data/pg_sankey_data_1_5_yag_dummy.csv")

cohort1 <- subset(cohort1, select = -X)
cohort2 <- subset(cohort2, select = -X)
cohort3 <- subset(cohort3, select = -X)

cohort1$SECTIONNAME.x <- StrCap(tolower(cohort1$SECTIONNAME.x))
cohort1$SECTIONNAME.y <- StrCap(tolower(cohort1$SECTIONNAME.y))
cohort2$SECTIONNAME.x <- StrCap(tolower(cohort2$SECTIONNAME.x))
cohort2$SECTIONNAME.y <- StrCap(tolower(cohort2$SECTIONNAME.y))
cohort3$SECTIONNAME.x <- StrCap(tolower(cohort3$SECTIONNAME.x))
cohort3$SECTIONNAME.y <- StrCap(tolower(cohort3$SECTIONNAME.y))

tables_data <- fread("data/pg_sic_crosstabs_earnings_data_cf_dummy.csv") %>% select(-V1)

names(tables_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "earnings_median", "threshold", "qualification_TR", "group_name")

tables_data$SECTIONNAME[tables_data$group_name == "Radio broadcasting"] <- "INFORMATION AND COMMUNICATION"
tables_data$SECTIONNAME[tables_data$group_name == "Reproduction of recorded media"] <- "MANUFACTURING"

tables_data$SECTIONNAME <- StrCap(tolower(tables_data$SECTIONNAME))

tables_earnings_data <- tables_data

names(tables_earnings_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "earnings_median", "threshold", "qualification_TR", "group_name")

# Create sankey chart, inputs based on server logic from ui inputs.


# Sankey chart ------------------------------------------------------------

sankey_chart <- function(subjectinput, sexinput, qualinput) {
  cohort_sankey1 <- cohort1 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey2 <- cohort2 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey1 <- na.omit(cohort_sankey1)
  cohort_sankey2 <- na.omit(cohort_sankey2)

  # quick table code
  # 1 YAG
  one_yag_table <- cohort_sankey1 %>%
    group_by(YAG.x, SECTIONNAME.x) %>%
    dplyr::summarise(count = sum(count))
  one_yag_table <- one_yag_table %>%
    mutate("1 YAG" = count / sum(count))

  # 3 YAG
  three_yag_table <- cohort_sankey1 %>%
    group_by(YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count))
  three_yag_table <- three_yag_table %>%
    mutate("3 YAG" = count / sum(count))

  # 5 YAG
  five_yag_table <- cohort_sankey2 %>%
    group_by(YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count))
  five_yag_table <- five_yag_table %>%
    mutate("5 YAG" = count / sum(count))

  # Choose top 9 SIC section names and label all others as 'OTHER' based on counts for 1 YAG

  section_names1 <- cohort_sankey1 %>%
    group_by(SECTIONNAME.x) %>%
    dplyr::summarise(count = sum(count)) %>%
    arrange(., -count)

  section_names2 <- cohort_sankey1 %>%
    group_by(SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count)) %>%
    arrange(., -count)

  section_names <- section_names1 %>%
    full_join(section_names2, by = c("SECTIONNAME.x" = "SECTIONNAME.y"))
  section_names$count.x[is.na(section_names$count.x) == TRUE] <- 0
  section_names$count.y[is.na(section_names$count.y) == TRUE] <- 0
  section_names$count <- section_names$count.x + section_names$count.y

  section_names <- section_names[, -c(2, 3)] %>%
    arrange(., -count)

  section_names$ID <- 1:nrow(section_names)
  section_names$SECTIONNAME_NEW <- section_names$SECTIONNAME.x
  section_names$SECTIONNAME_NEW[section_names$ID > 9] <- "Other"

  names(section_names) <- c("old", "count", "ID", "new")

  # re-calculate counts using new section names variable

  cohort_sankey1 <- cohort_sankey1 %>%
    left_join(section_names, by = c("SECTIONNAME.x" = "old"))
  cohort_sankey1$SECTIONNAME.x <- cohort_sankey1$new
  cohort_sankey1 <- cohort_sankey1[, -c(10, 11, 12)]

  cohort_sankey1 <- cohort_sankey1 %>%
    left_join(section_names, by = c("SECTIONNAME.y" = "old"))
  cohort_sankey1$SECTIONNAME.y <- cohort_sankey1$new
  cohort_sankey1 <- cohort_sankey1[, -c(10, 11, 12)]

  cohort_sankey1$SECTIONNAME.y[is.na(cohort_sankey1$SECTIONNAME.y) == TRUE] <- "Other"

  cohort_sankey1 <- cohort_sankey1 %>%
    group_by(sex.x, subject_name.x, YAG.x, SECTIONNAME.x, YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count.x)) %>%
    arrange(., -count)

  # re-calculate counts using new section names variable

  cohort_sankey2 <- cohort_sankey2 %>%
    left_join(section_names, by = c("SECTIONNAME.x" = "old"))
  cohort_sankey2$SECTIONNAME.x <- cohort_sankey2$new
  cohort_sankey2 <- cohort_sankey2[, -c(10, 11, 12)]

  cohort_sankey2 <- cohort_sankey2 %>%
    left_join(section_names, by = c("SECTIONNAME.y" = "old"))
  cohort_sankey2$SECTIONNAME.y <- cohort_sankey2$new
  cohort_sankey2 <- cohort_sankey2[, -c(10, 11, 12)]

  cohort_sankey2$SECTIONNAME.y[is.na(cohort_sankey2$SECTIONNAME.y) == TRUE] <- "Other"
  cohort_sankey2$SECTIONNAME.x[is.na(cohort_sankey2$SECTIONNAME.x) == TRUE] <- "Other"

  cohort_sankey2 <- cohort_sankey2 %>%
    group_by(sex.x, subject_name.x, YAG.x, SECTIONNAME.x, YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count.x)) %>%
    arrange(., -count)

  # Now name nodes

  library(networkD3)
  nodes <- data.frame("name" = c(
    unique(cohort_sankey1$SECTIONNAME.x),
    unique(c(cohort_sankey1$SECTIONNAME.y, cohort_sankey2$SECTIONNAME.x)),
    unique(cohort_sankey2$SECTIONNAME.y)
  ))

  nodes$ID <- 0:(nrow(nodes) - 1)

  # View nodes and separate into 3 sections to join the ID numbers into links.

  nodes1 <- nodes[1:length(unique(cohort_sankey1$SECTIONNAME.x)), ]
  nodes2 <- nodes[(length(unique(cohort_sankey1$SECTIONNAME.x)) + 1):((length(unique(cohort_sankey1$SECTIONNAME.x))) + length(unique(c(cohort_sankey1$SECTIONNAME.y, cohort_sankey2$SECTIONNAME.x)))), ]
  nodes3 <- nodes[((length(unique(cohort_sankey1$SECTIONNAME.x))) + length(unique(c(cohort_sankey1$SECTIONNAME.y, cohort_sankey2$SECTIONNAME.x))) + 1):nrow(nodes), ]

  # Create links for 1-3 YAG ------------------------------------------------

  links1 <- as.data.frame(
    cohort_sankey1[, c(4, 6, 7)],
    byrow = TRUE, ncol = 3
  )

  names(links1) <- c("source", "target", "value")

  # Change names in links to numbers

  links1 <- links1 %>%
    left_join(nodes1, by = c("source" = "name"))
  links1$source <- links1$ID
  links1 <- links1[, -4]

  links1 <- links1 %>%
    left_join(nodes2, by = c("target" = "name"))
  links1$target <- links1$ID
  links1 <- links1[, -4]


  # Create links for 3-5 YAG ------------------------------------------------

  links2 <- as.data.frame(
    cohort_sankey2[, c(4, 6, 7)],
    byrow = TRUE, ncol = 3
  )

  names(links2) <- c("source", "target", "value")

  # Change names in links to numbers

  links2 <- links2 %>%
    left_join(nodes2, by = c("source" = "name"))
  links2$source <- links2$ID
  links2 <- links2[, -4]

  links2 <- links2 %>%
    left_join(nodes3, by = c("target" = "name"))
  links2$target <- links2$ID
  links2 <- links2[, -4]


  # join the 2 links together

  links <- links1 %>%
    full_join(links2)

  links <- links %>%
    mutate_at(
      "value",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  plot <- sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name", fontSize = 10, nodePadding = 20
  )

  return(plot)
}

sankey_title <- function(subjectinput, sexinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )

  if (sexinput == "F") {
    sextext <- "female"
  } else if (sexinput == "M") {
    sextext <- "male"
  } else {
    sextext <- "male and female"
  }


  sankey_title <- paste("<h4> Industry of graduate employment for 2012/13 academic year graduates of", subjecttext, "one, three and five years after
                          graduation (YAG), ", sextext, qualinput, "graduates from English HEIs, APs and FECs, 2018/19 tax year.</h4>")

  return(sankey_title)
}

# Sankey table ------------------------------------------------------------


sankey_table <- function(subjectinput, sexinput, qualinput) {
  cohort_sankey1 <- cohort1 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey2 <- cohort2 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey1 <- na.omit(cohort_sankey1)
  cohort_sankey2 <- na.omit(cohort_sankey2)

  # quick table code
  # 1 YAG
  one_yag_table <- cohort_sankey1 %>%
    group_by(YAG.x, SECTIONNAME.x) %>%
    dplyr::summarise(count = sum(count))
  one_yag_table <- one_yag_table %>%
    mutate("1 YAG" = count / sum(count))

  # 3 YAG
  three_yag_table <- cohort_sankey2 %>%
    group_by(YAG.x, SECTIONNAME.x) %>%
    dplyr::summarise(count = sum(count))
  three_yag_table <- three_yag_table %>%
    mutate("3 YAG" = count / sum(count))

  # 5 YAG
  five_yag_table <- cohort_sankey2 %>%
    group_by(YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count))
  five_yag_table <- five_yag_table %>%
    mutate("5 YAG" = count / sum(count))


  # create table
  # join
  yag_table <- one_yag_table %>%
    left_join(three_yag_table, by = c("SECTIONNAME.x" = "SECTIONNAME.x"))

  yag_table <- yag_table %>%
    left_join(five_yag_table, by = c("SECTIONNAME.x" = "SECTIONNAME.y"))

  yag_table[is.na(yag_table)] <- 0

  # quick check - should all be one
  sum(yag_table$`1 YAG`)
  sum(yag_table$`3 YAG`)
  sum(yag_table$`5 YAG`)

  # remove columns
  yag_table_final <- yag_table[, c("SECTIONNAME.x", "1 YAG", "3 YAG", "5 YAG")]

  yag_table_final <- yag_table_final %>%
    arrange(., -`1 YAG`)

  names(yag_table_final) <- c("INDUSTRY", "1 YAG", "3 YAG", "5 YAG")

  library(reactable)
  library(magrittr)

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
    normalized <- (value - min(yag_table_final[name], na.rm = T)) /
      (max(yag_table_final[name], na.rm = T) - min(yag_table_final[name], na.rm = T))
    color <- orange_pal(normalized)
    list(background = color)
  }

  # list giving column formatting (using style function) for single column

  coldefs <- list(
    INDUSTRY = colDef(na = "c", name = "Industry", width = 600, footer = "TOTAL (N)"),
    `1 YAG` = colDef(na = "c", style = stylefunc, format = colFormat(percent = TRUE, digits = 1), footer = format(round_any(sum(yag_table$count.x), 5), big.mark = ",", scientific = FALSE)),
    `3 YAG` = colDef(na = "c", style = stylefunc, format = colFormat(percent = TRUE, digits = 1), footer = format(round_any(sum(yag_table$count.y), 5), big.mark = ",", scientific = FALSE)),
    `5 YAG` = colDef(na = "c", style = stylefunc, format = colFormat(percent = TRUE, digits = 1), footer = format(round_any(sum(yag_table$count), 5), big.mark = ",", scientific = FALSE))
  )


  # create table
  sankey_table <- reactable(yag_table_final,
    defaultPageSize = 21, columns = coldefs, showSortable = TRUE,
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
  )
}


# Sankey text -------------------------------------------------------------


sankeytext1 <- function(subjectinput, sexinput, qualinput) {
  if (sexinput == "F+M") {
    sextext <- "both female and male"
  }

  if (sexinput == "F") {
    sextext <- "female"
  }

  if (sexinput == "M") {
    sextext <- "male"
  }

  cohort_sankey1 <- cohort1 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey2 <- cohort2 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey1 <- na.omit(cohort_sankey1)
  cohort_sankey2 <- na.omit(cohort_sankey2)

  # quick table code
  # 1 YAG
  one_yag_table <- cohort_sankey1 %>%
    group_by(YAG.x, SECTIONNAME.x) %>%
    dplyr::summarise(count = sum(count))
  one_yag_table <- one_yag_table %>%
    mutate("1 YAG" = count / sum(count))

  # 3 YAG
  three_yag_table <- cohort_sankey1 %>%
    group_by(YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count))
  three_yag_table <- three_yag_table %>%
    mutate("3 YAG" = count / sum(count))

  # 5 YAG
  five_yag_table <- cohort_sankey2 %>%
    group_by(YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count))
  five_yag_table <- five_yag_table %>%
    mutate("5 YAG" = count / sum(count))


  # create table
  # join
  yag_table <- one_yag_table %>%
    left_join(three_yag_table, by = c("SECTIONNAME.x" = "SECTIONNAME.y"))

  yag_table <- yag_table %>%
    left_join(five_yag_table, by = c("SECTIONNAME.x" = "SECTIONNAME.y"))

  yag_table[is.na(yag_table)] <- 0

  # quick check - should all be one
  sum(yag_table$`1 YAG`)
  sum(yag_table$`3 YAG`)
  sum(yag_table$`5 YAG`)

  # remove columns
  yag_table_final <- yag_table[, c("SECTIONNAME.x", "1 YAG", "3 YAG", "5 YAG")]

  yag_table_final <- yag_table_final %>%
    arrange(., -`1 YAG`)

  yag_table_final$`1 YAG` <- readr::parse_number(scales::percent(yag_table_final$`1 YAG`, accuracy = 0.1))
  yag_table_final$`3 YAG` <- readr::parse_number(scales::percent(yag_table_final$`3 YAG`, accuracy = 0.1))
  yag_table_final$`5 YAG` <- readr::parse_number(scales::percent(yag_table_final$`5 YAG`, accuracy = 0.1))

  five_yag_table_subject <- yag_table_final %>%
    arrange(., -`5 YAG`)

  ifelse(subjectinput == "All",
    sankeytext1 <- paste("For ", sextext, ", ", qualinput, " graduates of all subjects, the industry with the highest
                      proportion of graduates one year after graduation is ", first(yag_table_final$SECTIONNAME.x),
      " (", first(yag_table_final$`1 YAG`), "%), and at five years after graduation it is ",
      ifelse(first(five_yag_table_subject$SECTIONNAME.x) == first(yag_table_final$SECTIONNAME.x),
        "the same",
        first(five_yag_table_subject$SECTIONNAME.x)
      ), " (", first(five_yag_table_subject$`5 YAG`), "%).",
      sep = ""
    ),
    sankeytext1 <- paste(
      "For ", sextext, ", ", qualinput, " graduates who studied", subjectinput, ", the industry with the highest
                      proportion of graduates one year after graduation is ", first(yag_table_final$SECTIONNAME.x),
      " (", first(yag_table_final$`1 YAG`), "%), and at five years after graduation it is ",
      ifelse(first(five_yag_table_subject$SECTIONNAME.x) == first(yag_table_final$SECTIONNAME.x),
        "the same",
        first(five_yag_table_subject$SECTIONNAME.x)
      ), " (", first(five_yag_table_subject$`5 YAG`), "%)."
    )
  )

  return(sankeytext1)
}

sankeytext2 <- function(subjectinput, sexinput, qualinput) {
  cohort_sankey1 <- cohort1 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey2 <- cohort2 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey1 <- na.omit(cohort_sankey1)
  cohort_sankey2 <- na.omit(cohort_sankey2)

  # quick table code
  # 1 YAG
  one_yag_table <- cohort_sankey1 %>%
    group_by(YAG.x, SECTIONNAME.x) %>%
    dplyr::summarise(count = sum(count))
  one_yag_table <- one_yag_table %>%
    mutate("1 YAG" = count / sum(count))

  # 3 YAG
  three_yag_table <- cohort_sankey1 %>%
    group_by(YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count))
  three_yag_table <- three_yag_table %>%
    mutate("3 YAG" = count / sum(count))

  # 5 YAG
  five_yag_table <- cohort_sankey2 %>%
    group_by(YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count))
  five_yag_table <- five_yag_table %>%
    mutate("5 YAG" = count / sum(count))

  # Choose top 9 SIC section names and label all others as 'OTHER' based on counts for 1 YAG

  section_names1 <- cohort_sankey1 %>%
    group_by(SECTIONNAME.x) %>%
    dplyr::summarise(count = sum(count)) %>%
    arrange(., -count)

  section_names2 <- cohort_sankey1 %>%
    group_by(SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count)) %>%
    arrange(., -count)

  section_names <- section_names1 %>%
    full_join(section_names2, by = c("SECTIONNAME.x" = "SECTIONNAME.y"))
  section_names$count.x[is.na(section_names$count.x) == TRUE] <- 0
  section_names$count.x[is.na(section_names$count.y) == TRUE] <- 0
  section_names$count <- section_names$count.x + section_names$count.y

  section_names <- section_names[, -c(2, 3)] %>%
    arrange(., -count)

  section_names$ID <- 1:nrow(section_names)
  section_names$SECTIONNAME_NEW <- section_names$SECTIONNAME.x
  section_names$SECTIONNAME_NEW[section_names$ID > 9] <- "Other"

  names(section_names) <- c("old", "count", "ID", "new")

  # re-calculate counts using new section names variable

  cohort_sankey1 <- cohort_sankey1 %>%
    left_join(section_names, by = c("SECTIONNAME.x" = "old"))
  cohort_sankey1$SECTIONNAME.x <- cohort_sankey1$new
  cohort_sankey1 <- cohort_sankey1[, -c(10, 11, 12)]

  cohort_sankey1 <- cohort_sankey1 %>%
    left_join(section_names, by = c("SECTIONNAME.y" = "old"))
  cohort_sankey1$SECTIONNAME.y <- cohort_sankey1$new
  cohort_sankey1 <- cohort_sankey1[, -c(10, 11, 12)]

  cohort_sankey1$SECTIONNAME.y[is.na(cohort_sankey1$SECTIONNAME.y) == TRUE] <- "Other"

  cohort_sankey1 <- cohort_sankey1 %>%
    group_by(sex.x, subject_name.x, YAG.x, SECTIONNAME.x, YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count.x)) %>%
    arrange(., -count)

  # re-calculate counts using new section names variable

  cohort_sankey2 <- cohort_sankey2 %>%
    left_join(section_names, by = c("SECTIONNAME.x" = "old"))
  cohort_sankey2$SECTIONNAME.x <- cohort_sankey2$new
  cohort_sankey2 <- cohort_sankey2[, -c(10, 11, 12)]

  cohort_sankey2 <- cohort_sankey2 %>%
    left_join(section_names, by = c("SECTIONNAME.y" = "old"))
  cohort_sankey2$SECTIONNAME.y <- cohort_sankey2$new
  cohort_sankey2 <- cohort_sankey2[, -c(10, 11, 12)]

  cohort_sankey2$SECTIONNAME.y[is.na(cohort_sankey2$SECTIONNAME.y) == TRUE] <- "Other"
  cohort_sankey2$SECTIONNAME.x[is.na(cohort_sankey2$SECTIONNAME.x) == TRUE] <- "Other"

  cohort_sankey2 <- cohort_sankey2 %>%
    group_by(sex.x, subject_name.x, YAG.x, SECTIONNAME.x, YAG.y, SECTIONNAME.y) %>%
    dplyr::summarise(count = sum(count.x)) %>%
    arrange(., -count)

  cohort_sankey1_text <- cohort_sankey1 %>%
    filter(SECTIONNAME.x != SECTIONNAME.y) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  cohort_sankey1_text$count <- prettyNum(cohort_sankey1_text$count, big.mark = ",", scientific = FALSE)

  cohort_sankey2_text <- cohort_sankey2 %>%
    filter(SECTIONNAME.x != SECTIONNAME.y) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  cohort_sankey2_text$count <- prettyNum(cohort_sankey2_text$count, big.mark = ",", scientific = FALSE)

  sankeytext2 <- paste("The most movement between one and three years after graduation is seen for ",
    first(cohort_sankey1_text$SECTIONNAME.x), ", where ", first(cohort_sankey1_text$count),
    " graduates move to ", first(cohort_sankey1_text$SECTIONNAME.y), ". Between three and five
                       years after graduation it's seen for ", first(cohort_sankey2_text$SECTIONNAME.x), " where",
    first(cohort_sankey2_text$count), " graduates moved to ", first(cohort_sankey2_text$SECTIONNAME.y),
    ".",
    sep = ""
  )

  return(sankeytext2)
}

# earnings_text <- function(subjectinput, sexinput){
#
#   cohort_earnings1 <- cohort3 %>%
#     filter(subject_name.x == subjectinput, sex.x == sexinput)
#
#   earnings_text_table <- cohort_earnings1 %>%
#     arrange(.,-earnings_change_average) %>%
#     mutate_at('count',
#               funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))) %>%
#     mutate_at(c("earnings_median_1YAG", "earnings_median_5YAG", "earnings_change_average"),
#               funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))) %>%
#     filter(count>=11)
#
#   earnings_text_table$count <- prettyNum(earnings_text_table$count, big.mark = ",", scientific = FALSE)
#   earnings_text_table$earnings_change_average <- prettyNum(earnings_text_table$earnings_change_average, big.mark = ",", scientific = FALSE)
#
#   ifelse(last(earnings_text_table$CHANGE == "negative"),
#          lowesttext <- 'largest decrease in median earnings is seen in those graduates moving from',
#          lowesttext <- 'lowest increase in median earnings is seen in those graduates moving from')
#
#   #re-calculate counts using new section names variable
#
#   earnings_text <- paste("Of those moving industry between one and five years after graduation (excluding counts of less than 11), the flow with the biggest earnings increase is
#                        seen in those moving from ", first(earnings_text_table$SECTIONNAME.x)," to ",
#                          first(earnings_text_table$SECTIONNAME.y), " where ", first(earnings_text_table$count), " graduates moved,
#                        and the increase in median earnings was £",first(earnings_text_table$earnings_change_average),".",br()," The flow
#                        with the ",lowesttext,last(earnings_text_table$SECTIONNAME.x)," to ",last(earnings_text_table$SECTIONNAME.y),
#                          " where ",last(earnings_text_table$count)," graduates moved, and the difference in median earnings was £",
#                          last(earnings_text_table$earnings_change_average),".", sep = '')
#
# }


# Earnings ----------------------------------------------------------------

earnings_sankey <- function(subjectinput, sexinput, earningsinput) {
  cohort_earnings1 <- cohort3 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, SECTIONNAME.x == earningsinput)

  # Choose top 9 SIC section names and label all others as 'OTHER' based on counts for 1 YAG

  # Now name nodes

  library(networkD3)
  nodes <- data.frame("name" = c(
    unique(cohort_earnings1$SECTIONNAME.x),
    unique(cohort_earnings1$SECTIONNAME.y)
  ))

  nodes <- na.omit(nodes)
  nodes$ID <- 0:(nrow(nodes) - 1)

  # View nodes and separate into sections to join the ID numbers into links.

  nodes1 <- nodes[1, ]
  nodes2 <- nodes[2:nrow(nodes), ]

  # Create links for 1-3 YAG ------------------------------------------------

  links1 <- as.data.frame(
    cohort_earnings1[, c(7, 9, 10, 14)],
    byrow = TRUE, ncol = 4
  )

  names(links1) <- c("source", "target", "value", "change")

  # Change names in links to numbers

  links1 <- links1 %>%
    left_join(nodes1, by = c("source" = "name"))
  links1$source <- links1$ID
  links1 <- links1[, -5]

  links1 <- links1 %>%
    left_join(nodes2, by = c("target" = "name"))
  links1$target <- links1$ID
  links1 <- links1[, -5]

  nodes$change <- as.factor(c("my_unique_group"))
  my_color <- 'd3.scaleOrdinal() .domain(["positive", "negative", "my_unique_group"]) .range(["palegreen", "lightpink", "grey"])'

  sankeyNetwork(
    Links = links1, Nodes = nodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name", LinkGroup = "change", NodeGroup = "change", colourScale = my_color,
    fontSize = 10
  )
}

# earnings table

earnings_table <- function(subjectinput, sexinput, earningsinput) {
  cohort_earnings1 <- cohort3 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, SECTIONNAME.x == earningsinput)

  total_earnings <- earnings_data %>%
    filter(subject_name == subjectinput, sex == sexinput, SECTIONNAME == earningsinput)


  yag_table_final2 <- cohort_earnings1[, c(
    "SECTIONNAME.x", "earnings_median_1YAG", "SECTIONNAME.y",
    "earnings_median_5YAG", "count", "earnings_change_average"
  )] %>%
    arrange(., -count) %>%
    mutate_at(
      c("earnings_median_1YAG", "earnings_median_5YAG", "earnings_change_average"),
      funs(ifelse(count < 10.9999, NA, .))
    ) %>%
    mutate_at(
      "count",
      funs(ifelse(count < 10.9999, NA, .))
    ) %>%
    mutate_at(
      c("earnings_median_1YAG", "earnings_median_5YAG", "earnings_change_average"),
      funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))
    ) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  yag_table_final2$earnings_change_average <- yag_table_final2$earnings_median_5YAG - yag_table_final2$earnings_median_1YAG

  # reactable

  library(reactable)
  library(magrittr)

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
    normalized <- (value - min(yag_table_final2[name], na.rm = T)) /
      (max(yag_table_final2[name], na.rm = T) - min(yag_table_final2[name], na.rm = T))
    color <- orange_pal(normalized)
    list(background = color)
  }


  # list giving column formatting (using style function) for single column
  coldefs <- list(
    colDef(
      na = "c",
      style = stylefunc,
      name = "Earnings median",
      minWidth = 30,
      format = colFormat(prefix = "£", separators = TRUE, digits = 0)
    )
  )

  # get names of numerical cols
  numcols <- c("earnings_median_1YAG", "earnings_median_5YAG")
  # replicate list to required length
  coldefs <- rep(coldefs, length(numcols))
  # name elements of list according to cols
  names(coldefs) <- numcols

  coldefs$earnings_change_average <- colDef(
    style = function(value) {
      if (value > 0 & is.na(value) == FALSE) {
        color <- "#008000"
      } else if (value < 0 & is.na(value) == FALSE) {
        color <- "#e00000"
      } else {
        color <- "#777"
      }
      list(color = color, fontWeight = "bold")
    },
    name = "Difference in medians",
    minWidth = 30,
    na = "c",
    format = colFormat(prefix = "£", separators = TRUE, digits = 0)
  )

  coldefs$SECTIONNAME.x <- colDef(name = "Industry", footer = paste("Total median for all grads in", earningsinput))
  coldefs$SECTIONNAME.y <- colDef(name = "Industry", footer = paste("Total median for all grads in", earningsinput))
  coldefs$count <- colDef(na = "c", minWidth = 25, name = "Number of graduates", format = colFormat(separators = TRUE))

  YAG1_total <- total_earnings %>%
    filter(YAG == 1) %>%
    dplyr::select(earnings_median) %>%
    mutate_at(
      "earnings_median",
      funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))
    )

  YAG5_total <- total_earnings %>%
    filter(YAG == 5) %>%
    dplyr::select(earnings_median) %>%
    mutate_at(
      "earnings_median",
      funs(ifelse(!is.na(as.numeric(.)), round(as.numeric(.), -2), .))
    )

  coldefs$earnings_median_1YAG$footer <- paste("£", format(YAG1_total, big.mark = ",", scientific = FALSE))
  coldefs$earnings_median_5YAG$footer <- paste("£", format(YAG5_total, big.mark = ",", scientific = FALSE))

  # create table
  earnings_table <- reactable(yag_table_final2,
    defaultPageSize = 21, columns = coldefs, showSortable = TRUE,
    columnGroups = list(
      colGroup(name = "1 YAG", columns = c("SECTIONNAME.x", "earnings_median_1YAG")),
      colGroup(name = "5 YAG", columns = c("SECTIONNAME.y", "earnings_median_5YAG"))
    ),
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
  )
}



# REGIONAL ---------------------------------------------------------------------


data <- read.csv("data/regional_data_with_pg_dummy.csv")
regional_movement_data <- read.csv("data/regional_movement_with_pg_dummy.csv")

ukRegions <- st_read("data/boundaries/Regions__December_2019__Boundaries_EN_BFE.shp")
str(ukRegions)
ukRegions <- ukRegions[order(ukRegions$rgn19nm), ]
ukRegions$rgn19nm[ukRegions$rgn19nm == "Yorkshire and The Humber"] <- "Yorkshire and the Humber"

data$SECTIONNAME <- StrCap(tolower(data$SECTIONNAME))
regional_movement_data$SECTIONNAME <- StrCap(tolower(regional_movement_data$SECTIONNAME))

# Create the map

map_chart <- function(sectionnameinput, subjectinput, countinput, YAGinput, qualinput) {
  mapdata <- data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, qualification_TR == qualinput)
  mapdata <- left_join(ukRegions, mapdata, by = c("rgn19nm" = "region"))

  mapdata2 <- regional_movement_data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, qualification_TR == qualinput) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  instregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, InstRegion, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(trained_in_region2 = sum(count))

  currentregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, current_region, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(living_in_region2 = sum(count))

  instregion <- instregion %>%
    ungroup() %>%
    select(InstRegion, trained_in_region2)

  currentregion <- currentregion %>%
    ungroup() %>%
    select(current_region, living_in_region2)

  mapdata <- mapdata %>%
    left_join(instregion, by = c("rgn19nm" = "InstRegion")) %>%
    left_join(currentregion, by = c("rgn19nm" = "current_region"))

  mapdata <- mapdata %>%
    mutate(difference2 = living_in_region2 - trained_in_region2) %>%
    mutate(difference_prop2 = difference2 / trained_in_region2)

  leafletmapdata <- st_transform(mapdata, crs = 4326)


  if (countinput == "trained_in_region") {
    pal_fun <- colorNumeric("Blues", domain = leafletmapdata$trained_in_region2)
    fill_fun <- ~ pal_fun(trained_in_region2)
    value_fun <- ~ leafletmapdata$trained_in_region2
    title_fun <- "studied in the region"
  }

  if (countinput == "living_in_region") {
    pal_fun <- colorNumeric("Blues", domain = leafletmapdata$living_in_region2)
    fill_fun <- ~ pal_fun(living_in_region2)
    value_fun <- ~ leafletmapdata$living_in_region2
    title_fun <- "living in the region"
  }

  if (countinput == "difference") {
    pal_fun <- colorNumeric("RdBu", domain = leafletmapdata$difference2)
    fill_fun <- ~ pal_fun(difference2)
    value_fun <- ~ leafletmapdata$difference2
    title_fun <- "Difference"
  }

  if (countinput == "difference_prop") {
    pal_fun <- colorNumeric("RdBu", domain = leafletmapdata$difference_prop2)
    fill_fun <- ~ pal_fun(difference_prop2)
    value_fun <- ~ leafletmapdata$difference_prop2
    title_fun <- "Proportionate difference"
  }

  p_popup <- paste(
    "<B>", leafletmapdata$rgn19nm, "</B>", br(), br(),
    "Number who studied in region:        ", prettyNum(leafletmapdata$trained_in_region2, big.mark = ",", scientific = FALSE), br(),
    "Number who currently live in region: ", prettyNum(leafletmapdata$living_in_region2, big.mark = ",", scientific = FALSE), br(),
    "Difference in graduate numbers:      ", prettyNum(leafletmapdata$difference2, big.mark = ",", scientific = FALSE), br(),
    "Difference in proportion:            ", round(100 * leafletmapdata$difference_prop2, digits = 1), "%", br(),
    "Number of providers in region:       ", leafletmapdata$number_of_providers, br(),
    "Median graduate earnings:            £", prettyNum(leafletmapdata$earnings_median, big.mark = ",", scientific = FALSE)
  )

  map <- leaflet(leafletmapdata) %>%
    addPolygons(
      color = "black",
      weight = 1,
      fillColor = fill_fun,
      fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
      popup = p_popup
    ) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLegend("topright",
      pal = pal_fun,
      values = value_fun,
      title = title_fun
    )


  return(map)
}

map_title <- function(sectionnameinput, subjectinput, countinput, YAGinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )

  if (countinput == "trained_in_region") {
    counttext <- paste("number of graduates of", subjecttext, "now working in", sectionnameinput, " who
                       studied in each region")
  } else if (countinput == "living_in_region") {
    counttext <- paste("number of graduates of", subjecttext, "now working in", sectionnameinput, " who
                       are current living in each region")
  } else if (countinput == "difference") {
    counttext <- paste("difference in graduates of", subjecttext, "now working in", sectionnameinput, " who
                       studied in and are current living in each region")
  } else if (countinput == "difference_prop") {
    counttext <- paste("percentage difference in graduates of", subjecttext, "now working in", sectionnameinput, " who
                       studied in and are current living in each region")
  }


  map_title <- paste("<h4> Map to show the ", counttext, YAGinput, "years after
                          graduation, male and female", qualinput, "graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>")

  return(map_title)
}

map_text <- function(sectionnameinput, subjectinput, countinput, YAGinput, qualinput) {
  mapdata <- data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, qualification_TR == qualinput)

  mapdata2 <- regional_movement_data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, qualification_TR == qualinput) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  instregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, InstRegion, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(trained_in_region2 = sum(count))

  currentregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, current_region, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(living_in_region2 = sum(count))

  instregion <- instregion %>%
    ungroup() %>%
    select(InstRegion, trained_in_region2)

  currentregion <- currentregion %>%
    ungroup() %>%
    select(current_region, living_in_region2)

  mapdata <- mapdata %>%
    left_join(instregion, by = c("region" = "InstRegion")) %>%
    left_join(currentregion, by = c("region" = "current_region"))

  mapdata <- mapdata %>%
    mutate(difference2 = living_in_region2 - trained_in_region2) %>%
    mutate(difference_prop2 = difference2 / trained_in_region2)

  ifelse(subjectinput == "All",
    subjecttext <- paste("For", qualinput, "graduates of all subjects"),
    subjecttext <- paste("For", qualinput, "graduates of", subjectinput)
  )

  mapdata_trained <- mapdata %>%
    arrange(-trained_in_region2)

  mapdata_current <- mapdata %>%
    arrange(-living_in_region2)

  mapdata_earnings <- mapdata %>%
    arrange(-earnings_median)

  mapdata_difference <- mapdata %>%
    arrange(-difference2)

  mapdata_diff_prop <- mapdata %>%
    arrange(-difference_prop2)

  map_text <- paste(subjecttext, "in the", sectionnameinput, "industry", YAGinput, "years after graduation, the region that saw the highest number of students
                    studying there was", first(mapdata_trained$region), ". The region with the lowest number of students studying
                    there was", last(mapdata_trained$region), ". The region with the highest number of graduates living
                    there", YAGinput, "years after graduation was", first(mapdata_current$region), " and the region with the
                    least graduates living there was", last(mapdata_current$region), ".")

  return(map_text)
}

map_text2 <- function(sectionnameinput, subjectinput, countinput, YAGinput, qualinput) {
  mapdata <- data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, qualification_TR == qualinput)

  mapdata2 <- regional_movement_data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, qualification_TR == qualinput) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  instregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, InstRegion, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(trained_in_region2 = sum(count))

  currentregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, current_region, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(living_in_region2 = sum(count))

  instregion <- instregion %>%
    ungroup() %>%
    select(InstRegion, trained_in_region2)

  currentregion <- currentregion %>%
    ungroup() %>%
    select(current_region, living_in_region2)

  mapdata <- mapdata %>%
    left_join(instregion, by = c("region" = "InstRegion")) %>%
    left_join(currentregion, by = c("region" = "current_region"))

  mapdata <- mapdata %>%
    mutate(difference2 = living_in_region2 - trained_in_region2) %>%
    mutate(difference_prop2 = difference2 / trained_in_region2)

  ifelse(subjectinput == "All",
    subjecttext <- paste("For", qualinput, "graduates of all subjects"),
    subjecttext <- paste("For", qualinput, "graduates of", subjectinput)
  )

  mapdata_trained <- mapdata %>%
    arrange(-trained_in_region2)

  mapdata_current <- mapdata %>%
    arrange(-living_in_region2)

  mapdata_earnings <- mapdata %>%
    arrange(-earnings_median)

  mapdata_difference <- mapdata %>%
    arrange(-difference2)

  mapdata_diff_prop <- mapdata %>%
    arrange(-difference_prop)

  mapdata_diff_prop$difference_prop2 <- readr::parse_number(scales::percent(mapdata_diff_prop$difference_prop2, accuracy = 0.1))

  map_text <- paste(
    subjecttext, "in the", sectionnameinput, "industry, the region with the highest proportionate increase in graduates who studied there compared to living
                    there", YAGinput, "years after graduation is", first(mapdata_diff_prop$region), ", where the number of
                    graduates increased by", first(mapdata_diff_prop$difference_prop2), "%. The region with the largest
                    decrease is", last(mapdata_diff_prop$region), "where the number of graduates decreased by", last(mapdata_diff_prop$difference_prop2),
    "%."
  )

  return(map_text)
}

maptable <- function(sectionnameinput, subjectinput, countinput, YAGinput, regioninput, qualinput) {
  mapdata <- data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, region %in% c(regioninput), qualification_TR == qualinput)

  mapdata2 <- regional_movement_data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, qualification_TR == qualinput) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

  instregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, InstRegion, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(trained_in_region2 = sum(count))

  currentregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, current_region, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(living_in_region2 = sum(count))

  instregion <- instregion %>%
    ungroup() %>%
    select(InstRegion, trained_in_region2)

  currentregion <- currentregion %>%
    ungroup() %>%
    select(current_region, living_in_region2)

  mapdata <- mapdata %>%
    left_join(instregion, by = c("region" = "InstRegion")) %>%
    left_join(currentregion, by = c("region" = "current_region"))

  mapdata <- mapdata %>%
    mutate(difference2 = living_in_region2 - trained_in_region2) %>%
    mutate(difference_prop2 = difference2 / trained_in_region2)

  mapdata$difference_prop2 <- readr::parse_number(scales::percent(mapdata$difference_prop2, accuracy = 0.1))

  maptabledata <- mapdata[, c(
    "region", "trained_in_region2", "living_in_region2", "difference2", "difference_prop2", "number_of_providers",
    "earnings_median"
  )]

  map_table <- reactable(maptabledata,
    sortable = TRUE, resizable = TRUE, showSortable = TRUE,
    highlight = TRUE, fullWidth = FALSE,
    columns = list(
      region = colDef(name = "Region"),
      trained_in_region2 = colDef(name = "Studied in region", format = colFormat(separators = TRUE)),
      living_in_region2 = colDef(name = "Living in region", format = colFormat(separators = TRUE)),
      number_of_providers = colDef(
        name = "Number of providers", style = list(backgroundColor = "#f7f7f7"),
        headerStyle = list(backgroundColor = "#f7f7f7")
      ),
      difference2 = colDef(name = "Difference", format = colFormat(separators = TRUE)),
      difference_prop2 = colDef(name = "Difference (%)"),
      earnings_median = colDef(
        name = "Median earnings",
        format = colFormat(prefix = "£", separators = TRUE, digits = 0), style = list(backgroundColor = "#f7f7f7"),
        headerStyle = list(backgroundColor = "#f7f7f7")
      )
    ),
    columnGroups = list(
      colGroup(name = "Statistics", columns = c("living_in_region2", "trained_in_region2", "difference2", "difference_prop2")),
      colGroup(name = "Context", columns = c("number_of_providers", "earnings_median"))
    )
  )

  return(map_table)
}

regional_sankey <- function(sectionnameinput, subjectinput, YAGinput, qualinput) {
  sankey_data <- regional_movement_data %>%
    filter(
      SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput,
      qualification_TR == qualinput
    ) %>%
    filter(is.na(count) != TRUE)

  nodes <- data.frame("name" = c(
    unique(sankey_data$InstRegion),
    unique(sankey_data$current_region)
  ))

  nodes$ID <- 0:(nrow(nodes) - 1)
  nodes1 <- nodes[1:length(unique(sankey_data$InstRegion)), ]
  nodes2 <- nodes[(length(unique(sankey_data$InstRegion)) + 1):nrow(nodes), ]

  links <- as.data.frame(
    sankey_data[, c(4, 5, 7)],
    byrow = TRUE, ncol = 3
  )

  names(links) <- c("source", "target", "value")

  # Change names in links to numbers

  links <- links %>%
    left_join(nodes1, by = c("source" = "name"))
  links$source <- links$ID
  links <- links[, -4]

  links <- links %>%
    left_join(nodes2, by = c("target" = "name"))
  links$target <- links$ID
  links <- links[, -4]

  links <- links %>%
    mutate_at(
      "value",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    ) %>%
    filter(value != 0)

  plot <- sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name", fontSize = 14
  )
  return(plot)
}

regional_sankey_title <- function(sectionnameinput, subjectinput, YAGinput, qualinput) {
  ifelse(subjectinput == "All",
    subjecttext <- "all subjects",
    subjecttext <- subjectinput
  )

  regional_sankey_title <- paste("<h4> Number of graduates working in the", sectionnameinput, " industry who
                      studied in each region, and where they currently live", YAGinput, "years after graduation.</h4>")

  return(regional_sankey_title)
}


# CROSSTABS ---------------------------------------------------------------

crosstabs <- function(subjectinput, YAGinput, countinput, qualinput, buttoninput, thresholdinput) {
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
        ) %>%
        group_by(ethnicity) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
        ) %>%
        group_by(current_region) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
        ) %>%
        group_by(FSM) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
          prior_attainment == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
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
          prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name != "All"
        ) %>%
        group_by(sex) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", current_region == "All", FSM == "All",
          prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name != "All"
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
          prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name != "All"
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
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
        current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name == "All"
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
          current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
        ) %>%
        group_by(prior_attainment) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
          current_region == "All", qualification_TR == "First degree", threshold == thresholdinput, group_name != "All"
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
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
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
        prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name == "All"
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
          prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name != "All"
        ) %>%
        group_by(subject_name) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", YAG == YAGinput, ethnicity == "All", FSM == "All", current_region == "All",
          prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name != "All"
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
          prior_attainment == "All", qualification_TR == qualinput, threshold == thresholdinput, group_name != "All"
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
        current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name == "All"
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
        current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name == "All"
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
        current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name == "All"
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
          current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name != "All"
        ) %>%
        group_by(qualification_TR) %>%
        mutate(prop = count / sum(count, na.rm = TRUE))

      nested_table <- tables_data_nested[tables_data_nested$SECTIONNAME == crosstabs_data$SECTIONNAME[index], ] %>%
        filter(
          sex == "F+M", subject_name == subjectinput, YAG == YAGinput, ethnicity == "All", FSM == "All",
          current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name != "All"
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
          current_region == "All", prior_attainment == "All", threshold == thresholdinput, group_name != "All"
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


# Run the application -----------------------------------------------------

# shinyApp(ui = ui, server = server)
