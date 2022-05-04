# Create sankey chart, inputs based on server logic from ui inputs.


# Sankey chart ------------------------------------------------------------

sankey_chart <- function(subjectinput, sexinput, qualinput) {
  cohort_sankey1 <- cohort1 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey2 <- cohort2 %>%
    filter(subject_name.x == subjectinput, sex.x == sexinput, qualification_TR.x == qualinput)

  cohort_sankey1 <- na.omit(cohort_sankey1)
  cohort_sankey2 <- na.omit(cohort_sankey2)

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

  # The following line had disappeared from my branch compared to main branch.
  # I didn't mean to delete it and assuming no-one else has been on the branch,
  # so assuming it was an accidental deletion.
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


  sankey_title <- paste("<h3>Industry of graduate employment for 2012/13 academic year graduates of", subjecttext, "one, three and five years after
                          graduation (YAG), ", sextext, tolower(qualinput), "graduates from English HEIs, APs and FECs, 2018/19 tax year</h3>")

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
    list(color = "#000000", background = color)
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
    sankeytext1 <- paste("For ", sextext, ", ", tolower(qualinput), " graduates of all subjects, the industry with the highest
                      proportion of graduates one year after graduation is <b>", first(yag_table_final$SECTIONNAME.x),
      " (", first(yag_table_final$`1 YAG`), "%)</b>, and at five years after graduation it is <b>",
      ifelse(first(five_yag_table_subject$SECTIONNAME.x) == first(yag_table_final$SECTIONNAME.x),
        "the same",
        first(five_yag_table_subject$SECTIONNAME.x)
      ), " (", first(five_yag_table_subject$`5 YAG`), "%)</b>.",
      sep = ""
    ),
    sankeytext1 <- paste(
      "For ", sextext, ", ", tolower(qualinput), " graduates who studied", subjectinput, ", the industry with the highest
                      proportion of graduates one year after graduation is <b>", first(yag_table_final$SECTIONNAME.x),
      "</b> (", first(yag_table_final$`1 YAG`), "%), and at five years after graduation it is <b>",
      ifelse(first(five_yag_table_subject$SECTIONNAME.x) == first(yag_table_final$SECTIONNAME.x),
        "the same",
        first(five_yag_table_subject$SECTIONNAME.x)
      ), " (", first(five_yag_table_subject$`5 YAG`), "%)</b>."
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

  sankeytext2 <- paste(
    "The most movement between one and three years after graduation is seen for <b>",
    first(cohort_sankey1_text$SECTIONNAME.x), "</b>, where </b>", first(cohort_sankey1_text$count),
    " graduates move to <b>", first(cohort_sankey1_text$SECTIONNAME.y), "</b>. Between three and five
                       years after graduation it's seen for <b>", first(cohort_sankey2_text$SECTIONNAME.x), "</b> where <b>",
    first(cohort_sankey2_text$count), "</b> graduates moved to <b>", first(cohort_sankey2_text$SECTIONNAME.y),
    "</b>."
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
    list(color = "#000000", background = color)
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
    defaultColDef = colDef(footerStyle = list(color = "#000000", fontWeight = "bold"))
  )
}
