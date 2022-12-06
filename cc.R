cohort_sankey1 <- cohort1 %>%
  filter(subject_name == 'Economics', sex.x == 'F+M', qualification_TR.x == 'First degree') %>%
  filter(count != 0)

cohort_sankey2 <- cohort2 %>%
  filter(subject_name == 'Economics', sex.x == 'F+M', qualification_TR.x == 'First degree') %>%
  filter(count != 0)

cohort_sankey1 <- na.omit(cohort_sankey1)
cohort_sankey2 <- na.omit(cohort_sankey2)
if (all(cohort_sankey1$count == 0) | all(is.na(cohort_sankey1$count))) {
  return(NULL)
} else {
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

  
  cohort_sankey1 <- cohort_sankey1 %>%
    group_by(sex.x, subject_name, YAG.x, SECTIONNAME.x, YAG.y, SECTIONNAME.y) %>%
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
    group_by(sex.x, subject_name, YAG.x, SECTIONNAME.x, YAG.y, SECTIONNAME.y) %>%
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
  
  # Force a space between node names and values
  nodes$name <- paste(nodes$name, " ")
  
  plot <- sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name", fontSize = 10, nodePadding = 20
  )
  
  plot
}