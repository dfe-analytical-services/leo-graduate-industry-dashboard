# REGIONAL ---------------------------------------------------------------------

### This code defines lots of region related functions



# ukRegions <- st_read("data/boundaries/RGN_DEC_2023_EN_BFE.shp", quiet = TRUE)
#
# ukRegions <- ukRegions[order(ukRegions$RGN23NM), ]
# ukRegions$RGN23NM[ukRegions$RGN23NM == "Yorkshire and The Humber"] <- "Yorkshire and the Humber"
#
# data$SECTIONNAME <- StrCap(tolower(data$SECTIONNAME))
# regional_movement_data$SECTIONNAME <- StrCap(tolower(regional_movement_data$SECTIONNAME))

format_filtervalues_region <- function(filtervalues) {
  filtervalues <- sort(unique(filtervalues))
  if (length(filtervalues) == 1) {
    return(paste("was <b>", filtervalues, "</b>", sep = ""))
  } else if (length(filtervalues) > 1) {
    return(paste0("were <b>", paste0(filtervalues[1:length(filtervalues) - 1], collapse = ", "), "</b> and <b>", filtervalues[length(filtervalues)], "</b>", sep = ""))
  } else if ((length(filtervalues) == 0)) {
    return(paste("there is no data"))
  }
}
regions <- function(regionsdata) {
  if (nrow(regionsdata) >= 1) {
    regions <- format_filtervalues_region(regionsdata$region)
    return(regions)
  }
}
pluralregion <- function(regionsdata) {
  if (nrow(regionsdata) == 1) {
    return(paste0("region"))
  } else if (nrow(regionsdata) > 1) {
    return(paste0("regions"))
  }
}

# Create the map

create_maptabledata <- function(regional_data, regional_movement,
                                sectionnameinput, subjectinput,
                                YAGinput, qualinput) {
  # This function creates one central table that the other functions can then
  # call on. I've put the output of this function into a reactive() in server.R.
  mapdata <- regional_data %>%
    filter(
      SECTIONNAME == sectionnameinput, subject_name == subjectinput,
      YAG == YAGinput, qualification_TR == qualinput
    )
  mapdata <- left_join(ukRegions, mapdata, by = c("RGN23NM" = "region"))

  mapdata2 <- regional_movement %>%
    filter(
      SECTIONNAME == sectionnameinput, subject_name == subjectinput,
      YAG == YAGinput, qualification_TR == qualinput
    ) %>%
    # Cathie changed mutate_at to mutate(across())
    #    mutate_at(
    #     "count",
    #    funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    # )
    mutate(across(c("count"), ~ ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .)))


  instregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, InstRegion, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(trained_in_region2 = sum(count), .groups = "drop") %>%
    ungroup() %>%
    select(InstRegion, trained_in_region2)

  currentregion <- mapdata2 %>%
    filter(is.na(count) != TRUE) %>%
    group_by(YAG, current_region, subject_name, SECTIONNAME, qualification_TR) %>%
    summarise(living_in_region2 = sum(count), .groups = "drop") %>%
    ungroup() %>%
    select(current_region, living_in_region2)

  mapdata <- mapdata %>%
    left_join(instregion, by = c("RGN23NM" = "InstRegion")) %>%
    left_join(currentregion, by = c("RGN23NM" = "current_region"))

  mapdata <- mapdata %>%
    mutate(difference2 = ifelse(is.na(living_in_region2) == TRUE & is.na(trained_in_region2) == TRUE, NA, replace(mapdata$living_in_region2, is.na(mapdata$living_in_region2), 0) - replace(mapdata$trained_in_region2, is.na(mapdata$trained_in_region2), 0))) %>%
    mutate(difference_prop2 = difference2 / replace(mapdata$trained_in_region2, is.na(mapdata$trained_in_region2), 0)) %>%
    rename(region = RGN23NM)

  mapdata$difference_prop2 <- readr::parse_number(
    scales::percent(mapdata$difference_prop2, accuracy = 0.1)
  )
  return(mapdata)
}

map_chart <- function(mapdata, countinput) {
  mapdata <- mapdata %>%
    # Cathie changed mutate_at to mutate(across())
    #    mutate_at(
    #     "earnings_median",
    #    funs(ifelse(. < 0, "c", .))
    # ) %>%
    mutate(across(c("earnings_median"), ~ ifelse(. < 0, "c", .))) %>%
    #    mutate_at(
    #     c("trained_in_region", "living_in_region", "difference", "difference_prop", "number_of_providers", "earnings_median"),
    #    funs(ifelse(is.na(.), "x", .))
    # )
    mutate(across(
      c("trained_in_region", "living_in_region", "difference", "difference_prop", "number_of_providers", "earnings_median"),
      ~ ifelse(is.na(.), "x", .)
    ))

  leafletmapdata <- st_transform(mapdata, crs = 4326)

  domain_lim_difference <- max(c(abs(max(leafletmapdata$difference2, na.rm = TRUE)), abs(min(leafletmapdata$difference2, na.rm = TRUE))))
  domain_lim_difference_prop <- max(c(abs(max(leafletmapdata$difference_prop2, na.rm = TRUE)), abs(min(leafletmapdata$difference_prop2, na.rm = TRUE))))

  if (countinput == "trained_in_region") {
    pal_fun <- colorNumeric("Blues", domain = c(0, max(leafletmapdata$trained_in_region2, na.rm = TRUE)))
    fill_fun <- ~ pal_fun(trained_in_region2)
    value_fun <- ~ leafletmapdata$trained_in_region2
    title_fun <- "studied in the region"
  }

  if (countinput == "living_in_region") {
    pal_fun <- colorNumeric("Blues", domain = c(0, max(leafletmapdata$living_in_region2, na.rm = TRUE)))
    fill_fun <- ~ pal_fun(living_in_region2)
    value_fun <- ~ leafletmapdata$living_in_region2
    title_fun <- "living in the region"
  }

  if (countinput == "difference") {
    pal_fun <- colorNumeric("RdBu", domain = c(-domain_lim_difference, domain_lim_difference))
    fill_fun <- ~ pal_fun(difference2)
    value_fun <- ~ leafletmapdata$difference2
    title_fun <- "Difference"
  }

  if (countinput == "difference_prop") {
    pal_fun <- colorNumeric("RdBu", domain = c(-domain_lim_difference_prop, domain_lim_difference_prop))
    fill_fun <- ~ pal_fun(difference_prop2)
    value_fun <- ~ leafletmapdata$difference_prop2
    title_fun <- "Proportionate difference"
  }

  p_popup <- paste(
    "<B>", leafletmapdata$region, "</B>", br(), br(),
    "Number who studied in region:        ", prettyNum(leafletmapdata$trained_in_region2, big.mark = ",", scientific = FALSE), br(),
    "Number who lived in region in 2021-22 tax year: ", prettyNum(leafletmapdata$living_in_region2, big.mark = ",", scientific = FALSE), br(),
    "Difference in graduate numbers:      ", prettyNum(leafletmapdata$difference2, big.mark = ",", scientific = FALSE), br(),
    "Difference in proportion:            ", round(leafletmapdata$difference_prop2, digits = 1), "%", br(),
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

  if (YAGinput == 1) {
    YAGtext <- "one year"
  } else if (YAGinput == 3) {
    YAGtext <- "three years"
  } else if (YAGinput == 5) {
    YAGtext <- "five years"
  } else if (YAGinput == 10) {
    YAGtext <- "ten years"
  }

  if (countinput == "trained_in_region") {
    counttext <- paste("number of graduates who studied in each region:")
  } else if (countinput == "living_in_region") {
    counttext <- paste("number of graduates who lived in each region during the 2021-22 tax year:")
  } else if (countinput == "difference") {
    counttext <- paste("difference between the numbers of graduates who lived in each region during the 2021-22 tax year and the numbers who studied there:")
  } else if (countinput == "difference_prop") {
    counttext <- paste("percentage difference between the numbers of graduates who lived in each region during the 2021-22 tax year and the numbers who studied there:")
  }
  map_title <- paste(
    "<h4> Map showing the ",
    counttext,
    " among ",
    tolower(qualinput),
    "graduates of all sexes from English HE providers who studied ",
    subjecttext,
    " and worked in ",
    sectionnameinput,
    " during the 2021-22 tax year, ",
    YAGtext,
    " after graduation.</h4>"
    #    after graduation during the ",
    #    tax_year_slash, " tax year.</h4>"
  )
  return(map_title)
}

map_text <- function(mapdata, sectionnameinput, subjectinput,
                     YAGinput, qualinput) {
  if (YAGinput == 1) {
    YAGtext <- "one year"
  } else if (YAGinput == 3) {
    YAGtext <- "three years"
  } else if (YAGinput == 5) {
    YAGtext <- "five years"
  } else if (YAGinput == 10) {
    YAGtext <- "ten years"
  }

  mapdata <- mapdata %>% as.data.frame()
  ifelse(subjectinput == "All",
    subjecttext <- paste("For", tolower(qualinput), "graduates of all subjects"),
    subjecttext <- paste("For", tolower(qualinput), "graduates of", subjectinput)
  )

  mapdata_trained <- mapdata %>%
    arrange(-trained_in_region2)
  mapdata_trained_highest <- mapdata_trained %>%
    filter(trained_in_region2 == first(mapdata_trained$trained_in_region2))
  mapdata_trained_lowest <- mapdata_trained %>%
    filter(trained_in_region2 == last(mapdata_trained$trained_in_region2))

  mapdata_current <- mapdata %>%
    arrange(-living_in_region2)
  mapdata_current_highest <- mapdata_current %>%
    filter(living_in_region2 == first(mapdata_current$living_in_region2))
  mapdata_current_lowest <- mapdata_current %>%
    filter(living_in_region2 == last(mapdata_current$living_in_region2))

  if (first(is.na(mapdata_trained$trained_in_region2)) == FALSE & first(mapdata_trained$trained_in_region2) > 0) {
    highest_studied <- paste0(subjecttext, " in the ", sectionnameinput, " industry ", YAGtext, " after graduation, the ", pluralregion(mapdata_trained_highest), " where
                    the most graduates had studied ", regions(mapdata_trained_highest), ".", sep = "")
  } else if (first(mapdata_trained$trained_in_region2) == 0 | is.na(first(mapdata_trained$trained_in_region2)) == TRUE) {
    highest_studied <- ""
  }

  if (last(is.na(mapdata_trained$trained_in_region2)) == FALSE) {
    fewest_studied <- paste0(" The ", pluralregion(mapdata_trained_lowest), " where the fewest graduates
                    had studied ", regions(mapdata_trained_lowest), ".", sep = "")
  } else if (is.na(last(mapdata_trained$trained_in_region2)) == TRUE) {
    fewest_studied <- ""
  }

  if (first(is.na(mapdata_current$living_in_region2)) == FALSE & first(mapdata_current$living_in_region2) > 0) {
    highest_current <- paste0(" The ", pluralregion(mapdata_current_highest), " where the highest number of graduates lived
                    ", YAGtext, " after graduation ", regions(mapdata_current_highest), sep = "")
  } else if (first(mapdata_current$living_in_region2) == 0 | is.na(first(mapdata_current$living_in_region2)) == TRUE) {
    highest_current <- ""
  }

  if (last(is.na(mapdata_current$living_in_region2)) == FALSE) {
    fewest_current <- paste0(" and the ", pluralregion(mapdata_current_lowest), " where the
                    fewest graduates lived ", regions(mapdata_current_lowest), ".", sep = "")
  } else if (is.na(last(mapdata_current$living_in_region2)) == TRUE) {
    fewest_current <- paste0(".")
  }

  if (paste0(highest_studied, fewest_studied, highest_current, fewest_current) == ".") {
    nosummary <- "There is no summary for this selection"
  } else {
    nosummary <- ""
  }

  map_text <- paste0(highest_studied, fewest_studied, highest_current, nosummary, fewest_current,
    sep = ""
  )

  return(map_text)
}

map_text2 <- function(mapdata, sectionnameinput, subjectinput,
                      YAGinput, qualinput) {
  if (YAGinput == 1) {
    YAGtext <- "one year"
  } else if (YAGinput == 3) {
    YAGtext <- "three years"
  } else if (YAGinput == 5) {
    YAGtext <- "five years"
  } else if (YAGinput == 10) {
    YAGtext <- "ten years"
  }

  mapdata <- mapdata %>% as.data.frame()

  ifelse(subjectinput == "All",
    subjecttext <- paste("For", tolower(qualinput), "graduates of all subjects"),
    subjecttext <- paste("For", tolower(qualinput), "graduates of", subjectinput)
  )

  mapdata_diff_prop <- mapdata %>%
    arrange(-difference_prop2)


  clean_map_data <- mapdata_diff_prop %>%
    filter(!is.na(difference_prop2)) %>%
    select(region, difference_prop2)
  clean_map_data_highest <- clean_map_data %>%
    filter(difference_prop2 == first(clean_map_data$difference_prop2))
  clean_map_data_lowest <- clean_map_data %>%
    filter(difference_prop2 == last(clean_map_data$difference_prop2))


  if (nrow(clean_map_data) > 1) {
    if (first(clean_map_data$difference_prop2) > 0) {
      max_text <- paste0(
        "the ", pluralregion(clean_map_data_highest),
        " with the highest proportionate increase in the number of graduates living in the region during the 2021-22 tax year ",
        YAGtext, " after graduation compared to the number who graduated from HE providers in the same region <b>",
        regions(clean_map_data_highest),
        ", </b> where the number of graduates increased by <b>",
        first(clean_map_data$difference_prop2),
        "%</b>. "
      )
    } else if (first(clean_map_data$difference_prop2) < 0) {
      max_text <- paste0(
        "the ", pluralregion(clean_map_data_highest),
        " with the smallest proportionate <b>decrease</b> in the number of graduates living in the region during the 2021-22 tax year ",
        #        tax_year_slash,
        #       ") ",
        YAGtext, " after graduation compared to the number who graduated from HE providers in the same region <b>",
        regions(clean_map_data_highest),
        ".</b> In this region, the change in the number of graduates was </b>",
        first(clean_map_data$difference_prop2),
        "%</b>, where a negative number represents a proportionate decrease."
      )
    } else {
      max_text <- paste0(
        "the ", pluralregion(clean_map_data_highest), " with the most graduates living there during the 2021-22 tax year ",
        #        tax_year_slash,
        #       ") ",
        YAGtext, " after graduation compared to the number who graduated from HE providers in the same region <b>",
        regions(clean_map_data_highest),
        ",</b> where the numbers of graduates at the two time points is the same."
      )
    }

    if (last(clean_map_data$difference_prop2) > 0) {
      min_text <- paste0(
        "The ", pluralregion(clean_map_data_lowest), " with the smallest increase ",
        regions(clean_map_data_lowest), " where the number of graduates increased by <b>",
        last(clean_map_data$difference_prop2), "%</b>."
      )
    } else if (last(clean_map_data$difference_prop2) < 0) {
      min_text <- paste0(
        "The ", pluralregion(clean_map_data_lowest), " with the largest decrease ",
        regions(clean_map_data_lowest), " where the number of graduates changed by <b>",
        last(clean_map_data$difference_prop2), "%</b> (the negative number indicates a decrease)."
      )
    } else {
      min_text <- paste0(
        "The ", pluralregion(clean_map_data_lowest), " with the fewest graduates living there ",
        YAGtext, " after graduation, compared to the number having studied there ",
        regions(clean_map_data_highest),
        ", where the number of graduates living in the region during the 2021-22 tax year was the same as the number who graduated from HE providers in the same region."
      )
    }


    map_text <- paste0(
      subjecttext, " in the ", sectionnameinput,
      " industry, ", max_text, min_text
    )
  } else {
    # If the data is a full tranch of NAs or only has 1 row, then return a blank.
    map_text <- ""
  }

  return(map_text)
}


create_regions_table <- function(maptabledata, regioninput) {
  cellfunc <- function(value) {
    if (is.na(value)) {
      "x"
    } else if (value < 0) "c" else paste0("£", format(value, big.mark = ","))
  }

  dftable <- maptabledata %>%
    as.data.frame() %>%
    select(
      region, "trained_in_region2", "living_in_region2", "difference2",
      "difference_prop2", "number_of_providers", "earnings_median"
    ) %>%
    filter(region %in% c(regioninput))
  map_table <- reactable(dftable,
    sortable = TRUE, resizable = TRUE, showSortable = TRUE,
    highlight = TRUE, fullWidth = TRUE,
    columns = list(
      region = colDef(name = "Region"),
      trained_in_region2 = colDef(name = "Studied in region", format = colFormat(separators = TRUE), na = "x"),
      living_in_region2 = colDef(name = "Living in region", format = colFormat(separators = TRUE), na = "x"),
      number_of_providers = colDef(
        name = "Number of providers", style = list(backgroundColor = "#f7f7f7"),
        headerStyle = list(backgroundColor = "#f7f7f7"), na = "x"
      ),
      difference2 = colDef(name = "Difference", format = colFormat(separators = TRUE), na = "x"),
      difference_prop2 = colDef(name = "Difference (%)", na = "x"),
      earnings_median = colDef(
        name = "Median earnings",
        style = list(backgroundColor = "#f7f7f7"),
        headerStyle = list(backgroundColor = "#f7f7f7"), cell = cellfunc
      )
    ),
    columnGroups = list(
      colGroup(name = "Outcomes of interest", columns = c("living_in_region2", "trained_in_region2", "difference2", "difference_prop2")),
      colGroup(name = "Context", columns = c("number_of_providers", "earnings_median"))
    )
  )
  return(map_table)
}

create_regionalsankeyframe <- function(sectionnameinput, subjectinput, YAGinput, qualinput) {
  sankey_data <- regional_movement_data %>%
    filter(
      SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput,
      qualification_TR == qualinput
    ) %>%
    filter(is.na(count) != TRUE, count != 0)

  nodes <- data.frame("name" = c(
    unique(sankey_data$InstRegion),
    unique(sankey_data$current_region)
  ))
  links <- as.data.frame(
    sankey_data[, c(3, 4, 6)],
    byrow = TRUE, ncol = 3
  )

  names(links) <- c("source", "target", "value")

  # Change names in links to numbers

  if (nrow(nodes) >= 1) {
    nodes$ID <- 0:(nrow(nodes) - 1)
    nodes1 <- nodes[1:length(unique(sankey_data$InstRegion)), ]
    nodes2 <- nodes[(length(unique(sankey_data$InstRegion)) + 1):nrow(nodes), ]
    links <- links %>%
      left_join(nodes1, by = c("source" = "name"))
    links$source <- links$ID
    links <- links[, -4]

    links <- links %>%
      left_join(nodes2, by = c("target" = "name"))
    links$target <- links$ID
    links <- links[, -4]

    links <- links %>%
      # Cathie changed mutate_at() to mutate(across)
      #      mutate_at(
      #       "value",
      #      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
      #   ) %>%
      mutate(across(c("value"), ~ ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))) %>%
      filter(value != 0)

    # Force a space between node names and values
    nodes$name <- paste(nodes$name, " ")
  }
  list(links = links, nodes = nodes)
}

regional_sankey <- function(links, nodes) {
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

  if (YAGinput == 1) {
    YAGtext <- "one year"
  } else if (YAGinput == 3) {
    YAGtext <- "three years"
  } else if (YAGinput == 5) {
    YAGtext <- "five years"
  } else if (YAGinput == 10) {
    YAGtext <- "ten years"
  }

  regional_sankey_title <- paste("<h4> Number of graduates working in the", sectionnameinput, " industry who
                      studied in each region, and where they lived during the 2021-22 tax year, ", YAGtext, " after graduation.</h4>")

  return(regional_sankey_title)
}
