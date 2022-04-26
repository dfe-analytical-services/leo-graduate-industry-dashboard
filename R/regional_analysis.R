# REGIONAL ---------------------------------------------------------------------


data <- read.csv("data/regional_data_with_pg_dummy.csv")

regional_movement_data <- read.csv("data/regional_movement_with_pg_dummy.csv")

ukRegions <- st_read("data/boundaries/Regions__December_2019__Boundaries_EN_BFE.shp", quiet = TRUE)

ukRegions <- ukRegions[order(ukRegions$rgn19nm), ]
ukRegions$rgn19nm[ukRegions$rgn19nm == "Yorkshire and The Humber"] <- "Yorkshire and the Humber"

data$SECTIONNAME <- StrCap(tolower(data$SECTIONNAME))
regional_movement_data$SECTIONNAME <- StrCap(tolower(regional_movement_data$SECTIONNAME))

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
  mapdata <- left_join(ukRegions, mapdata, by = c("rgn19nm" = "region"))

  mapdata2 <- regional_movement %>%
    filter(
      SECTIONNAME == sectionnameinput, subject_name == subjectinput,
      YAG == YAGinput, qualification_TR == qualinput
    ) %>%
    mutate_at(
      "count",
      funs(ifelse(!is.na(as.numeric(.)), round_any(as.numeric(.), 5), .))
    )

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
    left_join(instregion, by = c("rgn19nm" = "InstRegion")) %>%
    left_join(currentregion, by = c("rgn19nm" = "current_region")) %>%
    mutate(difference2 = living_in_region2 - trained_in_region2) %>%
    mutate(difference_prop2 = difference2 / trained_in_region2) %>%
    rename(region = rgn19nm)

  mapdata$difference_prop2 <- readr::parse_number(
    scales::percent(mapdata$difference_prop2, accuracy = 0.1)
  )
  return(mapdata)
}

map_chart <- function(mapdata, countinput) {
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
    "<B>", leafletmapdata$region, "</B>", br(), br(),
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
  map_title <- paste("<h4> Map to show the ", counttext, YAGtext, " after
                          graduation, male and female", qualinput, "graduates from English HEIs, APs and FECs,
                            2018/19 tax year.</h4>")
  return(map_title)
}

map_text <- function(mapdata, sectionnameinput, subjectinput,
                     YAGinput, qualinput) {
  mapdata <- mapdata %>% as.data.frame()
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

  map_text <- paste0(subjecttext, " in the ", sectionnameinput, " industry ", YAGinput, " years after graduation, the region where
                    the most graduates had studied was <b>", first(mapdata_trained$region), "</b>. The region where the least graduates
                    had studied was <b>", last(mapdata_trained$region), "</b>. The region where the highest number of graduates lived
                    ", YAGinput, " years after graduation was <b>", first(mapdata_current$region), "</b> and the region with the
                    least graduates lived was <b>", last(mapdata_current$region), "</b>.",
    sep = ""
  )

  return(map_text)
}

map_text2 <- function(mapdata, sectionnameinput, subjectinput,
                      YAGinput, qualinput) {
  mapdata <- mapdata %>% as.data.frame()

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

  clean_map_data <- mapdata_diff_prop %>%
    filter(!is.na(difference_prop2)) %>%
    select(region, difference_prop2)
  print(clean_map_data)
  if (nrow(clean_map_data) >= 1) {
    if (first(clean_map_data$difference_prop2) > 0) {
      max_text <- paste0(
        "the region with the highest proportionate increase in graduates who studied there compared to living there ",
        YAGinput, " years after graduation was <b>", first(clean_map_data$region),
        "</b>, where the number of graduates increased by <b>",
        first(clean_map_data$difference_prop2),
        "%</b>. "
      )
    } else if (first(clean_map_data$difference_prop2) < 0) {
      max_text <- paste0(
        "the region with the smallest proportionate decrease in graduates who studied there compared to living there ",
        YAGinput, " years after graduation was <b>", first(clean_map_data$region),
        "</b>, where the number of graduates decreased by <b>",
        first(clean_map_data$difference_prop2),
        "%</b>. "
      )
    } else {
      max_text <- paste0(
        "the region with the most graduates living there ",
        YAGinput, " years after graduation, compared to the number having studied there was <b>",
        first(clean_map_data$region),
        "</b>, where the number of graduates was the same as the number of students."
      )
    }


maptable <- function(sectionnameinput, subjectinput, countinput, YAGinput, regioninput, qualinput) {
  cellfunc <- function(value) {
    if (is.na(value)) {
      "x"
    } else if (value < 0) "c" else paste0("£", format(value, big.mark = ","))
  }

  mapdata <- data %>%
    filter(SECTIONNAME == sectionnameinput, subject_name == subjectinput, YAG == YAGinput, region %in% c(regioninput), qualification_TR == qualinput)

    if (last(clean_map_data$difference_prop2) > 0) {
      min_text <- paste0(
        "The region with the smallest increase is <b>",
        last(clean_map_data$region), "</b> where the number of graduates increased by <b>",
        last(clean_map_data$difference_prop2), "%</b>."
      )
    } else if (last(clean_map_data$difference_prop2) < 0) {
      min_text <- paste0(
        "The region with the largest decrease is <b>",
        last(clean_map_data$region), "</b> where the number of graduates decreased by <b>",
        last(clean_map_data$difference_prop2), "%</b>."
      )
    } else {
      min_text <- paste0(
        "The region with the fewest graduates living there ",
        YAGinput, " years after graduation, compared to the number having studied there was <b>",
        first(clean_map_data$region),
        "</b>, where the number of graduates was the same as the number of students."
      )
    }


    map_text <- paste0(
      subjecttext, " in the ", sectionnameinput,
      " industry, ", max_text, min_text
    )
  } else {
    # If the data is a full tranch of NAs, then return a blank.
    map_text <- ""
  }

  return(map_text)
}

create_regions_table <- function(maptabledata, regioninput) {
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
        # format = colFormat(prefix = "£", separators = TRUE, digits = 0),
        style = list(backgroundColor = "#f7f7f7"),
        headerStyle = list(backgroundColor = "#f7f7f7"), cell = cellfunc
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
                      studied in each region, and where they currently live", YAGtext, " after graduation.</h4>")

  return(regional_sankey_title)
}
