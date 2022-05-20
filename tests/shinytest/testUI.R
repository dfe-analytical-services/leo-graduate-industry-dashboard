clean_json <- function(file, parent_script = "testUI", basedir = "./", iteration = "current") {
  # Reactable outputs some random keys as part of its table rendering, which snag
  # in shinytest when run across different machines. There's probably a way to fix
  # fix this in shinytest itself, but I gave up on trying and did a post-process
  # fix instead.
  filepath <- paste0(basedir, parent_script, "-", iteration, "/", file)
  shinytest_json <- readr::read_lines(filepath)
  for (i in 1:length(shinytest_json)) {
    shinytest_json[i] <- gsub('ey\": \".*\"', 'ey\": "random_key"', shinytest_json[i])
    shinytest_json[i] <- gsub("\r\n", "\n", shinytest_json[i])
  }
  readr::write_lines(shinytest_json, filepath)
}

run_set_shinytests <- function(dfinputs, outstring, listrecords) {
  # This function loops through a set of inputs and takes a snapshot for each one.
  # dfinputs: data frame containing field list and value list.
  # outstring: the stem for the output filename.
  # listrecords: list of input and output variables to record the values of.
  for (i in 1:nrow(dfinputs)) {
    file <- paste0(outstring, "_", i - 1, ".json")
    eval(parse(text = paste0("app$setInputs(", dfinputs$field[i], '="', dfinputs$value[i], '", timeout_ = 1e+4, values_ = FALSE)')))
    app$snapshot(
      items = listrecords,
      filename = file
    )
    clean_json(file)
  }
}

# Run the shiny tests.
# Should really set this up to loop over arrays of inputs in order to run
# through the shiny tests.
app <- ShinyDriver$new("../../", loadTimeout = 1.6e+05, seed = 2011)

app$snapshotInit("testUI", screenshot = FALSE)

# Industry flow tab ===========================================================

industryFlow_input <- c("navbar", "qualinput", "sexinput", "indflow.subjectinput")
industryFlow_output <- c(
  "sankey", "sankey_title", "sankeyhelp", "sankeysubjectlist",
  "sankeytext1", "sankeytext2"
)

app$setInputs(navbar = "industryFlow", timeout_ = 1e+4)
app$snapshot(
  items = list(
    input = industryFlow_input,
    output = industryFlow_output
  ),
  filename = "industryFlow_1.json"
)
app$setInputs(qualinput = "Level 7 (taught)", timeout_ = 4e+4)
app$snapshot(
  items = list(
    input = industryFlow_input,
    output = industryFlow_output
  ),
  filename = "industryFlow_2.json"
)
app$setInputs(qualinput = "Level 7 (research)", timeout_ = 4e+4)
app$snapshot(
  items = list(
    input = industryFlow_input,
    output = industryFlow_output
  ),
  filename = "industryFlow_3.json"
)
app$setInputs(qualinput = "Level 8", timeout_ = 4e+4)
app$snapshot(
  items = list(
    input = industryFlow_input,
    output = industryFlow_output
  ),
  filename = "industryFlow_4.json"
)
app$setInputs(sexinput = "F", timeout_ = 4e+4)
app$snapshot(
  items = list(
    input = industryFlow_input,
    output = industryFlow_output
  ),
  filename = "industryFlow_5.json"
)
app$setInputs(sexinput = "M", timeout_ = 4e+4)
app$snapshot(
  items = list(
    input = industryFlow_input,
    output = industryFlow_output
  ),
  filename = "industryFlow_6.json"
)

# Industry by subject tab =====================================================

RecsIndustryBySubject <- list(
  input = c(
    "navbar", "countinput3", "YAGinput3", "qualinput4",
    "sectionnameinput2", "earningsbutton2"
  ),
  output = c("backwards_crosstab_title", "crosstab_backwards")
)

dfTestInputs <- data.frame(
  field = c(
    "navbar", "earningsbutton2", "countinput3", "YAGinput3",
    "sectionnameinput2", "YAGinput3",
    "sectionnameinput2", "countinput3", "sectionnameinput2",
    "earningsbutton2"
  ),
  value = c(
    "industryBySubject", "Median earnings", "ethnicity", "10",
    "PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY", "1",
    "Mining and quarrying", "FSM", "Construction",
    "Proportions"
  )
)
run_set_shinytests(dfTestInputs, "industryBySubject", RecsIndustryBySubject)


# Subject by industry tab =====================================================

subjectByIndustry_input <- c(
  "navbar", "countinput2", "YAGinput2", "crosstabs.subjectinput", "earningsbutton", "qualinput3"
)

# Note that I've excluded the crosstab_backwards tabulated output here as it
# has a datakey that changes across different runs.
subjectByIndustry_output <- c("crosstab_title", "crosstab", "crosstab_text")

app$setInputs(navbar = "subjectByIndustry", timeout_ = 2.e4)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_0.json"
)
clean_json("subjectByIndustry_0.json")

app$setInputs(earningsbutton = "Median earnings", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_1.json"
)
clean_json("subjectByIndustry_1.json")


app$setInputs(qualinput3 = "Level 8", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_2.json"
)
clean_json("subjectByIndustry_2.json")

app$setInputs(crosstabs.subjectinput = "Allied health", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_3.json"
)
clean_json("subjectByIndustry_3.json")


app$setInputs(countinput2 = "ethnicity", timeout_ = 1e+4)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_4.json"
)
clean_json("subjectByIndustry_4.json")

app$setInputs(YAGinput2 = "5", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_5.json"
)
clean_json("subjectByIndustry_5.json")

app$setInputs(YAGinput2 = "1", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_6.json"
)
clean_json("subjectByIndustry_6.json")

app$setInputs(crosstabs.subjectinput = "English studies", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_7.json"
)
clean_json("subjectByIndustry_7.json")

app$setInputs(earningsbutton = "Proportions", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_8.json"
)
clean_json("subjectByIndustry_8.json")

app$setInputs(crosstabs.subjectinput = "Physics and astronomy", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_9.json"
)
clean_json("subjectByIndustry_9.json")


# Regional tab ================================================================

# Run tests for regional tab - note: excluding the map output as it makes the
# json files massive.
regional_input <- c(
  "navbar", "regions.subjectinput", "YAGinput", "qualinput2",
  "sectionnameinput"
)
regional_output <- c("map_title", "mapsubjectlist", "maptext", "maptext2")

app$setInputs(navbar = "regional", timeout_ = 1e+4)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_0.json"
)
app$setInputs(regioninput = c("London", "North East", "West Midlands"), timeout_ = 2e+4)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_1.json"
)
app$setInputs(qualinput2 = "Level 7 (research)")
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_2.json"
)
app$setInputs(sectionnameinput = "TRANSPORTATION AND STORAGE")
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_3.json"
)
app$setInputs(regions.subjectinput = "Medicine and dentistry")
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_4.json"
)
app$setInputs(sectionnameinput = "EDUCATION", countinput = "living_in_region")
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_5.json"
)

# Note: for the following test, when YAG input is changed, subjectinput2
# automatically reverts to the default. Don't know if this is the required
# behaviour, but as it stands (on my laptop at least), the map title reverts to
# "all subjects", rather than keeping "Medicine and dentistry" from the prior
# tests above. This is both in shinytest and running the App in the browser.
app$setInputs(YAGinput = "3", timeout_ = 3e+4)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_6.json"
)
