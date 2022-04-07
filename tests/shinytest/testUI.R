app <- ShinyDriver$new("../../", loadTimeout = 1.6e+04, seed=2011)

app$snapshotInit("testUI", screenshot = FALSE)


# Testing the industry flow panel
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



# Run tests for industry by subject tab
industryBySubject_input <- c(
  "navbar", "countinput3", "YAGinput3", "qualinput4",
  "sectionnameinput2", "earningsbutton2"
)
# Note that I've excluded the crosstab_backwards tabulated output here as it
# has a datakey that changes across different runs.
industryBySubject_output <- c("backwards_crosstab_title", "crosstab_backwards")

app$setInputs(navbar = "industryBySubject", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_0.json"
)
app$setInputs(earningsbutton2 = "Median earnings", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_1.json"
)
app$setInputs(countinput3 = "ethnicity", timeout_ = 1e+4)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_2.json"
)
app$setInputs(YAGinput3 = "10", timeout_ = 1e+4)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_3.json"
)

app$setInputs(
  sectionnameinput2 = "PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY",
  wait_ = FALSE, values_ = FALSE
)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_4.json"
)
app$setInputs(YAGinput3 = "1", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_5.json"
)
app$setInputs(countinput3 = "FSM", timeout_ = 1e+4)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_6.json"
)
app$setInputs(earningsbutton2 = "Proportions", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_7.json"
)



# Run tests for subject by industry tab
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
app$setInputs(earningsbutton = "Median earnings", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_1.json"
)
app$setInputs(qualinput3 = "Level 8", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_2.json"
)
app$setInputs(crosstabs.subjectinput = "Allied health", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_3.json"
)
app$setInputs(countinput2 = "ethnicity", timeout_ = 1e+4)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_4.json"
)
app$setInputs(YAGinput2 = "5", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_5.json"
)
app$setInputs(YAGinput2 = "1", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_6.json"
)
app$setInputs(crosstabs.subjectinput = "English studies", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_7.json"
)
app$setInputs(earningsbutton = "Proportions", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_8.json"
)


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
