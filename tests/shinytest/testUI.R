app <- ShinyDriver$new("../../", loadTimeout = 1e+05)

app$snapshotInit("testUI", screenshot = FALSE)


# Testing the industry flow panel
industryFlow_input <- c("navbar", "qualinput", "sexinput")
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
industryBySubject_output <- c("backwards_crosstab_title")

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

app$setInputs(sectionnameinput2 = "PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_4.json"
)
app$setInputs(YAGinput3 = "1", wait_=FALSE, values_=FALSE)
app$snapshot(
  items = list(
    input = industryBySubject_input,
    output = industryBySubject_output
  ),
  filename = "industryBySubject_5.json"
)
app$setInputs(countinput3 = "SECTIONNAME", timeout_ = 1e+4)
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
  "navbar", "countinput2", "YAGinput2", "subjectinput3",
  "thresholdinput", "earningsbutton"
)
# Note that I've excluded the crosstab_backwards tabulated output here as it
# has a datakey that changes across different runs.
subjectByIndustry_output <- c("crosstab_title")

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
app$setInputs(thresholdinput = "Above", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_2.json"
)
app$setInputs(thresholdinput = "Below", wait_ = FALSE, values_ = FALSE)
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
app$setInputs(thresholdinput = "All", wait_ = FALSE, values_ = FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_5.json"
)
app$setInputs(YAGinput2 = "1", wait_=FALSE, values_=FALSE)
app$snapshot(
  items = list(
    input = subjectByIndustry_input,
    output = subjectByIndustry_output
  ),
  filename = "subjectByIndustry_6.json"
)
app$setInputs(subjectinput3 = "Sport and exercise sciences", wait_=FALSE, values_=FALSE)
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
  "navbar", "mapsubjectinput", "YAGinput", "qualinput2",
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
app$setInputs(qualinput2 = "Level 7 (research)", timeout_ = 2e+4)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_2.json"
)
app$setInputs(sectionnameinput = "TRANSPORTATION AND STORAGE", wait_=FALSE, values_=FALSE)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_3.json"
)
app$setInputs(subjectinput2 = "Medicine and dentistry", timeout_ = 2e+4)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_4.json"
)
app$setInputs(sectionnameinput = "EDUCATION", countinput = "living_in_region", wait_=FALSE, values_=FALSE)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_5.json"
)
app$setInputs(YAGinput = "3", timeout_ = 2e+4)
app$snapshot(
  items = list(
    input = regional_input,
    output = regional_output
  ),
  filename = "regional_6.json"
)
