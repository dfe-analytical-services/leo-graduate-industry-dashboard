library(shinytest2)

test_that("Migrated shinytest test: testUI.R", {
  # Run the shiny tests.
  # Should really set this up to loop over arrays of inputs in order to run
  # through the shiny tests.
  app <- AppDriver$new(load_timeout = 160000, seed = 2011)


  # Industry flow tab ===========================================================

  industryFlow_input <- c("navbar", "qualinput", "sexinput", "indflow.subjectinput")
  industryFlow_output <- c(
    "sankey", "sankey_title", "sankeyhelp", "sankeysubjectlist",
    "sankeytext1", "sankeytext2"
  )

  app$set_inputs(navbar = "industryFlow", timeout_ = 10000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  app$set_inputs(qualinput = "Level 7 (taught)", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  app$set_inputs(qualinput = "Level 7 (research)", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  app$set_inputs(qualinput = "Level 8", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  app$set_inputs(sexinput = "F", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  app$set_inputs(sexinput = "M", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  # Subject by industry tab =====================================================

  subjectByIndustry_input <- c(
    "navbar", "countinput2", "YAGinput2", "crosstabs.subjectinput", "earningsbutton", "qualinput3"
  )

  # Note that I've excluded the crosstab_backwards tabulated output here as it
  # has a datakey that changes across different runs.
  subjectByIndustry_output <- c("crosstab_title", "crosstab", "crosstab_text")

  app$set_inputs(navbar = "subjectByIndustry", timeout_ = 20000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(earningsbutton = "Median earnings", wait_ = FALSE)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(qualinput3 = "Level 8", wait_ = FALSE)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(crosstabs.subjectinput = "Allied health", wait_ = FALSE)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(countinput2 = "ethnicity", timeout_ = 10000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(YAGinput2 = "5", wait_ = FALSE)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(YAGinput2 = "1", wait_ = FALSE)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(crosstabs.subjectinput = "English studies", wait_ = FALSE)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(earningsbutton = "Proportions", wait_ = FALSE)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  app$set_inputs(
    crosstabs.subjectinput = "Physics and astronomy",
    wait_ = FALSE
  )
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)


  # Regional tab ================================================================

  # Run tests for regional tab - note: excluding the map output as it makes the
  # json files massive.
  regional_input <- c(
    "navbar", "regions.subjectinput", "YAGinput", "qualinput2",
    "sectionnameinput"
  )
  regional_output <- c("map_title", "mapsubjectlist", "maptext", "maptext2")

  app$set_inputs(navbar = "regional", timeout_ = 10000)
  app$expect_values(input = regional_input, output = regional_output)
  app$set_inputs(
    regioninput = c("London", "North East", "West Midlands"),
    timeout_ = 20000
  )
  app$expect_values(input = regional_input, output = regional_output)
  app$set_inputs(qualinput2 = "Level 7 (research)")
  app$expect_values(input = regional_input, output = regional_output)
  app$set_inputs(sectionnameinput = "TRANSPORTATION AND STORAGE")
  app$expect_values(input = regional_input, output = regional_output)
  app$set_inputs(regions.subjectinput = "Medicine and dentistry")
  app$expect_values(input = regional_input, output = regional_output)
  app$set_inputs(sectionnameinput = "EDUCATION", countinput = "living_in_region")
  app$expect_values(input = regional_input, output = regional_output)

  # Note: for the following test, when YAG input is changed, subjectinput2
  # automatically reverts to the default. Don't know if this is the required
  # behaviour, but as it stands (on my laptop at least), the map title reverts to
  # "all subjects", rather than keeping "Medicine and dentistry" from the prior
  # tests above. This is both in shinytest and running the App in the browser.
  app$set_inputs(YAGinput = "3", timeout_ = 30000)
  app$expect_values(input = regional_input, output = regional_output)
})
