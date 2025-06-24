library(shinytest2)

test_that("Migrated shinytest test: testUI.R", {
  # Run the shiny tests.
  # Should really set this up to loop over arrays of inputs in order to run
  # through the shiny tests.
  app <- AppDriver$new(load_timeout = 160000, seed = 2011)


  # Industry flow tab ===========================================================

  industryFlow_input <- c("navlistPanel", "qualinput", "sexinput", "indflow.subjectinput")
  industryFlow_output <- c(
    "sankey", "sankey_title", "sankeyhelp", "sankeysubjectlist",
    "sankeytext1", "sankeytext2"
  )
  # 1.
  app$set_inputs(navlistPanel = "industryFlow", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)
  # 2.
  app$set_inputs(qualinput = "Level 7 (taught)", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  ### Commented out by Cathie because L7 research and L8 aren't options you can select on this page
  #  app$set_inputs(qualinput = "Level 7 (research)", timeout_ = 40000)
  #  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  #  app$set_inputs(qualinput = "Level 8", timeout_ = 40000)
  #  app$expect_values(input = industryFlow_input, output = industryFlow_output)
  # 3.
  app$set_inputs(sexinput = "F", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)
  # 4.
  app$set_inputs(sexinput = "M", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)

  # 5.  ### Added by Cathie
  app$set_inputs(indflow.subjectinput = "English studies", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)
  # 6.
  app$set_inputs(indflow.subjectinput = "Celtic studies", timeout_ = 40000)
  app$expect_values(input = industryFlow_input, output = industryFlow_output)


  # Subject by industry tab (renamed Industry by subject tab, but this is sheet 4 - NOT sheet 5!) =====================================================

  subjectByIndustry_input <- c(
    "navlistPanel", "countinput2", "YAGinput2", "crosstabs.subjectinput", "earningsbutton", "qualinput3"
  )

  # Note that I've excluded the crosstab_backwards tabulated output here as it
  # has a datakey that changes across different runs.
  subjectByIndustry_output <- c("crosstab_title", "crosstab", "crosstab_text")
  # 7.
  app$set_inputs(navlistPanel = "subjectByIndustry", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 8. Changed , wait_ = FALSE to timeout_ = 40000 for this and the following tests
  app$set_inputs(earningsbutton = "Median earnings", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 9.
  app$set_inputs(qualinput3 = "Level 8", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 10.
  app$set_inputs(qualinput3 = "Level 7 (research)", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 11.
  app$set_inputs(qualinput3 = "Level 7 (taught)", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 12.
  app$set_inputs(qualinput3 = "First degree", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 13.
  app$set_inputs(crosstabs.subjectinput = "Allied health", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 14.
  app$set_inputs(countinput2 = "ethnicity", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 15.
  app$set_inputs(YAGinput2 = "1", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 16.
  app$set_inputs(YAGinput2 = "3", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 17.
  app$set_inputs(YAGinput2 = "10", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 18.
  app$set_inputs(crosstabs.subjectinput = "English studies", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 19.
  app$set_inputs(earningsbutton = "Proportions", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 20.
  app$set_inputs(
    crosstabs.subjectinput = "Physics and astronomy",
    timeout_ = 40000
  )
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 21 and 22??
  app$set_inputs(crosstabs.subjectinput = "Celtic studies", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 22.
  app$set_inputs(earningsbutton = "Median earnings", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 23.
  app$set_inputs(YAGinput2 = "5", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 24.
  app$set_inputs(countinput2 = "current_region", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 25.
  app$set_inputs(countinput2 = "FSM", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 26.
  app$set_inputs(countinput2 = "prior_attainment", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 27.
  app$set_inputs(countinput2 = "subject_name", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 28.
  app$set_inputs(countinput2 = "qualification_TR", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 29.
  app$set_inputs(crosstabs.subjectinput = "Psychology", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 30.
  app$set_inputs(countinput2 = "current_region", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 31.
  app$set_inputs(countinput2 = "FSM", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 32.
  app$set_inputs(countinput2 = "prior_attainment", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)
  # 33.
  app$set_inputs(countinput2 = "subject_name", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)

  # 34.
  app$set_inputs(countinput2 = "qualification_TR", timeout_ = 40000)
  app$expect_values(input = subjectByIndustry_input, output = subjectByIndustry_output)


  # Regional tab ================================================================

  # Run tests for regional tab - note: excluding the map output as it makes the
  # json files massive.
  regional_input <- c(
    "navlistPanel", "regions.subjectinput", "YAGinput", "qualinput2",
    "sectionnameinput"
  )
  regional_output <- c("map_title", "mapsubjectlist", "maptext", "maptext2")

  # 35.
  app$set_inputs(navlistPanel = "regional", timeout_ = 40000)
  app$expect_values(input = regional_input, output = regional_output)

  # 36.
  app$set_inputs(
    regioninput = c("London", "North East", "West Midlands"),
    timeout_ = 40000
  )
  app$expect_values(input = regional_input, output = regional_output)

  ### Cathie commented the following line out because L7 research isn't an option for the regional page
  #  app$set_inputs(qualinput2 = "Level 7 (research)")
  # 37.
  app$set_inputs(qualinput2 = "Level 7 (taught)", timeout_ = 40000)
  app$expect_values(input = regional_input, output = regional_output)
  # 38.
  app$set_inputs(qualinput2 = "First degree", timeout_ = 40000)
  app$expect_values(input = regional_input, output = regional_output)
  # 39.
  app$set_inputs(sectionnameinput = "Transportation and storage", timeout_ = 40000)
  app$expect_values(input = regional_input, output = regional_output)
  # 40.
  app$set_inputs(regions.subjectinput = "Medicine and dentistry", timeout_ = 40000)
  app$expect_values(input = regional_input, output = regional_output)
  # 41.
  app$set_inputs(sectionnameinput = "EDUCATION", timeout_ = 40000)
  app$expect_values(input = regional_input, output = regional_output)

  # Note: for the following test, when YAG input is changed, subjectinput2
  # automatically reverts to the default. Don't know if this is the required
  # behaviour, but as it stands (on my laptop at least), the map title reverts to
  # "all subjects", rather than keeping "Medicine and dentistry" from the prior
  # tests above. This is both in shinytest and running the App in the browser.
  ### Cathie - the above no longer happens
  # 42.
  app$set_inputs(sectionnameinput = "Celtic Studies", timeout_ = 40000)
  app$expect_values(input = regional_input, output = regional_output)




  # Industry by subject tab =====================================================

  industryBySubject_input <- c(
    "navlistPanel", "countinput3", "YAGinput3", "sectionnameinput2",
    "groupinput", "earningsbutton2", "qualinput4"
  )
  # Cathie... what's this about?
  # Note that I've excluded the crosstab_backwards tabulated output here as it
  # has a datakey that changes across different runs.
  industryBySubject_output <- c("crosstab_backwards")
  # 43.
  app$set_inputs(navlistPanel = "industryBySubject", timeout_ = 40000)
  app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
  # 44.
  app$set_inputs(earningsbutton2 = "Median earnings", timeout_ = 40000)
  app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
  # 45.
  app$set_inputs(qualinput4 = "Level 8", timeout_ = 40000)
  app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
  # 46.
#   app$set_inputs(qualinput4 = "Level 7 (research)", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
#   # 47.
#   app$set_inputs(qualinput4 = "Level 7 (taught)", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
#   # 48.
#   app$set_inputs(qualinput4 = "First degree", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
#   # 49.
#   app$set_inputs(earningsbutton2 = "Proportions", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
#   # 50.
#   app$set_inputs(qualinput4 = "Level 8", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
#   # 51.
#   app$set_inputs(qualinput4 = "Level 7 (research)", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(qualinput4 = "Level 7 (taught)", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(qualinput4 = "First degree", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(sectionnameinput2 = "Construction", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(earningsbutton2 = "Median earnings", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(groupinput = "Construction of residential and non-residential buildings", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(sectionnameinput2 = "Real estate activities", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(YAGinput3 = "1", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(YAGinput3 = "3", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(YAGinput3 = "10", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(countinput3 = "ethnicity", YAGinput3 = "5", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(countinput3 = "current_region", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(countinput3 = "FSM", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(countinput3 = "prior_attainment", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(countinput3 = "qualification_TR", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(earningsbutton2 = "Proportions", timeout_ = 40000)
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
# 
#   app$set_inputs(
#     sectionname2 = "Arts, entertainment and recreation",
#     timeout_ = 40000
#   )
#   app$expect_values(input = industryBySubject_input, output = industryBySubject_output)
})
