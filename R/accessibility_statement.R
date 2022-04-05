accessibility_statement <- function() {
  div(
    h2("Accessibility statement for LEO Graduate Industry dashboard"),
    "This accessibility statement applies to the Longitudinal Education Outcomes (LEO) graduate industry dashboard.",
    "This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind.",
    br(),
    br(),
    "We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements", .noWS = c("after")),
    ". This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool", .noWS = c("after")),
    ", which did not detect accessibility issues.", # TODO: CHECK THIS!
    "This application also fully passes the accessibility audits checked by DfE analysts using the ",
    a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool", .noWS = c("after")),
    ". This means that this application:",
    br(),
    tags$div(tags$ul(
      tags$li("uses colours that have sufficient contrast"),
      tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
      tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
    )), # TODO: UPDATE THESE BULLET POINTS
    "We've also made the website text as simple as possible to understand.",
    a(href = "https://mcmw.abilitynet.org.uk/", "AbilityNet", .noWS = c("after")),
    " has advice on making your device easier to use if you have a disability.",
    h3("How accessible this website is"),
    "We recognise that there are still potential issues with accessibility in this application, but we will continue
           to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
           are known issues that we will continue to monitor and improve:",
    tags$div(tags$ul(
      tags$li("Keyboard navigation through the interactive charts is currently limited"),
      tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
    )), # TODO: UPDATE THESE BULLET POINTS

    h3("Feedback and contact information"),
    "If you need information on this website in a different format like accessible PDF, large print, easy read, audio recording or braille or
      if you have any feedback on how we could further improve the accessibility of this application, please contact us at ",
    a(href = "mailto:he.leo@education.gov.uk", "he.leo@education.gov.uk", .noWS = c("after")),
    ".",
    br(),
    br(),
    h3("Technical information about this website’s accessibility"),
    "Department for Education is committed to making its website accessible, in accordance with the Public Sector Bodies
      (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018.",
    h4("Compliance status"),
    "This website is partially compliant with the ",
    a(href = "", "Web Content Accessibility Guidelines version 2.1", .noWS = c("after")),
    " AA standard, due to [insert one of the following: ‘the non-compliances’, ‘the exemptions’ or ‘the non-compliances and exemptions’] listed below.",
    br(),
    br(),
    "List of any non-compliances", # TODO: List the non-compliances using the appropriate template

    h3("Preparation of this accessibility statement"),
    "This statement was prepared on [date when it was first published].", # TODO: Add date
    br(),
    br(),
    "This website was last tested on [date]. The test was carried out by Department for Education.", # TODO: Add date
    br(),
    br(),
    br()
  )
}
