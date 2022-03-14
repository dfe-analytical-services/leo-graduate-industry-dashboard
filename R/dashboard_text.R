# File for reading in text files containing text content for the dashboard

intro_text <- readr::read_file("www/text/intro_text.txt")

sic_groups <- paste("We have two SIC variables joined onto our data: SECTIONNAME and SIC2007. There are over 700 detailed SIC
                                 codes in the SIC2007 variable, which are grouped into 21 industry sections in the SECTIONNAME variable. We
                                 have used the broader industry sections as the base for our analysis, which consist of the following:", br(), br(),
"1. ACCOMMODATION AND FOOD SERVICE ACTIVITIES", br(),
"2. ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES", br(),
"3. ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS - UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE", br(),
"4. ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES", br(),
"5. AGRICULTURE, FORESTRY AND FISHING", br(),
"6. ARTS, ENTERTAINMENT AND RECREATION", br(),
"7. CONSTRUCTION", br(),
"8. EDUCATION", br(),
"9. ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY", br(),
"10. FINANCIAL AND INSURANCE ACTIVITIES", br(),
"11. HUMAN HEALTH AND SOCIAL WORK ACTIVITIES", br(),
"12. INFORMATION AND COMMUNICATION", br(),
"13. MANUFACTURING", br(),
"14. MINING AND QUARRYING", br(),
"15. OTHER SERVICE ACTIVITIES", br(),
"16. PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES", br(),
"17. PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY", br(),
"18. REAL ESTATE ACTIVITIES", br(),
"19. TRANSPORTATION AND STORAGE", br(),
"20. WATER SUPPLY - SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES", br(),
"21. WHOLESALE AND RETAIL TRADE - REPAIR OF MOTOR VEHICLES AND MOTORCYCLES"
)