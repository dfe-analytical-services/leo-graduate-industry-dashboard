cohort1 <- read.csv("data/pg_sankey_data_1_3_yag_dummy.csv")
cohort2 <- read.csv("data/pg_sankey_data_3_5_yag_dummy.csv")
cohort3 <- read.csv("data/pg_sankey_data_1_5_yag_dummy.csv")

cohort1 <- subset(cohort1, select = -X)
cohort2 <- subset(cohort2, select = -X)
cohort3 <- subset(cohort3, select = -X)

cohort1$SECTIONNAME.x <- StrCap(tolower(cohort1$SECTIONNAME.x))
cohort1$SECTIONNAME.y <- StrCap(tolower(cohort1$SECTIONNAME.y))
cohort2$SECTIONNAME.x <- StrCap(tolower(cohort2$SECTIONNAME.x))
cohort2$SECTIONNAME.y <- StrCap(tolower(cohort2$SECTIONNAME.y))
cohort3$SECTIONNAME.x <- StrCap(tolower(cohort3$SECTIONNAME.x))
cohort3$SECTIONNAME.y <- StrCap(tolower(cohort3$SECTIONNAME.y))

tables_data <- fread("data/pg_sic_crosstabs_earnings_data_cf_dummy.csv") %>% select(-V1)

names(tables_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "earnings_median", "threshold", "qualification_TR", "group_name")

tables_data$SECTIONNAME[tables_data$group_name == "Radio broadcasting"] <- "INFORMATION AND COMMUNICATION"
tables_data$SECTIONNAME[tables_data$group_name == "Reproduction of recorded media"] <- "MANUFACTURING"

tables_data$SECTIONNAME <- StrCap(tolower(tables_data$SECTIONNAME))

tables_earnings_data <- tables_data

names(tables_earnings_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "earnings_median", "threshold", "qualification_TR", "group_name")
