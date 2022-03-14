app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("initial_load_test", screenshot = FALSE)

app$snapshot()
