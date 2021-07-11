app <- ShinyDriver$new("../../", loadTimeout = 1e+05,seed=1234)
app$snapshotInit("mytest")

app$uploadFile(file1 = "iris.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()
