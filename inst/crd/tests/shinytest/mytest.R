app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
app$uploadFile(file1 = "equal_replication.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(trt = 3,allowInputNoBinding_ = TRUE)
app$setInputs(yield = "WEIGHT",allowInputNoBinding_ = TRUE)
app$setInputs(submit = "click")
app$setInputs(submit1 = "click",allowInputNoBinding_ = TRUE)
app$setInputs(plotreq = "boxplot",allowInputNoBinding_ = TRUE)
app$setInputs(submit1 = "click",allowInputNoBinding_ = TRUE)
