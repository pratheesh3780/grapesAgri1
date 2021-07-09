app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
app$uploadFile(file1 = "mango_data.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(trt = 10,allowInputNoBinding_ = TRUE)
app$setInputs(rep = 3,allowInputNoBinding_ = TRUE)
app$setInputs(Replication = "Replication",allowInputNoBinding_ = TRUE)
app$setInputs(yield = "yield",allowInputNoBinding_ = TRUE)
app$setInputs(submit = "click",allowInputNoBinding_ = TRUE)
app$setInputs(submit1 = "click",allowInputNoBinding_ = TRUE)
app$setInputs(plotreq = "boxplot",allowInputNoBinding_ = TRUE)
app$setInputs(submit1 = "click",allowInputNoBinding_ = TRUE)
