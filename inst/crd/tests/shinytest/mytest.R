app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
app$uploadFile(file1 = "equal_replication.csv")
app$setInputs(trt = 3,allowInputNoBinding_ = TRUE)
app$setInputs(yield = "WEIGHT",allowInputNoBinding_ = TRUE)
app$setInputs(submit = "click")
