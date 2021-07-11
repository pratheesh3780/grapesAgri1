app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
app$uploadFile(file1 = "equal_replication.csv")
app$setInputs(trt = 3)
app$setInputs(yield = "WEIGHT")
app$setInputs(submit = "click")
