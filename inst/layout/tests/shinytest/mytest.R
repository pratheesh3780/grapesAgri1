app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(trt_crd = 3)
app$setInputs(rep_crd = 5)
app$setInputs(submit1 = "click")
