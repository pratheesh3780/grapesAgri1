app <- ShinyDriver$new("../../", loadTimeout= 1e+05, seed=1234)
app$snapshotInit("mytest")

app$snapshot()
