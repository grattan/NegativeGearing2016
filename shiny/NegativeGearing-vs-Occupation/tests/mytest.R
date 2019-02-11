app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

# Input 'Table1_rows_current' was set, but doesn't have an input binding.
# Input 'Table1_rows_all' was set, but doesn't have an input binding.
app$snapshot(items = list(output = "UnusedTable"))

app$snapshot(items = list(input = TRUE, export = "o_export"))
