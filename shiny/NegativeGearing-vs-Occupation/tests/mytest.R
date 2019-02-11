app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

# Input 'Table1_rows_current' was set, but doesn't have an input binding.
# Input 'Table1_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(search_q = "teach")
# Input 'Table1_rows_current' was set, but doesn't have an input binding.
# Input 'Table1_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(search_q = "teach ")
app$setInputs(search_q = "teach nur")
# Input 'Table1_rows_current' was set, but doesn't have an input binding.
# Input 'Table1_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(search_q = "teach law")
# Input 'Table1_rows_current' was set, but doesn't have an input binding.
# Input 'Table1_rows_all' was set, but doesn't have an input binding.
app$snapshot()
