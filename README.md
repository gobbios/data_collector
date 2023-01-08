# data_collector

The apps require at least two packages to be locally installed:

```
install.packages("shiny")
install.packages("rhandsontable")
```

The main app is this:

```
shiny::runGitHub("data_collector", "gobbios")
```


There are a few smaller examples that allow testing/debugging of smaller components:

```
shiny::runGitHub("data_collector", "gobbios", subdir="hot_example")
```

