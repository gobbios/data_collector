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
shiny::runGitHub("data_collector", "gobbios", subdir = "example_components/hot_example")
shiny::runGitHub("data_collector", "gobbios", subdir = "example_components/dynamic_nn_test")
shiny::runGitHub("data_collector", "gobbios", subdir = "example_components/blinking_text")
shiny::runGitHub("data_collector", "gobbios", subdir = "example_components/button_grid")
shiny::runGitHub("data_collector", "gobbios", subdir = "example_components/checkbox_reactive")
shiny::runGitHub("data_collector", "gobbios", subdir = "example_components/time_choice")
```

