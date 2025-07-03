---
title: "How to contribute"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## How to contribute?

Contributions to this package are welcome. Ideally, talk to the developer team
first before you start coding new features. This helps us to keep the package
consistent, compatible and create the best user experience for semtree users.
We prefer code contributions as pull requests because this allows the developer
team to discuss changes before they are merged within in the main branch. If
you are interested in contributing and would like to start a discussion on
a new feature or a bug fix, please open an issue in the semtree github repository.

## Some notes

- Please make sure that all tests pass before you create a pull request with a new
feature or a bug fix, e.g., by running this in your local semtree directory:
```{r}
devtools::test(".")
```
- Please make sure that the package passes all checks that are required by CRAN
before you create a pull request with a new feature or a bug fix, e.g., by running
this in your local semtree directory:
```{r}
devtools::check(".")
```
- please use camel case for function names indicating the separation of words with a single capitalized letter, and the first word starting with lower case, e.g., `checkBinSize()` or `getExpectedMean()`.
