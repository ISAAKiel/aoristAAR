[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# aoristAAR

aoristAAR is an R package for performing aoristic analysis on archaeological datasets with uncertain or imprecise dating.

It provides tools to transform start–end dated archaeological entities (e.g. sites, features, artefacts) into yearly time series, enabling the study of contemporaneity, intensity, and temporal overlap under uncertainty.

---

## What is aoristic analysis?

Aoristic analysis distributes the probability of an event across the full time interval in which it may have occurred.
In archaeology, this allows us to work systematically with:
- imprecise dates
- overlapping cultural phases
- mixed-resolution chronologies

The approach was introduced in criminology (Ratcliffe 2000) and adapted for archaeology by Mischka (2004) and later methodological work.

---

## Installation

aoristAAR is not (yet) available on CRAN.

Install the development version from GitHub:

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("ISAAKiel/aoristAAR")
```

---

## Basic usage

```r
library(aoristAAR)

x <- data.frame(
  start = c(-3800, -3750, -3500),
  end   = c(-3700, -3400, -3300)
)

ts <- aorist(
  x,
  from = "start",
  to   = "end",
  method = "number"
)

head(ts)
```

This returns a yearly time series, where each year represents the expected number of contemporaneous entities.

---

## Methods

Currently supported aoristic methods:

- number: Counts presence per year (each entity contributes 1 per covered year)
- weight: Distributes a total weight of 1 across the covered interval
- period_correction: Applies a correction for overlapping periods following archaeological adaptations of the aoristic method

---

## Contributing

Contributions are very welcome — including:

- bug reports
- test cases
- documentation improvements
- methodological discussion

Please open an issue or pull request on GitHub.

---

## Licence

aoristAAR is released under the GNU General Public Licence (GPL-3).

See LICENSE for details.

---

## References

The methods implemented here are inspired by:

- Ratcliffe (2000)
- Mischka (2004)
- Hinz & Müller-Scheeßel (forthcoming)


Full references are provided in the package documentation.