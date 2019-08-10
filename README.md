[![Build Status](https://travis-ci.org/PabRod/rolldown.svg?branch=master)](https://travis-ci.org/PabRod/rolldown)
[![codecov](https://codecov.io/gh/PabRod/rolldown/graph/badge.svg)](https://codecov.io/gh/PabRod/rolldown)
[![codecov](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/143162432.svg)](https://zenodo.org/record/2593241#.XIpSSihKg2w)

# rolldown <img src="vignettes/img/logo.png" width="120" align="right" />
This package implements some methods for computing potential landscapes for non-gradient systems.

For a detailed overview of the underlying ideas, please refer to the paper [_Climbing Escher's stairs: a simple quasi-potential algorithm for weakly non-gradient systems_](https://arxiv.org/abs/1903.05615), by Pablo Rodríguez-Sánchez, Egbert van Nes, and Marten Scheffer.

## Getting started

### Prerrequisites
This is an _R_ package. [_R_](https://www.r-project.org/) is required, [_RStudio_](https://www.rstudio.com/) is recommended.

### Installing

#### Latest stable version
Type `devtools::install_github("PabRod/rolldown", ref = "master")` in your `R` command console.

#### Latest version
Type `devtools::install_github("PabRod/rolldown", ref = "develop")` in your `R` command console.

#### Reproduce my manuscript
If you want to locally reproduce my manuscript [_Climbing Escher's stairs: a simple quasi-potential algorithm for weakly non-gradient systems_](https://arxiv.org/abs/1903.05615), follow these steps:

0. Type `devtools::install_github("PabRod/rolldown", ref = "feature/reproducible")` to install `rolldown` and the libraries needed to reproduce the manuscript
1. Clone or download the reproducible branch of this repository (shortcut: `git clone --single-branch --branch feature/reproducible https://github.com/PabRod/rolldown.git`)
2. knit the file [`vignettes\manuscript.Rmd`](https://github.com/PabRod/rolldown/blob/feature/reproducible/vignettes/manuscript.Rmd)

Rendering the figures requires `Python`, and the packages `matplotlib` and `numpy`.

### Running the tests
The integrity of this package can be checked by running the battery of tests available at `./tests`.

### Examples of usage
A vignette with examples of usage can be found in [inst/doc/examples.pdf](inst/doc/examples.pdf)

## Authors
- [Pablo Rodríguez-Sánchez](https://pabrod.github.io)

## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE) file for details

## Acknowledgements
This work was greatly inspired by the dicussions with [Cristina Sargent](https://www.researchgate.net/profile/Cristina_Sargent), [Iñaki Úcar](https://github.com/Enchufa2/), [Enrique Benito](https://sites.google.com/site/enriquebenitomatias/), [Tobias Oertel-Jäger](https://users.fmi.uni-jena.de/~tjaeger/), [Jelle Lever](https://www.linkedin.com/in/jellelever/), [Sanne J.P. van den Berg](https://www.linkedin.com/in/sanne-van-den-berg-23253b6b/) and [Els Weinans](https://www.wur.nl/es/Persons/Els-E-Els-Weinans-MSc.htm). This work was supported by funding from the European Union's _Horizon 2020_ research and innovation programme for the [_ITN CRITICS_](http://www.criticsitn.eu/wp/) under Grant Agreement Number _643073_.
