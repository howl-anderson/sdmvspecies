#!/usr/bin/env bash

~/R-devel/bin/R CMD build sdmvspecies
~/R-devel/bin/R CMD check --as-cran sdmvspecies_*.tar.gz
rm sdmvspecies_*.tar.gz
