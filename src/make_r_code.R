knitr::purl('escala_lote.qmd')
knitr::purl('escala_regional.qmd')

file.rename("escala_lote.R",
            "src/escala_lote.R")
file.rename("escala_regional.R",
            "src/escala_regional.R")
