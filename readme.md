# Indicadores económicos de Chile

[![Scrapear Banco Central](https://github.com/bastianolea/economia_chile/actions/workflows/scrapear_bancocentral.yaml/badge.svg)](https://github.com/bastianolea/economia_chile/actions/workflows/scrapear_bancocentral.yaml)

Proyecto que busca presentar de forma sencilla y compacta los principales indicadores de la economía chilena.

Los datos de este repositorio se actualizan automáticamente cada 24 horas por medio de GitHub Actions. Estos datos se obtienen realizando web scraping al [sitio web del Banco Central.](https://www.bcentral.cl/web/banco-central) usando el [paquete de R `{rvest}`](https://rvest.tidyverse.org).

_En contrucción_

### Objetivos
- crear funciones para scrapear datos oficiales
- automatizar obtención de datos usando GitHub Actions
- mantener actualizados indicadores económicos
- visualizar indicadores económicos en un solo dashboard

### Referencias
https://www.gavinrozzi.com/post/automating-scraping-gh-actions/

https://si3.bcentral.cl/SetGraficos/