---
title: "Rumus"
author: "Muh. Adriansyah"
date: "7/14/2022"
output: html_document: self_contained: no
---




```r
library(knitr)
```

The formula for $\alpha$ is: $$\alpha = eK = 
\frac{\frac{Q_t - Q_{t-1}}{Q_{t-1}}}{\frac{K_t - K_{t-1}}{K_{t-1}}}$$

The formula for $\beta$ is: $$\beta = eL = 1 - \alpha$$

Keterangan:

- $Q_t$ = Data Q(PDRB) tahun ke-t
- $Q_{t-1}$ = Data Q(PDRB) tahun ke-(t-1)
- $K_t$ = Data K(Modal) tahun ke-t
- $K_{t-1}$ = Data K(Modal) tahun ke-(t-1)
- $\alpha$ = eK = elastisitas kapital(modal)
- $\beta$ = eL = elastisitas labor(tenaga kerja)

The formula for $b_0$ is: $$b_0 = lnA = \frac{Q_t - Q_{t-1}}{Q_{t-1}} - \alpha\times\frac{K_t-K_{t-1}}{K_{t-1}} - \beta\times\frac{L_t - L_{t-1}}{L_{t-1}}$$

The formula for A(TFP) is:
$$A(TFP) = e^{b_0}$$

Keterangan:
- $b_0$ = A = Total Factor Productivity (TFP)

The formula for Economic Growth (EG) is:
$$eg = \frac{Q_t - Q_{t-1}}{Q_{t-1}}$$

The formula for TFP Growth (TFPG) is:
$$tfpg = \frac{A_t - A_{t-1}}{A_{t-1}}$$

The formula for Contribution of Capitals is:
$$sk = \alpha\times\frac{K_t - K_{t-1}}{K_{t-1}}$$

The formula for Contribution of Labor is:
$$sl = \beta \times \frac{L_t - L_{t-1}}{L_{t-1}}$$
