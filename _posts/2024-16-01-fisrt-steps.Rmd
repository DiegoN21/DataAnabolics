---
title: "First Steps for Backtesting"
output: html_document
date: "2024-01-16"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading libraries
Before we download stock prices, let's load the necessary libraries.

```{r, echo=TRUE, warning = FALSE, message = FALSE}
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(scales)
library(ggridges)
```
## Selecting the universe of assets and downloading prices

In this simple case, we will work with sector ETFs. For downloading the prices, we will use the `tq_get` function from the tidyquant package, and for simplicity, we will remove empty values.

```{r}
etf_symbols <- c("XLC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLRE", "XLK", "XLU")

etf_prices <- tq_get(etf_symbols, from = "1990-01-01", to = "2024-01-15")

etf_prices_clean <- etf_prices %>%
  group_by(symbol) %>%
  drop_na() %>%
  filter(!is.na(adjusted))
```

## Calculating Returns

```{r}
etf_returns <- etf_prices_clean %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", col_rename = "daily_return")
```

## Visualizations

```{r}
etf_returns %>%
  ggplot(aes(x = date, y = daily_return, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol)+
  theme_minimal() +
  labs(title = "Daily Returns of S&P 500 Sector ETFs", x = "Date", y = "Daily Return")
```
```{r}
ggplot(etf_returns, aes(x = symbol, y = daily_return, color=symbol)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Daily Return Distributions", x = "", y = "")
```

```{r}
etf_returns %>%
  group_by(symbol) %>%
  mutate(cumulative_return = cumprod(1+daily_return)) %>%
  ggplot(aes(x = date, y = cumulative_return, color = symbol)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar)+
  theme_minimal() +
  guides(color=guide_legend(nrow=1, byrow=TRUE)) +
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = "", x = "", y = "")

```

```{r, message=FALSE}
ggplot(etf_returns, aes(x = daily_return, y = symbol, fill = symbol)) +
  ggridges::geom_density_ridges(alpha = 0.7) +
  labs(title = "Distribución de Retornos Diarios por ETF", 
       x = "Retorno Diario", 
       y = "ETF") +
  theme_minimal() +  # Utiliza un tema minimalista
  theme(axis.title.y = element_blank(),  # Oculta el título del eje Y
        axis.ticks.y = element_blank())  # Oculta las marcas del eje Y



```

