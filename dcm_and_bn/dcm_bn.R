library(tidyverse)
library(bnlearn)
library(CDM)
library(Rgraphviz)
library(rstan)
library(StanHeaders)

data("data.cdm06")

exdf <- data.cdm06[[1]]
exq <- data.cdm06[[2]]
ex_skill <- data.cdm06[[3]]

exdf <- exdf |>
	janitor::clean_names()

bndf <- exdf |>
	mutate(
		across(
			everything(),
			~as.factor(.x)
		)
	)


exq <- exq |>
	janitor::clean_names() 


lcdm <- gdina(
	exdf,
	exq,
	method = "ML"
)

plot(lcdm)
round(lcdm$attr.prob, 2)
round(lcdm$attribute.patt, 2)
round(lcdm$skill.patt, 2)[, 2]

names(bndf)

hcbn <- hc(bndf)
View(exq)
graphviz.plot(hcbn)

iambbn <- iamb(
	bndf, 
	alpha = .05,
	debug = TRUE)

graphviz.plot(iambbn)

score(
	hcbn,
	bndf,
	by.node = TRUE
	)
