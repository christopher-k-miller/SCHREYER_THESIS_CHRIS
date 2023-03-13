library(dplyr)
library(tidyverse)
library(data.table)
library(forecast)
library(kableExtra)
library(reshape2)
library(ggplot2)
library(scales)

rm(list=ls())

gamedataadvanced <- read_csv("gamedataadvanced.csv")