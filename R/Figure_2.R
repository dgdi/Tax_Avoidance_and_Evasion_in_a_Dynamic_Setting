#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@           Tax Avoidance and Evasion             @@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@              in a Dynamic Setting               @@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@                   Figure 2                   @@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# Reference article:
# Duccio Gamannossi degl'Innocenti, Rosella Levaggi and Francesco Menoncin, "Tax Avoidance and Evasion in a Dynamic Setting", MIMEO
#
# All of the required packages can be installed by running the script package_setup.R
# This script has been tested on Win 10, R-4.1.1, RStudio-1.4.1717
# This script is distributed under BSD license. For more information, please check the license.txt file.
#
# For any question, suggestion or comment,
# write to:
# mail@dgdi.me


#cleaning environment
rm(list = ls())

#load libraries
library(here)
library(data.table)
library(ggplot2)
library(tikzDevice)
library(extrafont)
library(patchwork)

#set plotting parameters
size_lyx <- .4

#set model parameters
A = 0.3
cm = 0
rho = 0.05
k0 = 100
delta = 2.5
eta = 2.5
lambda = 0.3
gamma = 2.5
tau = seq(0, 1, 0.01)
beta = 0.5

#compute ratio of expected revenues to capital and revenue-maximizing tax rates for different constants of variation of the avoidance cost function
omega = 0.1
Tk1 = tau * (1 - beta * (beta * tau / omega / gamma) * (1 / (gamma - 1))) *
  A - (1 - lambda * eta) / eta * (1 - (lambda * eta) * (1 / delta))
tau_star1 = omega * gamma / beta * ((gamma - 1) / (gamma * beta)) ^ (gamma -
                                                                       1)

omega = 0.125
Tk2 = tau * (1 - beta * (beta * tau / omega / gamma) * (1 / (gamma - 1))) *
  A - (1 - lambda * eta) / eta * (1 - (lambda * eta) * (1 / delta))
tau_star2 = omega * gamma / beta * ((gamma - 1) / (gamma * beta)) ^ (gamma -
                                                                       1)

omega = 0.15
Tk3 = tau * (1 - beta * (beta * tau / omega / gamma) * (1 / (gamma - 1))) *
  A - (1 - lambda * eta) / eta * (1 - (lambda * eta) * (1 / delta))
tau_star3 = omega * gamma / beta * ((gamma - 1) / (gamma * beta)) ^ (gamma -
                                                                       1)


#create a data.table to store ratio of expected revenues to capital as a function of the tax rate
graph_laffer_dt <- data.table(
  tau = c(rep(tau, 3)),
  Tks = c(Tk1, Tk2, Tk3),
  omega = factor(
    c(
      rep("$\\omega_1 = .1$", length(tau)),
      rep("$\\omega_2 = .125$", length(tau)),
      rep("$\\omega_3 = .15$", length(tau))
    ),
    levels = c("$\\omega_3 = .15$", "$\\omega_2 = .125$", "$\\omega_1 = .1$")
  )
)

#create a data.table to store revenue-maximizing tax rates
tstars <- data.table(
  x = c(tau_star1, tau_star2, tau_star3),
  y = .1075,
  lab =  c(
    "$\\tau^{\\ast}_1$",
    "$\\tau^{\\ast}_2$",
    "$\\tau^{\\ast}_3$"
  )
)

#plot Figure 2
graph_laffer_plot <- ggplot(data = graph_laffer_dt) +
  
  geom_line(aes(x = tau, y = Tks, linetype = omega), size = size_lyx) +
  
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  
  geom_vline(
    xintercept = c(tau_star1, tau_star2, tau_star3),
    linetype = c("dotted", "longdash", "solid"),
    size = size_lyx
  ) +
  
  geom_hline(
    yintercept = c(0),
    linetype = c("solid"),
    colour = "black",
    size = .15
  ) +
  
  labs(y = "Revenues to Capital ratio $\\mathbb{E}_t[dT_t]/k_t$",
       x = "$\\tau$")  +
  
  theme_bw() +
  
  geom_text(
    data = tstars,
    aes(x = x, y = y, label = lab),
    hjust = .5,
    size = 11 * 1 / ggplot2:::.pt
  ) +
  
  coord_cartesian(ylim = c(-0.08, .1000000001),
                  expand = FALSE,
                  clip = "off") +
  
  theme(
    plot.title = element_text(size = rel(1), hjust = .5),
    legend.text = element_text(size = rel(.65)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = grid::unit(1, "lines"),
    legend.key.width = unit(.875, "cm"),
    legend.background = element_rect(colour = "white"),
    legend.box.background = element_rect(colour = "white"),
    legend.position = c(0.2, 0.8),
    axis.line = element_line(colour = "black", size = .1),
    axis.ticks = element_line(colour = "black", size = .1),
    axis.text.y = element_text(
      size = rel(.9),
      colour = "black",
      margin = margin(
        t = 0,
        r = 5,
        b = 0,
        l = 0
      )
    ),
    axis.text.x = element_text(
      size = rel(.9),
      colour = "black",
      margin = margin(
        t = 5,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(vjust = .5, hjust = .5),
    axis.title.y = element_text(
      vjust = .5,
      hjust = .5,
      angle = 90,
      margin = margin(
        t = 0,
        r = 10,
        b = 0,
        l = 0
      )
    ),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_line(
      colour = "gray",
      linetype = "solid",
      size = .11
    ),
    panel.grid.major = element_line(
      colour = "gray",
      linetype = "solid",
      size = .11
    ),
    plot.background = element_blank(),
    plot.margin = margin(
      t = 30,
      r = 10,
      b = 0,
      l = 0
    )
  )
graph_laffer_plot

#define variable to store errors in tikz compilation
tmptikz_err <- NULL

#tikz plot
plotname <- "Figure_2"
tmptikz <- paste0(here(), "/tikz/tmp_", plotname)
setTikzDefaults()
options(
  tikzLwdUnit = 72.27 / 96,
  tikzMetricPackages = c(
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}",
    "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"
  ),
  tikzMetricsDictionary = tmptikz
)
tikz(
  paste0(here(), "/tikz", "/", plotname, ".tex"),
  width = 5,
  height = 3.5,
  standAlone = TRUE,
  packages = c(
    "\\usepackage{tikz}",
    "\\usepackage[active,tightpage,psfixbb]{preview}",
    "\\PreviewEnvironment{pgfpicture}",
    "\\setlength\\PreviewBorder{0pt}",
    "\\usepackage{amssymb}",
    "\\usepackage{amsfonts}",
    "\\usepackage{eurosym}",
    "\\usepackage{amsmath}",
    "\\usepackage{amssymb}",
    "\\usepackage[T1]{fontenc}"
  )
)

graph_laffer_plot

dev.off()

#compiling and displaying the tikz plot ***the compiler path needs to be set by the user***
if (!file.exists(paste0(tmptikz, "___LOCK"))) {
  #set the compiler path
  Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.21/bin/gswin64c.exe")
  setwd(here("fig/"))
  tools::texi2pdf(paste0(here("tikz/"), "/", plotname, ".tex"), clean = T)
  embed_fonts(paste0(here("fig/"), "/", plotname, ".pdf"))
  system(paste(getOption("pdfviewer"),
               paste0(here("fig/"), "/", plotname, ".pdf")))
  tmptikz_err <- setdiff(tmptikz_err, plotname)
} else{
  file.remove(paste0(tmptikz, "___LOCK"))
  tmptikz_err <- c(tmptikz_err, plotname)
}
