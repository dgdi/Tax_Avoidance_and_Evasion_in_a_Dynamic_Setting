#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@           Tax Avoidance and Evasion             @@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@              in a Dynamic Setting               @@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@                   Figure 1                   @@@@@@@@@@@@@@@@@@@@@@@@@
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
cm = 10
rho = 0.05
delta = 2.5
tau = 0.3
eta = 2.5
lambda = 0.3
omega = 1
gamma = 2
k0 = 100
t = 50
N = 1000
dt = 1 / 50

#set betas to be considered
betas <- c(0, .5)

#loop to compute the optimal dynamics of evasion and ratio of consumption to capital under the two betas
list_optim <- lapply(betas, function(beta_temp) {
  #set seed to ensure reproducibility
  set.seed(03102021)
  
  #allocate arrays to store the dynamic of optimal consumption, evasion and capital
  c = e = k = array(NA, dim = c(t / dt, N))
  
  #initialize the capital array
  k[1,] = k0
  
  #define optimal value of avoidance
  a = (tau * beta_temp / gamma / omega) ^ (1 / (gamma - 1))
  
  #define optimal value of evasion and consumption and initialize their arrays
  H = cm / (A * (tau * beta_temp * a - omega * a ^ gamma + 1 - tau))
  e[1,] = (k[1,] - H) / (tau * eta * A * k[1,]) * (1 - (lambda * eta) ^
                                                     (1 / eta)) - (1 - beta_temp) * a
  c[1,] = cm + (k[1,] - H) *
    ((rho + lambda) / delta + (delta - 1) / delta / eta + (delta - 1) /
       delta * (1 - tau) * A -
       (lambda * eta) ^ (1 / delta) / eta +
       (delta - 1) / delta * (tau * beta_temp * a - omega * a ^ gamma) *
       A
    )
  
  #compute and store optimal consumption, evasion and capital at each time step
  for (i in 2:(t / dt)) {
    k[i,] = k[i - 1,] + (A * k[i - 1,] - tau * (1 - e[i - 1,] - a) * A * k[i -
                                                                             1,] -
                           c[i - 1,] - omega * a ^ gamma * A * k[i - 1,]) *
      dt -
      eta * (e[i - 1,] + (1 - beta_temp) * a) * tau * A * k[i - 1,] * rpois(N, lambda *
                                                                              dt)
    e[i,] = (k[i,] - H) / (tau * eta * A * k[i,]) * (1 - (lambda * eta) ^
                                                       (1 / eta)) - (1 - beta_temp) * a
    c[i,] = cm + (k[i,] - H) *
      ((rho + lambda) / delta + (delta - 1) / delta / eta + (delta - 1) /
         delta * (1 - tau) * A -
         (lambda * eta) ^ (1 / delta) / eta +
         (delta - 1) / delta * (tau * beta_temp * a - omega * a ^ gamma) *
         A
      )
  }
  
  #compute optimal evasion and consumption when the taxpayer do not consider minimum consumption (cm = 0)
  e_cm0 = 1 / (tau * eta * A) * (1 - (lambda * eta) ^ (1 / eta)) - (1 -
                                                                      beta_temp) * a
  ck_cm0 = (rho + lambda) / delta + (delta - 1) / delta / eta + (delta -
                                                                   1) / delta * (1 - tau) * A -
    (lambda * eta) ^ (1 / delta) / eta +
    (delta - 1) / delta * (tau * beta_temp * a - omega * a ^ gamma) * A
  
  #compute the 0 quantile, the 100 quantile and the mean of the ratio of optimal consumption to capital at each timestep for cm > 0
  #compute the value of the ratio of optimal consumption to capital for cm = 0
  ck_measures = cbind(t(apply(c / k, 1, quantile, probs = c(0, 1))),
                      apply(c / k, 1, mean),
                      rep(ck_cm0, t / dt))
  
  #compute the 0 quantile, the 100 quantile and the mean of optimal evasion at each timestep for cm > 0
  #compute the value of optimal evasion for cm = 0
  e_measures = cbind(t(apply(e, 1, quantile, probs = c(0, 1))),
                     apply(e, 1, mean),
                     rep(e_cm0, t / dt))
  
  #### Panel a) - Evasion dynamics ####
  
  #create a data.table to store optimal evasion graph
  graph_e_dt <- data.table(
    time = rep(seq(dt, t, dt), 3),
    y = c(e_measures[, 2],
          e_measures[, 3],
          e_measures[, 4]),
    group = c(rep("q1", length(seq(
      dt, t, dt
    ))),
    rep("mean", length(seq(
      dt, t, dt
    ))),
    rep("c0", length(seq(
      dt, t, dt
    )))),
    beta = as.character(beta_temp)
  )
  
  #set the group variable as a factor and order
  graph_e_dt$group_fac <-
    factor(graph_e_dt$group, levels = c("mean", "q1", "c0"))
  
  #create a data.table to store the 0 quantile (we plot it on a separate layer to keep the legend with just 3 entries)
  q0_e_dt <- data.table(time = seq(dt, t, dt),
                        q0 = e_measures[, 1],
                        beta = as.character(beta_temp))
  
  #### Panel b) - Ratio of consumption to capital dynamics ####
  
  #create a data.table to store optimal ratio of consumption to capital graph
  graph_ck_dt <- data.table(
    time = c(seq(dt, t, dt),
             seq(dt, t, dt),
             seq(dt, t, dt)),
    y = c(ck_measures[, 2],
          ck_measures[, 3],
          ck_measures[, 4]),
    group = c(rep("q1", length(seq(
      dt, t, dt
    ))),
    rep("mean", length(seq(
      dt, t, dt
    ))),
    rep("c0", length(seq(
      dt, t, dt
    )))),
    beta = as.character(beta_temp)
  )
  
  #set the group variable as a factor and order
  graph_ck_dt$group_fac <-
    factor(graph_ck_dt$group, levels = c("mean", "q1", "c0"))
  
  
  #create a data.table to store data for the 0 quantile (we plot it on a separate layer to keep the legend with just 3 entries)
  q0_c_dt <- data.table(time = seq(dt, t, dt),
                        q0 = ck_measures[, 1],
                        beta = as.character(beta_temp))
  
  list(
    "graph_ck_dt" = graph_ck_dt,
    "q0_c_dt" = q0_c_dt,
    "graph_e_dt" = graph_e_dt,
    "q0_e_dt" = q0_e_dt
  )
})
names(list_optim) <- paste0("beta = ", betas)

#create a data.table to store optimal evasion dynamics under the two betas
graph_e_dt <- rbindlist(list(
  list_optim$`beta = 0`$graph_e_dt,
  list_optim$`beta = 0.5`$graph_e_dt
))
q0_e_dt <- rbindlist(list(list_optim$`beta = 0`$q0_e_dt,
                          list_optim$`beta = 0.5`$q0_e_dt))

#create a data.table to store optimal ratio of consumption to capital graph under the two betas
graph_ck_dt <- rbindlist(list(
  list_optim$`beta = 0`$graph_ck_dt,
  list_optim$`beta = 0.5`$graph_ck_dt
))
q0_c_dt <- rbindlist(list(list_optim$`beta = 0`$q0_c_dt,
                          list_optim$`beta = 0.5`$q0_c_dt))

#plot panel a)
graph_e_plot <- ggplot(data = graph_e_dt) +
  
  geom_line(aes(
    x = time,
    y = y,
    linetype = group_fac,
    colour = beta
  ), size = size_lyx) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash"),
    labels = c('Evasion - Quantiles (0,1)',
               'Evasion - Mean',
               'Evasion - $c_m=0$')
  ) +
  scale_color_manual(values = c("grey", "black"),
                     guide = "none") +
  geom_line(
    data = q0_e_dt,
    aes(x = time, y = q0, colour = beta),
    linetype = "solid",
    size = size_lyx
  ) +
  
  labs(y = "Evasion $e_t$",
       x = "Years",
       tag = "a)")  +
  
  theme_bw() +
  
  coord_cartesian(xlim = c(0, 50.02),
                  ylim = c(0.1, .5),
                  expand = FALSE) +
  
  theme(
    plot.title = element_text(size = rel(1), hjust = .5),
    legend.text = element_text(size = rel(.65)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = grid::unit(1, "lines"),
    legend.key.width = unit(.875, "cm"),
    legend.background = element_rect(colour = "white"),
    legend.box.background = element_rect(colour = "white"),
    legend.position = c(0.75, 0.2),
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
    plot.background = element_blank()
  )
graph_e_plot

#plot panel b)
graph_ck_plot <- ggplot(data = graph_ck_dt) +
  
  geom_line(aes(
    x = time,
    y = y,
    linetype = group_fac,
    colour = beta
  ), size = size_lyx) +
  
  
  geom_line(data = q0_c_dt,
            aes(x = time, y = q0, color = beta),
            linetype = "solid") +
  
  scale_color_manual(values = c("grey", "black"),
                     guide = "none") +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash"),
    labels = c(
      "$c_t/k_t$ - Quantiles (0,1)",
      "$c_t/k_t$ - Mean",
      "$c_t/k_t$ - $c_m=0$"
    )
  ) +
  
  labs(y = "Consumption to Capital ratio $c_t/k_t$",
       x = "Years",
       tag = "b)")  +
  
  theme_bw() +
  
  coord_cartesian(
    xlim = c(0, 50.02),
    ylim = c(0.1475, .19),
    expand = FALSE
  ) +
  
  theme(
    plot.title = element_text(size = rel(1), hjust = .5),
    legend.text = element_text(size = rel(.65)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = grid::unit(1, "lines"),
    legend.key.width = unit(.875, "cm"),
    legend.background = element_rect(colour = "white"),
    legend.box.background = element_rect(colour = "white"),
    legend.position = c(0.75, 0.8),
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
    plot.background = element_blank()
  ) +
  guides(linetype = guide_legend(order = 1))
graph_ck_plot

#plot Figure 1
graph_e_plot / graph_ck_plot

#define variable to store errors in tikz compilation
tmptikz_err <- NULL

#tikz plot
plotname <- "Figure_1"
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
  height = 7,
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

graph_e_plot / graph_ck_plot

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
