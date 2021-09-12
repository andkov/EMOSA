rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-sources ------------------------------------------------------------

source("./publication_update/common-functions.R")
source("./publication_update/graphing-settings.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(tibble)
library(cowplot)

# ---- declare-globals ---------------------------------------------------------

# ---- declare-functions -------------------------------------------------------

prints_folder <- paste0("./publication_update/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}


# ---- load-data ---------------------------------------------------------------
dto <- readr::read_rds( "Data/EMOSA_models.rds")

dsDICLong        <- dto[["dsDICLong"]]          %>% as_tibble()     
dsEMOSA          <- dto[["dsEMOSA"]]            %>% as_tibble() 
# dsLSC_catatrans  <- dto[["dsLSC_catatrans"]]    %>% as_tibble()         
# dsLSP_catatrans  <- dto[["dsLSP_catatrans"]]    %>% as_tibble()         
# dsModel          <- dto[["dsModel"]]            %>% as_tibble() 
# dsModelsPars     <- dto[["dsModelsPars"]]       %>% as_tibble()      
dsModelsParsLong <- dto[["dsModelsParsLong"]]   %>% as_tibble()                   
# dsOContPars      <- dto[["dsOContPars"]]        %>% as_tibble()     
# dsODiffPars      <- dto[["dsODiffPars"]]        %>% as_tibble()     
# dsOHybPars       <- dto[["dsOHybPars"]]         %>% as_tibble()    
# dsWS_catatrans   <- dto[["dsWS_catatrans"]]     %>% as_tibble()        
# dsWSC_catatrans  <- dto[["dsWSC_catatrans"]]    %>% as_tibble()         
# dsWSP_catatrans  <- dto[["dsWSP_catatrans"]]    %>% as_tibble()         
rm(dto)


# ---- inspect-data ------------------------------------------------------------



# ---- tweak-data --------------------------------------------------------------


# ---- table-1 -----------------------------------------------------------------

# ---- 5b-off-diagonal-function ----------------
make_simple_traj <- function(
  d
  ,xvar
  ,yvar
  ,cvar
  ,clevels = model_colors
){
  # d <- d1
  # xvar = "cohort"
  # yvar = "value"
  # cvar = "model"
  # clevels = model_colors
  
  g <- 
    d %>% 
    ggplot(aes_string(x=xvar, y = yvar, color = cvar))+
    geom_line(size=.7,alpha=.8)+
    geom_point(size=6, shape=20)+
    geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)+
    scale_color_manual(values = clevels)
  g
} 
# dsModelsParsLong %>%
#   filter(parameter == "Tia") %>% 
#   make_simple_traj("cohort","value","model")


make_5b <- function(d,mx_i = "i", my_i = "g", ...){
  # d <- dsModelsParsLong
  # mx_i = "i"
  # my_i = "g"
  
  d1 <- 
    d %>% 
    mutate(
      mx  = substr(parameter,2,2) %>% as_factor() %>% fct_relevel("g","i","a")
      ,my = substr(parameter,3,3)%>% as_factor() %>% fct_relevel("g","i","a")
    ) %>% 
    filter(
      mx == mx_i
      ,my == my_i
    ) 
  
  g <- 
    d1 %>% 
    make_simple_traj("cohort","value","model",...)
  g
}

# dsModelsParsLong %>%  make_5b("i","a")

# ---- 5b-on-diagonal-function ------------------

make_traj_obs_prevalence <- function(
  d
  ,group_i = "pG"
  ,xvar 
  ,yvar
  ,cvar
  ,clevels = catatransColor
){
  
  # d <- dsEMOSA
  # group_i = "pG"
  # xvar = "age"
  # # xvar = "time"
  # yvar = "obs_proportion"
  # cvar = "catatrans"
  # clevels = catatransColor
  
  d1 <- 
    d %>% 
    # filter(catatrans %in% c("pG","pI","pA")) %>% 
    filter(catatrans %in% group_i) %>% 
    mutate(
      cohort = as_factor(cohort) %>% fct_relevel(paste0(1984:1980))
      ,parcoh=paste(cohort,catatrans)
    )
  
  g <-
    d1 %>% 
    mutate(catatrans = factor(catatrans)) %>% 
    ggplot(aes_string(x = xvar, y = yvar, group = "parcoh", fill = cvar)) +
    geom_line(aes_string(colour = cvar), show.legend = FALSE)+
    geom_point(aes_string(colour= cvar),show.legend = FALSE)+
    scale_color_manual(values =clevels)
    # scale_y_continuous("Prevalence: proportion of total",
    #                    limits=c(0, .7),
    #                    breaks=c(0,.1,.2,.3,.4,.5,.6,.7)
    #                    , labels = RemoveLeadingZero
    # )+
    # scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
    #                    limits=c(16,30),
    #                    breaks=c(16,18,20,22,24,26,28,30))+
    # labs(title=paste0("Prevalence of church attendance"))
  g
}
dsEMOSA %>% make_traj_obs_prevalence("pG","age","obs_proportion", "catatrans") 


# ---- make-5b ---------------------

p_g <- dsEMOSA %>% make_traj_obs_prevalence("pG","age","obs_proportion", "catatrans") 
p_i <- dsEMOSA %>% make_traj_obs_prevalence("pI","age","obs_proportion", "catatrans") 
p_a <- dsEMOSA %>% make_traj_obs_prevalence("pA","age","obs_proportion", "catatrans") 

p_gi <- dsModelsParsLong %>% make_5b("g","i")
p_ga <- dsModelsParsLong %>% make_5b("g","a")
p_ig <- dsModelsParsLong %>% make_5b("i","g")
p_ia <- dsModelsParsLong %>% make_5b("i","a")
p_ag <- dsModelsParsLong %>% make_5b("a","g")
p_ai <- dsModelsParsLong %>% make_5b("a","i")


common_theme <- theme(
  legend.position = "none"
  ,panel.grid.minor = element_blank()
)


adj_on_both <- function(p){
  p <- p +
    scale_y_continuous(breaks = seq(0.1,.9,.2), labels = RemoveLeadingZero
                       ,limits = c(0,1))+
    geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)
  
}

adj_on_d <- function(p){
  p <- p +
    scale_x_continuous(breaks = seq(16, 32,2))+
    labs(y="observed")

}
adj_off_d <- function(p){
  p <- p +
    scale_x_continuous(breaks = 1980:1984)+
    labs(y="estimated")
  
}

# Diagonal
p_g  <- p_g  %>% adj_on_both %>% adj_on_d()  + common_theme + annotate("text", x=23,y=.6, label ="Goers", color="grey60", size=7)
p_i  <- p_i  %>% adj_on_both %>% adj_on_d()  + common_theme + annotate("text", x=23,y=.6, label ="Irregulars", color="grey60", size=7)
p_a  <- p_a  %>% adj_on_both %>% adj_on_d()  + common_theme + annotate("text", x=23,y=.8, label ="Absentees", color="grey60", size=7)
# Top
p_gi <- p_gi %>% adj_on_both %>% adj_off_d() + common_theme + annotate("text", x=1980.2,y=.05, label = sprintf('G\u2192I'), alpha =.5, size=5)
p_ga <- p_ga %>% adj_on_both %>% adj_off_d() + common_theme + annotate("text", x=1980.2,y=.05, label = sprintf('G\u2192A'), alpha =.5, size=5)
p_ia <- p_ia %>% adj_on_both %>% adj_off_d() + common_theme + annotate("text", x=1980.2,y=.05, label = sprintf('I\u2192A'), alpha =.5, size=5)
# Bottom
p_ai <- p_ai %>% adj_on_both %>% adj_off_d() + common_theme + annotate("text", x=1983.7,y=.95, label = sprintf('A\u2192I'), alpha =.5, size=5)
p_ag <- p_ag %>% adj_on_both %>% adj_off_d() + common_theme + annotate("text", x=1983.7,y=.95, label = sprintf('A\u2192G'), alpha =.5, size=5)
p_ig <- p_ig %>% adj_on_both %>% adj_off_d() + common_theme + annotate("text", x=1983.7,y=.95, label = sprintf('I\u2192G'), alpha =.5, size=5)

# look into adding an arrow:
# annotate("segment", x = 1982.5, xend = 1983, y = .9, yend = .9, colour = "grey60", size=1, alpha=0.6, arrow=arrow())

title_final <- cowplot::ggdraw()+cowplot::draw_label("Estimated parameter values for three models")
caption_final <- cowplot::ggdraw()+cowplot::draw_label("Note: Diagonal shows observed prevalence")
matrix_plot <- cowplot::plot_grid(

  p_g,  p_gi, p_ga,
  p_ig, p_i,  p_ia,
  p_ag, p_ai, p_a
  
  ,ncol = 3
  # rel_heights values control vertical title margins
  ,rel_heights = rep(1,9)
  ,rel_widths = rep(1,9)
)+
  theme(
    plot.background =element_rect(fill = "white", color = "white")
)
final_plot <- cowplot::plot_grid(
  title_final
  ,matrix_plot
  # ,caption_final
  ,ncol = 1
  ,rel_heights = c(.1, 1)
  
)
final_plot
# final_plot
# final_plot %>% quick_save(dto$model_name,width = 10, height = 5)
ggplot2::ggsave(
  filename = paste0("figure_5b",".jpg"),
  plot     = final_plot,
  device   = "jpg",
  path     = prints_folder,
  width    = 10,
  height   = 10,
  # units = "cm",
  dpi      = 'retina',
  limitsize = FALSE
  # ,...
)

# ---- figure-5b -----------------------------------------------------------------
g <- 
  dsModelsParsLong %>% 
  mutate(
    mx  = substr(parameter,2,2) %>% as_factor() %>% fct_relevel("g","i","a")
    ,my = substr(parameter,3,3)%>% as_factor() %>% fct_relevel("g","i","a")
  ) %>% 
  ggplot(aes(x = cohort, color = model))+
  facet_grid(mx~my)+
  geom_line(aes(y=value),size=.7,alpha=.8)+
  geom_point(aes(y=value),size=6, shape=20)+
  geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)+
  scale_y_continuous("value of parameter",
                     limits=c(0, 1),
                     breaks=c(.2,.4,.6,.8,1))+
  scale_color_manual(values = model_colors)+
  labs(title=paste0("Parameter values for 3 models"))
g


# ---- figure-5c --------------
# observed prevalence on the diagonal 
g <- 
  dsEMOSA %>% 
  filter(catatrans %in% c("pG","pI","pA")) %>% 
  mutate(
    cohort = as_factor(cohort) %>% fct_relevel(paste0(1984:1980))
    ,parcoh=paste(cohort,catatrans)
  ) %>% 
  ggplot(aes(x=age,y=obs_proportion,group=parcoh,fill=factor(catatrans)))+
  geom_line(aes(colour = catatrans), show.legend = FALSE)+
    # facet_grid(. ~ cohort)+
  geom_point(aes(colour=catatrans),show.legend = FALSE)+
  scale_color_manual(values =catatransColor)+
  scale_y_continuous("Prevalence: proportion of total",
                     limits=c(0, .7),
                     breaks=c(0,.1,.2,.3,.4,.5,.6,.7)
                     , labels = RemoveLeadingZero
                     )+
  scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
                     limits=c(16,30),
                     breaks=c(16,18,20,22,24,26,28,30))+
  labs(title=paste0("Prevalence of church attendance"))
g

# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
