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


make_5b <- function(d,mx_i = "i", my_i = "g"){
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
    ggplot(aes_string(x="cohort",y="value", color = "model"))+
    geom_line(aes_string(y="value"),size=.7,alpha=.8)+
    geom_point(aes_string(y="value"),size=6, shape=20)+
    geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)+
    scale_color_manual(values = model_colors)
  g
}

dsModelsParsLong %>%  make_5b("i","g")


make_simple_traj <- 

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
