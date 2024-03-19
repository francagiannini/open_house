library(tidyverse)
#devtools::install("C:/Users/au710823/OneDrive - Aarhus universitet/rCTOOL")
library(rCTOOL)
library(ggthemes)

# plots setup

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# set simulation parameters ----
period = define_timeperiod(yr_start = 1, yr_end = 100)#+1974

management = management_config(
  manure_monthly_allocation = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  plant_monthly_allocation = c(0, 0, 0, 8, 12, 16, 64, 0, 0, 0, 0, 0) / 100,
  time_config = period
) # set to default

soil = soil_config(Csoil_init = 100,
                   f_hum_top = 0.4803,
                   f_rom_top = 0.4881,
                   f_hum_sub = 0.3123,
                   f_rom_sub = 0.6847,
                   Cproptop = 0.47,
                   clay_top = 0.1,
                   clay_sub = 0.15,
                   phi = 0.035,
                   f_co2 = 0.628,
                   f_romi = 0.012,
                   k_fom  = 0.12,
                   k_hum = 0.0028, 
                   k_rom = 3.85e-5,
                   ftr = 0.003,
                   cn = 10)
## Football court ryegrass (Lolium perenne) ----
# burn out period ww ctop 1.242	c=sub 0.123 

# anually input 4 MgC/ha rg
ysim <- 100

cin_fb = define_Cinputs(
  Cin_top = c(rep(1.69,ysim*0.5),rep(4*0.9,ysim*0.5)),
  Cin_sub = c(rep(0.123 ,ysim*0.5),rep(4*0.1,ysim*0.5)),
  Cin_man = rep(0,ysim),
  time_config = period
)

# Organic Dairy farm ----
# 
# burn out period ww ctop 1.242	c=sub 0.123 
# anually input 
#top  
#c(5.048423377, 4.565862857, 4.26855974, 0.826910723)
#sub 
#0.39855974, 0.360462857, 0.336991558, 0.081848514, 0.081848514,
#man 
#1.83, 1.83, 1.83, 0.48, 0.87

cin_orgdf = define_Cinputs(
  Cin_top = c(rep(1.69,ysim*0.5),rep(c(5.05,5.05,4.57,4.27,0.83),(ysim*0.5)/5)),
  Cin_sub = c(rep(0.123,ysim*0.5),rep(c(0.40,0.36,0.34,0.08,0.08),(ysim*0.5)/5)),
  Cin_man = c(rep(0,ysim*0.5),rep(c(1.83, 1.83, 1.83, 0.48, 0.87),(ysim*0.5)/5)),
  time_config = period
)

# Viborg pet cementery ----

# 1 dog 1.45kg C subsoil 
# 1 cat 0.25 kg C subsoil 
# 1 pet each 5 persons 
# 45000*(1/5)/7 
# 60% of the surface of grass other stones 
# 1020 kg C per year 
# 
cin_pet = define_Cinputs(
  Cin_top = c(rep(1.69,ysim*0.5),rep(3.5*0.9,ysim*0.5)),
  Cin_sub = c(rep(0.123,ysim*0.5), c(rep(3.5*0.1,ysim*0.5)+rep(1.02,ysim*0.5))),
  Cin_man = rep(0,ysim),
  time_config = period
)


temp = list('Tavg' = rep(c(4,5,6,8,9,9,9,8,7,6,5,5),ysim),
            'Tavg_col'=rep(c(0.46,0.41,2.45,6.06,10.63,13.78,15.73,15.73,12.45,8.59,4.62,1.86),ysim))

soil_pools = initialize_soil_pools(soil_config = soil)

# run simulation ----
ctool_fb = run_ctool(time_config = period, 
                  cin_config = cin_fb, 
                  m_config = management, 
                  t_config = temp, 
                  s_config = soil, 
                  soil_pools = soil_pools)


initial <- soil$Csoil_init
int = sum(cin$Cin_top) + sum(cin$Cin_sub) + sum(cin$Cin_man)
st = ctool$C_topsoil[60] + ctool$C_subsoil[60]
em = sum(ctool$em_CO2_top) + sum(ctool$em_CO2_sub)
(initial + int) - (st + em)

ysim <- 100

## Input setup -----

### Temperature ----- 
T_ave <- rep(c(0.46,0.41,2.45,6.06,10.63,13.78,15.73,15.73,12.45,8.59,4.62,1.86),ysim)

T_range <- rep(c(4,5,6,8,9,9,9,8,7,6,5,5),ysim)


# Monthly distribution of C inputs taken from previous version 
# assuming that C inputs comes from grain crops 
# and the Organic fertilization is all made in march  

month_prop_grain <- c(0,0,0,8,12,16,64,0,0,0,0,0)/100

month_prop_grass <-c(1,1,2,7,12,15,17,16,14,9,5,1)/100

month_man <- c(0,0,100,0,0,0,0,0,0,0,0,0)/100

# Fraction of manure that we consider is already Humidified

fman <- 0.192

### Soil ----
#Parameters referring to site-specific soil conditions 

# Initial C stock at 1m depth 
Cinit <- 100

# Proportion of the total C allocated in topsoil

Cproptop <- 0.47

clay_top <- 0.1

clay_sub <- 0.15

# Diffusion index 
phi <- 0.035

# respiration fraction
fco2 <- 0.628

# romification fraction
fromi <- 0.012

# decomposition rates 
kFOM <- 0.12

kHUM <- 0.0028

kROM <- 3.858e-05

# transport rate
ftr <- .003

# initial pool distribution
fHUM_top <- 0.4803

fROM_top <- 0.4881 

fHUM_sub <- 0.3123

fROM_sub <- 0.6847 

# CN relation
CN <- 10

## Pre-Processing for time 0 ----
# initial_values

startCAmount_top <- Cinit * Cproptop
startCAmount_sub <- Cinit * (1-Cproptop)

init_pool_top <-pool_cn(cn=CN,
                        HUM_frac = fHUM_top,
                        ROM_frac = fROM_top,
                        C_0=startCAmount_top)|> t()

colnames(init_pool_top) <- paste(colnames(init_pool_top), "top", sep = "_")

init_pool_sub <-pool_cn(cn=CN,
                        HUM_frac = fHUM_sub,
                        ROM_frac = fROM_sub,
                        C_0=startCAmount_sub) |> t()

colnames(init_pool_sub)<-paste(colnames(init_pool_sub),"sub",sep="_")

# time period 

y=seq(1,ysim,1) 
m=seq(1,12,1)
# 
initial_value <-
  cbind(
    "step" = 1,
    "yr" = y[1],
    "mth" = 0,
    init_pool_top,
    init_pool_sub,
    "C_topsoil" = NA,
    "C_subsoil" = NA,
    
    "FOM_tr" = NA,
    "HUM_tr" = NA,
    "ROM_tr" = NA,
    
    "C_tr" = NA,
    
    "CO2_FOM_top" = NA,
    "CO2_HUM_top" = NA,
    "CO2_ROM_top" = NA,
    
    "CO2_FOM_sub" = NA,
    "CO2_HUM_sub" = NA,
    "CO2_ROM_sub" = NA,
    
    "C_CO2_top" = NA,
    "C_CO2_sub" = NA
  )

# Defining number of steps in the complete run 

nsteps <- as.list(seq(1, length(y) * length(m)+1, 1))


## Football court ryegrass (Lolium perenne) ----
# burn out period ww ctop 1.242	c=sub 0.123 

# anually input 4 MgC/ha rg

C_input_top <- c(rep(1.69,ysim*0.5),rep(4*0.9,ysim*0.5))

C_input_sub <- c(rep(0.123 ,ysim*0.5),rep(4*0.1,ysim*0.5))

C_input_man <- rep(0,ysim)

Crop <- rep("Grass",ysim)


### Run Turnover core function -----

result_pools <-
  matrix(ncol = length(initial_value), nrow = length(nsteps))

colnames(result_pools) <- colnames(initial_value)

result_pools[1, ] <- initial_value

system.time(
  for (i in 2:length(nsteps)) {
    result_pools[i, ] <- rCTOOL::turnover(i)
  }
  
  
)

result_fball = result_pools

# Balance

input <- 
  sum(C_input_top)+sum(C_input_sub)+sum(C_input_man)

SOC_stock <- result_pools[length(nsteps),"C_topsoil"] +
  result_pools[length(nsteps),"C_subsoil"]

emited <-sum(result_pools[,"C_CO2_top"],na.rm = TRUE) +
  sum(result_pools[,"C_CO2_sub"],na.rm = TRUE)


as.numeric(Cinit+input) == as.numeric(SOC_stock+emited)
as.numeric(Cinit+input) - as.numeric(SOC_stock+emited)
isTRUE(all.equal(as.numeric(Cinit+input),as.numeric(SOC_stock+emited)))

# Organic Dairy farm ----
# 
# burn out period ww ctop 1.242	c=sub 0.123 
# anually input 
#top  
#c(5.048423377, 4.565862857, 4.26855974, 0.826910723)
#sub 
#0.39855974, 0.360462857, 0.336991558, 0.081848514, 0.081848514,
#man 
#1.83, 1.83, 1.83, 0.48, 0.87

C_input_top <- c(rep(1.69,ysim*0.5),rep(c(5.05,5.05,4.57,4.27,0.83),(ysim*0.5)/5))

C_input_sub <- c(rep(0.123,ysim*0.5),rep(c(0.40,0.36,0.34,0.08,0.08),ysim*0.5))

C_input_man <- c(rep(0,ysim*0.5),rep( c(1.83, 1.83, 1.83, 0.48, 0.87),ysim/5))

Crop <- rep(c("Grass","Grass", "Grass","Grain","Grain"),ysim/5)

result_pools <-
  matrix(ncol = length(initial_value), nrow = length(nsteps))

colnames(result_pools) <- colnames(initial_value)

result_pools[1, ] <- initial_value

system.time(
  for (i in 2:length(nsteps)) {
    result_pools[i, ] <- rCTOOL::turnover(i)
  }
  
  
)

result_orgdf = result_pools


# Viborg pet cementery ----

# 1 dog 1.45kg C subsoil 
# 1 cat 0.25 kg C subsoil 
# 1 pet each 5 persons 
# 45000*(1/5)/7 
# 60% of the surface of grass other stones 
# 1020 kg C per year 
# 
# burn out period ww ctop 1.242	c=sub 0.123 
# anually input 
# top  
#5.048423377
#4.565862857
#4.26855974
#0.826910723
# sub 
#0.39855974
#0.360462857
#0.336991558
#0.081848514
#0.081848514
#man 
#1.83
#1.83
#1.83
#0.48
#0.87

C_input_top <- c(rep(1.242,ysim*0.5), rep(4*0.9,ysim*0.5))

C_input_sub <- c(rep(0.123,ysim*0.5), (rep(4*0.1,ysim*0.5)+rep(1.02,ysim*0.5)))

C_input_man <- rep(0,ysim)

Crop <- c(rep("Grain", ysim*0.5), (rep("Grass",ysim*0.5)))

result_pools <-
  matrix(ncol = length(initial_value), nrow = length(nsteps))

colnames(result_pools) <- colnames(initial_value)

result_pools[1, ] <- initial_value

system.time(
  for (i in 2:length(nsteps)) {
    result_pools[i, ] <- rCTOOL::turnover(i)
  }
  
  
)

result_petsm = result_pools


## Exploring results----

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

#result_petsm |> 
#result_orgdf |> 
result_fball |> 
  as.data.frame() |> 
  pivot_longer(cols = c(C_subsoil,C_topsoil), 
               names_to = "depth",values_to = "SOC" )|>
  ggplot(aes(x=step,y=SOC,fill=depth))+
  geom_col()

pool_cols=values = c(FOM_top="#C52E19", FOM_sub="#54D8B1",
                     HUM_top="#b67c3b", HUM_sub="#5F5647",
                     ROM_top="#175149", ROM_sub="#0C1707")

#result_petsm |> 
#result_orgdf |> 
result_fball |> 
  as.data.frame() |> 
  mutate(date=make_date(year=1973+yr, month=mth)) |> 
  pivot_longer(
    cols = c("FOM_top","HUM_top","ROM_top",
             "FOM_sub","HUM_sub","ROM_sub"), 
    names_to = "pool",
    values_to = "SOC" )|>  
  mutate(pool = fct_relevel(pool, 
                            "FOM_top", "HUM_top","ROM_top", 
                            "FOM_sub","HUM_sub","ROM_sub"
  )) |> 
  ggplot(aes(x=date,y=SOC,fill=pool))+
  geom_col(position = "stack")+
  scale_fill_manual(values = pool_cols)+
  scale_x_date(date_labels ="%Y",date_breaks = "5 years")+
  #geom_vline(xintercept =),linetype="dotted")+
  labs(y="SOC tn/ha", x="time")+
  #ylim+
  scale_y_continuous(limits=c(0,140),breaks = seq(0,140,20))+
  theme(text=element_text(size=20),axis.text.x = element_text(angle = 40, hjust = 1))


#Ctop_open <- 
cbind(
  result_fball,
  "C_topsoil_pet" = result_petsm[, 'C_topsoil'],
  "C_topsoil_orgdf" = result_orgdf[, 'C_topsoil'],
  "C_topsoil_fball" = result_fball[, 'C_topsoil']
) |> as.data.frame() |> 
  mutate(date=make_date(year=1973+yr, month=mth)) |> 
  pivot_longer(
    cols = starts_with('C_topsoil_'), 
    names_to = 'landuse',
    values_to = 'ctopsoil' ) |> 
  ggplot(aes(x=date,y=ctopsoil,col=landuse))+
  geom_point(alpha=0.5)+
  geom_smooth()+
  scale_x_date(date_labels ="%Y",date_breaks = "5 years")+
  scale_color_colorblind()+
  #geom_vline(xintercept =),linetype="dotted")+
  labs(y="SOC tn/ha", x="time")+
  #ylim+
  scale_y_continuous(limits=c(20,60),breaks = seq(0,140,10))+
  theme(text=element_text(size=20),axis.text.x = element_text(angle = 40, hjust = 1))

