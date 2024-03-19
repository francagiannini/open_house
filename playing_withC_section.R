devtools::install("C:/Users/au710823/OneDrive - Aarhus universitet/rCTOOL", force = TRUE)

library(tidyverse)

library(rCTOOL)
library(ggthemes)

# plots setup

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# set simulation parameters ----
period = define_timeperiod(yr_start = 1, yr_end = 100)#+1974

management = management_config(
  manure_monthly_allocation = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  plant_monthly_allocation = c(0, 0, 0, 8, 12, 16, 64, 0, 0, 0, 0, 0) / 100
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

temp = list(Tavg =rep(c(0.46,0.41,2.45,6.06,10.63,13.78,15.73,15.73,12.45,8.59,4.62,1.86),ysim),
            Trange_col = rep(c(4,5,6,8,9,9,9,8,7,6,5,5),ysim))

soil_pools = initialize_soil_pools(soil_config = soil)

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

# run simulations ----
ctool_fb = run_ctool(time_config = period, 
                  cin_config = cin_fb, 
                  m_config = management, 
                  t_config = temp, 
                  s_config = soil, 
                  soil_pools = soil_pools)

# check
# initial <- soil$Csoil_init
# int = sum(cin_fb$Cin_top) + sum(cin_fb$Cin_sub) + sum(cin_fb$Cin_man)
# st = ctool_fb$C_topsoil[period$steps] + ctool_fb$C_subsoil[period$steps]
# em = sum(ctool_fb$em_CO2_top) + sum(ctool_fb$em_CO2_sub)
# (initial + int) - (st + em)

ctool_orgdf = run_ctool(time_config = period, 
                     cin_config = cin_orgdf, 
                     m_config = management, 
                     t_config = temp, 
                     s_config = soil, 
                     soil_pools = soil_pools)
#system.time(
ctool_pet= run_ctool(time_config = period, 
                        cin_config = cin_pet, 
                        m_config = management, 
                        t_config = temp, 
                        s_config = soil, 
                        soil_pools = soil_pools)
#)

## Exploring results----

#ctool_fb |> 
#ctool_orgdf |> 
ctool_pet |> 
  pivot_longer(cols = c(C_subsoil,C_topsoil), 
               names_to = "depth",values_to = "SOC" )|>
  ggplot(aes(x=yrs,y=SOC,fill=depth))+
  geom_col()

pool_cols=values = c(FOM_top="#C52E19", FOM_sub="#54D8B1",
                     HUM_top="#b67c3b", HUM_sub="#5F5647",
                     ROM_top="#175149", ROM_sub="#0C1707")

#ctool_fb |> 
#ctool_orgdf |> 
ctool_pet |> 
  as.data.frame() |> 
  mutate(date=make_date(year=1973+yrs, month=mon)) |> 
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
  ctool_fb,
  "C_topsoil_pet" = ctool_pet[, 'C_topsoil'],
  "C_topsoil_orgdf" = ctool_orgdf[, 'C_topsoil'],
  "C_topsoil_fball" = ctool_fb[, 'C_topsoil']
) |> as.data.frame() |> 
  mutate(date=make_date(year=1973+yrs, month=mon)) |> 
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

cbind(
  ctool_fb,
  "C_subsoil_pet" = ctool_pet[, 'C_subsoil'],
  "C_subsoil_orgdf" = ctool_orgdf[, 'C_subsoil'],
  "C_subsoil_fball" = ctool_fb[, 'C_subsoil']
) |> as.data.frame() |> 
  mutate(date=make_date(year=1973+yrs, month=mon)) |> 
  pivot_longer(
    cols = starts_with('C_subsoil_'), 
    names_to = 'landuse',
    values_to = 'csubsoil' ) |> 
  ggplot(aes(x=date,y=csubsoil,col=landuse))+
  geom_point(alpha=0.5)+
  geom_line()+
  scale_x_date(date_labels ="%Y",date_breaks = "5 years")+
  scale_color_colorblind()+
  #geom_vline(xintercept =),linetype="dotted")+
  labs(y="SOC tn/ha", x="time")+
  #ylim+
  #scale_y_continuous(limits=c(20,60),breaks = seq(0,140,10))+
  theme(text=element_text(size=20),axis.text.x = element_text(angle = 40, hjust = 1))
