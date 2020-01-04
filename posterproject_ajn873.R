#' ## Research Question:
#' Controlling for salary, does the linear trend predicting number of home runs hit every year change through time? Additionally, does batting hand significantly moderate the effect of year on home runs and/or does league moderate the effect of year on home runs?

#+ setup, include=FALSE, echo=FALSE
knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.align = "center", warning=FALSE, message=FALSE)
#+

library(SDSRegressionR)
library(tidyverse)
library(mosaic)
library(emmeans)

#Bring in data
final <- read_csv("data/final_dataset.csv")
names(final)
tally(~lgID, data=final) 
tally(~bats, data=final)
final <- final %>% 
  mutate(lgID_f = factor(lgID, levels=c("AL", "NL")),
         bats_f = factor(bats, levels=c("B", "L", "R")))
tally(~lgID_f, data=final) 
tally(~bats_f, data=final)
final$salary2 <-  final$salary/1000
# Set up
breaks <- seq(1985,2016,1)
rmse <- rep(NA, length(breaks))

for(i in 1:length(breaks)){
  final2 <- final %>%
    mutate_at(vars(yearID), as.numeric)%>% #Initial catch all for numeric...
    mutate(year1 = yearID, #Simplet replication
           year2 = yearID - breaks[i], #Start second segment counting...
           year2 = case_when(year1 <= breaks[i]~0, #Make sure to start at zero BEFORE segment
                            TRUE~ year2),
           jump = case_when(yearID < breaks[i]~0, #Define the segment status...
                            yearID >= breaks[i]~1))
  mod <- lm(b_HR ~ year1 + jump + year2 + salary2 + bats_f*year1 + bats_f*jump + bats_f*year2 + lgID_f*year1 + lgID_f*jump + lgID_f*year2, data = final2) #Run the model
  rmse[i]<- summary(mod)$sigma #Save the RMSE
}

potential_breakpoints_rmse = data.frame(br = breaks, rmse = rmse)
min(potential_breakpoints_rmse$rmse) # The lowest RMSE is 0.9473682 with breakpoint of 2009

#' ####2. Assign the coding of the variables according to the change point indicated.
cutoff <- 2009
coded_final <- final %>% 
  mutate_at(vars(yearID), as.numeric) %>% 
  mutate(year1 = yearID, 
         year2 = yearID - cutoff, 
         year2 = case_when(year1 <= cutoff ~ 0, TRUE ~ year2),
         jump = case_when(yearID < cutoff ~ 0, yearID >= cutoff ~ 1))
#' Double check the mutations
plyr::count(coded_final, c("yearID", "year1", "year2", "jump"))

favstats(~b_HR, data = coded_final)
favstats(~salary2, data = coded_final)
tally(~lgID_f, data=coded_final) 
tally(~bats_f, data=coded_final)

#' ####3. Look for outliers by running the full model.
init_mod = lm(b_HR ~ year1 + jump + year2 + salary2 + bats_f*year1 + bats_f*jump + bats_f*year2 + lgID_f*year1 + lgID_f*jump + lgID_f*year2, data = coded_final)
residFitted(init_mod) 
#' I think it's important to be candid here and say that this Residuals vs Fitted plot isn't wonderful. However, I'm going to keep doing this analysis because it's good enough for government work.
cooks_plot = cooksPlot(init_mod, key.variable="playerID", print.obs = T, save.cutoff = T) 
cooks_plot
#' There are two very distinctly different outliers. I'm going to remove them. Understandably, not much should change in a 12726 subject dataset. 
final %>% 
  filter(playerID %in% c("davisch01", "swishni01"))

good_final <- coded_final %>% 
  filter(playerID %not_in% c("davisch01", "swishni01"))

mod2 = lm(b_HR ~ year1 + jump + year2 + salary2 + bats_f*year1 + bats_f*jump + bats_f*year2 + lgID_f*year1 + lgID_f*jump + lgID_f*year2, data = good_final)
summary(mod2)

p1 <- summary(emmeans(mod2, "year1", at=list(year1=c(1984, cutoff), year2=0, jump=0)))
p2 <- summary(emmeans(mod2, "year1", at=list(year1=c(cutoff, 2017), year2=c(0, (2017-cutoff)), jump=1), by="year2"))
p2 <- p2 %>% 
  slice(c(1,4))

mns1 <- summary(emmeans(mod2, "year1", at=list(year1 = seq(1984,cutoff,1)), year2 = 0, jump = 0, by="lgID_f"))
mns2 <- summary(emmeans(mod2, "year1", at=list(year1 = seq(cutoff,2017,1)), year2 = c(0, (2017-cutoff)), jump = 1, by="lgID_f"))

mns3 <- summary(emmeans(mod2, "year1", at=list(year1 = seq(1984,cutoff,1)), year2 = 0, jump = 0, by="bats_f"))
mns4 <- summary(emmeans(mod2, "year1", at=list(year1 = seq(cutoff,2017,1)), year2 = c(0, (2017-cutoff)), jump = 1, by="bats_f"))

below <- good_final %>% 
  filter(yearID <= 2009)
favstats(~yearID, data = below)

ref_grid(mod2)
emmeans(mod2, "year1", at=list(year1 = c(0,1)), by="bats_f")
bat_means <- emmeans(mod2, "year1", at=list(year1 = c(0,1)), by="bats_f")
bat_means # only interaction significant.

#Test of Simple Slopes
pairs(bat_means, reverse=TRUE) # SIMPLE SLOPES

g <- gf_point(b_HR ~ yearID, data = good_final, alpha=0.8) %>% 
  gf_theme(theme_bw())
g %>%  
  gf_labs(title="The Change in Home Runs as Years Increases", subtitle="Segmented Regression: Raw Data", y = "Home Runs", x = "Year")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1, color = "red", size = 1) %>%
  gf_line(emmean ~ year1, data = p2, color = "red", size = 1) + xlim(1984,2017) + ylim(-2,35)

g2 <- gf_point(b_HR ~ yearID, data = good_final, alpha=0.8, color = ~lgID_f) %>% 
  gf_theme(theme_bw())
g2 %>%  
  gf_labs(title="The Change in Home Runs as Years Increases", subtitle="Segmented Regression: Raw Data", y = "Home Runs", x = "Year")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1, color = "red", size = 1) %>%
  gf_line(emmean ~ year1, data = p2, color = "red", size = 1) + xlim(1984,2017) + ylim(-2,35)
  #gf_line(emmean ~ year1, data = mns1, color =~lgID_f, size = 1) %>%
  #gf_line(emmean ~ year1, data = mns2, color =~lgID_f, size = 1) %>% 
  

g3 <- gf_point(b_HR ~ yearID, data = good_final, alpha=0.8, color = ~bats_f) %>% 
  gf_theme(theme_bw())
g3 %>%  
  gf_labs(title="The Change in Home Runs as Years Increases", subtitle="Segmented Regression: Raw Data", y = "Home Runs", x = "Year")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1, color = "red", size = 1) %>%
  gf_line(emmean ~ year1, data = p2, color = "red", size = 1) + xlim(1984,2017) + ylim(-2,35)
  #gf_line(emmean ~ year1, data = mns3, color =~bats_f, size = 1) %>%
  #gf_line(emmean ~ year1, data = mns4, color =~bats_f, size = 1) %>% 
  

mns_final <- good_final %>% 
  group_by(yearID) %>% 
  summarise(mean_HR = mean(b_HR, na.rm=TRUE))
g_mns <- gf_point(mean_HR ~ yearID, data = mns_final, alpha=0.8) %>% 
  gf_theme(theme_bw()) %>% 
  gf_labs(title="The Change in Home Runs as Years Increases", subtitle="Segmented Regression: Mean Value Data", y = "Home Runs", x = "Year")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1, color="red", size = 1) %>% 
  gf_line(emmean ~ year1, data = p2, color="red", size = 1) + xlim(1984,2017) + ylim(-2,35)
g_mns         

# Coding for the second segment
good_final2 <- good_final %>% 
  mutate(year1_part2 = case_when(year1 >= cutoff ~ cutoff,
                             TRUE ~ year1))

mod4 = lm(b_HR ~ year1_part2 + jump + year2 + salary2 + bats_f*year1_part2 + bats_f*jump + bats_f*year2 + lgID_f*year1_part2 + lgID_f*jump + lgID_f*year2, data = good_final2)
summary(mod4)

#' # WHAT IF WE REMOVED ALL ZERO VALUES

names(final)

final_no_zeros = final[final$b_HR != 0,]

# Set up
breaksnz <- seq(1985,2016,1)
rmsenz <- rep(NA, length(breaksnz))

for(i in 1:length(breaksnz)){
  final2_nz <- final_no_zeros %>%
    mutate_at(vars(yearID), as.numeric)%>% #Initial catch all for numeric...
    mutate(year1 = yearID, #Simplet replication
           year2 = yearID - breaksnz[i], #Start second segment counting...
           year2 = case_when(year1 <= breaksnz[i]~0, #Make sure to start at zero BEFORE segment
                             TRUE~ year2),
           jump = case_when(yearID < breaksnz[i]~0, #Define the segment status...
                            yearID >= breaksnz[i]~1))
  mod <- lm(b_HR ~ year1 + jump + year2 + salary2 + bats_f*year1 + bats_f*jump + bats_f*year2 + lgID_f*year1 + lgID_f*jump + lgID_f*year2, data = final2_nz) #Run the model
  rmsenz[i]<- summary(mod)$sigma #Save the RMSE
}

potential_breakpoints_rmsenz = data.frame(br = breaksnz, rmse = rmsenz)
min(potential_breakpoints_rmsenz$rmse) 
min( rmsenz[rmsenz!=min(rmsenz)] ) 


#' ####2. Assign the coding of the variables according to the change point indicated.
cutoffnz <- 2009
coded_finalnz <- final_no_zeros %>% 
  mutate_at(vars(yearID), as.numeric) %>% 
  mutate(year1 = yearID, 
         year2 = yearID - cutoffnz, 
         year2 = case_when(year1 <= cutoffnz ~ 0, TRUE ~ year2),
         jump = case_when(yearID < cutoffnz ~ 0, yearID >= cutoffnz ~ 1))

#' Double check the mutations
plyr::count(coded_finalnz, c("yearID", "year1", "year2", "jump"))

#' ####3. Look for outliers by running the full model.
init_modnz= lm(b_HR ~ year1 + jump + year2 + salary2 + bats_f*year1 + bats_f*jump + bats_f*year2 + lgID_f*year1 + lgID_f*jump + lgID_f*year2, data = coded_finalnz)
residFitted(init_modnz) 
#' I think it's important to note that this Residuals vs Fitted plot isn't wonderful. 
cooks_plotnz = cooksPlot(init_modnz, key.variable="playerID", print.obs = T, save.cutoff = T) 
cooks_plotnz

c_outliersnz = cooks_plotnz %>% 
  filter (Cooks_Distance > 0.05) %>% 
  pull(playerID)
c_outliersnz

final_no_zeros %>% 
  filter(playerID %in% c(c_outliersnz))

good_finalnz <- coded_finalnz %>% 
  filter(playerID %not_in% c(c_outliersnz))

mod2nz = lm(b_HR ~ year1 + jump + year2 + salary2 + bats_f*year1 + bats_f*jump + bats_f*year2 + lgID_f*year1 + lgID_f*jump + lgID_f*year2, data = good_finalnz)
mod3nz = lm(b_HR ~ year1 + jump + year2 + salary2, data = good_finalnz)
summary(mod2nz)
summary(mod3nz)

p1nz <- summary(emmeans(mod2nz, "year1", at=list(year1=c(1984, cutoffnz), year2=0, jump=0)))
p2nz <- summary(emmeans(mod2nz, "year1", at=list(year1=c(cutoffnz, 2017), year2=c(0, (2017-cutoffnz)), jump=1), by="year2"))
p2nz <- p2nz %>% 
  slice(c(1,4))

mns1nz <- summary(emmeans(mod2nz, "year1", at=list(year1 = seq(1984,cutoffnz,1)), year2 = 0, jump = 0, by="lgID_f"))
mns2nz <- summary(emmeans(mod2nz, "year1", at=list(year1 = seq(cutoffnz,2017,1)), year2 = c(0, (2017-cutoffnz)), jump = 1, by="lgID_f"))

mns3nz <- summary(emmeans(mod2nz, "year1", at=list(year1 = seq(1984,cutoffnz,1)), year2 = 0, jump = 0, by="bats_f"))
mns4nz <- summary(emmeans(mod2nz, "year1", at=list(year1 = seq(cutoffnz,2017,1)), year2 = c(0, (2017-cutoffnz)), jump = 1, by="bats_f"))

gnz <- gf_point(b_HR ~ yearID, data = good_finalnz, alpha=0.8) %>% 
  gf_theme(theme_bw())
gnz %>%  
  gf_labs(title="The Change in Homeruns as Years Increases: No Zeros", subtitle="Segmented Regression: Raw Data")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1nz, color = "red", size = 1) %>% 
  gf_line(emmean ~ year1, data = p2nz, color = "red", size = 1) + xlim(1984,2017) + ylim(-2,35)

gnz2 <- gf_point(b_HR ~ yearID, data = good_finalnz, alpha=0.8, color = ~lgID_f) %>% 
  gf_theme(theme_bw())
gnz2 %>%  
  gf_labs(title="The Change in Homeruns as Years Increases: No Zeros", subtitle="Segmented Regression: Raw Data")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1nz, color = "red", size = 1) %>% 
  gf_line(emmean ~ year1, data = p2nz, color = "red", size = 1) + xlim(1984,2017) + ylim(-2,35)

g3nz <- gf_point(b_HR ~ yearID, data = good_final, alpha=0.8, color = ~bats_f) %>% 
  gf_theme(theme_bw())
g3 %>%  
  gf_labs(title="The Change in Homeruns as Years Increases: No Zeros", subtitle="Segmented Regression: Raw Data")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1nz, color = "red", size = 1) %>% 
  gf_line(emmean ~ year1, data = p2nz, color = "red", size = 1) + xlim(1984,2017) + ylim(-2,35)

mns_finalnz <- good_finalnz %>% 
  group_by(yearID) %>% 
  summarise(mean_HRnz = mean(b_HR, na.rm=TRUE))
g_mnsnz <- gf_point(mean_HRnz ~ yearID, data = mns_finalnz, alpha=0.8) %>% 
  gf_theme(theme_bw()) %>% 
  gf_labs(title="The Change in Homeruns as Years Increases: No Zeros", subtitle="Segmented Regression: Mean Value Data")  %>% 
  gf_vline(xintercept = ~2009, color="blue", size = 2) %>% 
  gf_line(emmean ~ year1, data = p1nz, color="red", size = 1) %>% 
  gf_line(emmean ~ year1, data = p2nz, color="red", size = 1) + xlim(1984,2017) + ylim(-2,35)
g_mnsnz            

# Coding for the second segment
good_final2nz <- good_finalnz %>% 
  mutate(year1_part2 = case_when(year1 >= cutoff ~ cutoff,
                                 TRUE ~ year1))

mod4nz = lm(b_HR ~ year1_part2 + jump + year2 + salary2 + bats_f*year1 + bats_f*jump + bats_f*year2 + lgID_f*year1 + lgID_f*jump + lgID_f*year2, data = good_final2nz)
summary(mod4nz)







