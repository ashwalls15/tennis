library(plotly)
Sys.setenv("plotly_username"="awalls")
Sys.setenv("plotly_api_key"="PknK4qfeYjGhg2GSgFLh")

################# On Serve and Returning Win %s

library(tidyr)
atp <- read_csv("Desktop/Aus Open 2021 Datathon/Registration data pack/ATP_data_by_player.csv")
wta <- read_csv("Desktop/Aus Open 2021 Datathon/Registration data pack/WTA_data_by_player.csv")

ATP_onserve <- atp %>%
  drop_na(`1st_serve_won`) %>% 
  summarise(sum(service_points_won)/sum(service_points)) %>% 
  as.numeric()

ATP_return <- 1 - ATP_onserve

WTA_onserve <- wta %>%
  drop_na(`1st_serve_won`) %>% 
  summarise(sum(service_points_won)/sum(service_points)) %>% 
  as.numeric()

WTA_return <- 1 - WTA_onserve

library(ggplot2)
df <- tibble(Tour = c("ATP","ATP","WTA","WTA"),Serve = rep(c("On Serve","Return"),2), Probability = c(ATP_onserve,ATP_return,WTA_onserve,WTA_return))

p <- ggplot(df, aes(y = Probability, x = Tour, fill = Serve)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
  theme_minimal() + 
  labs(y = "Probability of Winning a Point on Serve") + 
  scale_fill_manual(values = c("olivedrab1","gray70")) + 
  ggtitle(label = "On-Serve & Returning Win Percentages (2011-2021)")
p

pp <- ggplotly(p)
api_create(pp, filename = "Tour On Serve Percentages")





################# Point prob v Game prob
#plot_ly(x = dataf$Pr_Point_on_Serve, y = dataf$Pr_Win_Service_Game)

options(scipen=10)
dataf <- tibble(Pr_Point_on_Serve = seq(0,1,0.001)) %>% 
  mutate(Pr_Win_Service_Game = round(Pr_Point_on_Serve^4 + 4*Pr_Point_on_Serve^4*(1-Pr_Point_on_Serve) + 
                                        10*Pr_Point_on_Serve^4*(1-Pr_Point_on_Serve)^2 + 
                                        ((20*Pr_Point_on_Serve^5*(1-Pr_Point_on_Serve)^3)/(1-2*Pr_Point_on_Serve+2*Pr_Point_on_Serve^2)),
                                      digits = 4))

pointsvgames = dataf %>% 
  ggplot(aes(x = Pr_Point_on_Serve, y = Pr_Win_Service_Game)) +
  geom_path(colour = "#00BFC4", size = 1) +
  labs(title = "Relationship Between Winning a Point on Serve & a Service Game",
       y = "Probability of Winning Service Game",
       x = "Probability of Winning a Point on Serve") +
  theme_minimal()

pointsvgames <- ggplotly(pointsvgames)
pointsvgames
api_create(pointsvgames, filename = "Points v Games Curve")








##### 3D Plot - Sets Serve First
x <- seq(0,1,0.01)
y <- seq(0,1,0.02)
data3d <- expand.grid(x,y) %>% mutate(z = set(Var1,Var2))

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "x Axis",
  titlefont = f
)
y <- list(
  title = "y Axis",
  titlefont = f
)

winningaset <- plot_ly(x = data3d$Var1, y = data3d$Var2, z = data3d$z, color = data3d$z) %>% 
  layout(
    title = "Probability of Winning a Set when Serving First",
scene = list(
  xaxis = list(title = "Pr(Win Point On-Serve)"),
  yaxis = list(title = "Pr(Win Point On Return)"),
  zaxis = list(title = "Pr(Win Set When Serving First)"),
  camera = list(eye = list(x = 1.25, y = -1.25, z = 1.25))
))
winningaset
api_create(winningaset, filename = "Winning a Set (First Serve)")

  


# r^6 * ( s^6* (504*t-252) + s^5 * (630-1512*t) + s^4* (1750*t-560) + s^3 * (210-980*t) + s^2 * (270*t-30) + s * (1-32*t) + t) -
#   r^5*s * ( 126*s^5 * (12*t-5) - 14*s^4*(290*t-91) + 70*s^3 * (58*t-11) - 30*s^2 * (62*t-3) + 5*s * (76*t+7) - 26*t - 5) +
#   5*r^4*s^2 * ( 14*s^4 * (25*t-8) -14*s^3 * (58*t-11) + 15*s^2 * (44*t-1) - 20*s * (11*t+2) + 5*(5*t+2) ) -
#   10*r^3*s^3 * (7*s^3 * (14*t-3) - 3*s^2 * (62*t-3) + 10*s * (11*t+2) - 10*(2*t+1) ) +
#   5*r^2*s^4 * (6*s^2 * (9*t-1) - s * (76*t+7) + 5*(5*t+2) ) - 
#   r*s^5 * (s * (32*t-1) - 26*t - 5 ) +
#   s^6*t


################# Match v Sets

options(scipen=10)
dataf2 <-
bind_rows(
  tibble(Pr_Win_Set = seq(0,1,0.001)) %>% 
  mutate(Pr_Win_Match = round(3*Pr_Win_Set^2 - 2*Pr_Win_Set^3, digits = 4)) %>% 
  mutate(Sets = "3 Sets"),
  
  tibble(Pr_Win_Set = seq(0,1,0.001)) %>% 
  mutate(Pr_Win_Match = round(10*Pr_Win_Set^3 - 15*Pr_Win_Set^4 + 6*Pr_Win_Set^5, digits = 4)) %>%
  mutate(Sets = "5 Sets")
)

setsvmatch = dataf2 %>% 
  ggplot(aes(x = Pr_Win_Set, colour = Sets)) +
  geom_line(aes(y = Pr_Win_Match), size = 1) +
  labs(title = "Relationship Between Winning a Set & the Match",
       y = "Probability of Winning Match",
       x = "Probability of Winning Set") +  theme_minimal() 

setsvmatch
ggplotly(setsvmatch)
api_create(setsvmatch, filename = "Winning the Match")









################# Functions

# Service Game
service_game <- function(p_s) {
  return(p_s^4 + 4*p_s^4*(1-p_s) + 10*p_s^4*(1-p_s)^2 + (20*p_s^5*(1-p_s)^3)/(1-2*p_s+2*p_s^2))
}

# Return Game
return_game <- function(p_r) {
  return(p_r^4 + 4*p_r^4*(1-p_r) + 10*p_r^4*(1-p_r)^2 + (20*p_r^5*(1-p_r)^3)/(1-2*p_r+2*p_r^2))
}

# Tie Breaker
tie_breaker <- function(p_s,p_r) {
  p_t = 0.5*p_s + 0.5*p_r
  return(p_t^7 + 7*p_t^7*(1-p_t) + 28*p_t^7*(1-p_t)^2 + 84*p_t^7*(1-p_t)^3 + 210*p_t^7*(1-p_t)^4 + 462*p_t^7*(1-p_t)^5 + (924*p_t^8*(1-p_t)^6)/(1-2*p_t+2*p_t^2))
}

# Set
set <- function(p_s,p_r) {
  s = service_game(p_s)
  r = return_game(p_r)
  t = tie_breaker(p_s,p_r)
  s_firstserve = 
    1* s^3 * r^3 + 3* s^3 * r^3 * (1-s)^1 + 3* s^4 * r^2 * (1-r)^1 + 12* s^3 * r^3 * (1-s)^1 * (1-r)^1 + 6* s^2 * r^4 * (1-s)^2 + 3* s^4 * r^2 * (1-r)^2 + 24* s^3 * r^3 * (1-s)^2 * (1-r)^1 + 24* s^4 * r^2 * (1-s)^1 * (1-r)^2 + 4* s^2 * r^4 * (1-s)^3 + 4* s^5 * r^1 * (1-r)^3 + 60* s^3 * r^3 * (1-s)^2 * (1-r)^2 + 40* s^2 * r^4 * (1-s)^3 * (1-r)^1 + 20* s^4 * r^2 * (1-s)^1 * (1-r)^3 + 5* s^1 * r^5 * (1-s)^4 + 1* s^5 * r^1 * (1-r)^4 + 100* s^3 * r^4 * (1-s)^3 * (1-r)^2 + 100* s^4 * r^3 * (1-s)^2 * (1-r)^3 + 25* s^2 * r^5 * (1-s)^4 * (1-r)^1 + 25* s^5 * r^2 * (1-s)^1 * (1-r)^4 + 1* s^1 * r^6 * (1-s)^5 + 1* s^6 * r^1 * (1-r)^5 + 200* s^3 * r^3 * (1-s)^3 * (1-r)^3 * t + 125* s^4 * r^2 * (1-s)^2 * (1-r)^4 * t + 125* s^2 * r^4 * (1-s)^4 * (1-r)^2 * t + 26* s^5 * r^1 * (1-s)^1 * (1-r)^5 * t + 26* s^1 * r^5 * (1-s)^5 * (1-r)^1 * t + 1* s^6 * (1-r)^6 * t + 1 * r^6 * (1-s)^6 * t
  s_firstreturn = 
    1* s^3 * r^3 + 3* s^3 * r^3 * (1-r)^1 + 3* s^2 * r^4 * (1-s)^1 + 12* s^3 * r^3 * (1-s)^1 * (1-r)^1 + 6* s^4 * r^2 * (1-r)^2 + 3* s^2 * r^4 * (1-s)^2 + 24* s^3 * r^3 * (1-s)^1 * (1-r)^2 + 24* s^2 * r^4 * (1-s)^2 * (1-r)^1 + 4* s^4 * r^2 * (1-r)^3 + 4* s^1 * r^5 * (1-s)^3 + 60* s^3 * r^3 * (1-s)^2 * (1-r)^2 + 40* s^4 * r^2 * (1-s)^1 * (1-r)^3 + 20* s^2 * r^4 * (1-s)^3 * (1-r)^1 + 5* s^5 * r^1 * (1-r)^4 + 1* s^1 * r^5 * (1-s)^4 + 100* s^4 * r^3 * (1-s)^2 * (1-r)^3 + 100* s^3 * r^4 * (1-s)^3 * (1-r)^2 + 25* s^5 * r^2 * (1-s)^1 * (1-r)^4 + 25* s^2 * r^5 * (1-s)^4 * (1-r)^1 + 1* s^6 * r^1 * (1-r)^5 + 1* s^1 * r^6 * (1-s)^5 + 200* s^3 * r^3 * (1-s)^3 * (1-r)^3 * t + 125* s^2 * r^4 * (1-s)^4 * (1-r)^2 * t + 125* s^4 * r^2 * (1-s)^2 * (1-r)^4 * t + 26* s^1 * r^5 * (1-s)^5 * (1-r)^1 * t + 26* s^5 * r^1 * (1-s)^1 * (1-r)^5 * t + 1 * r^6 * (1-s)^6 * t + 1* s^6 * (1-r)^6 * t
  return(0.5*s_firstserve + 0.5*s_firstreturn)
}

# Match
match <- function(p_s, p_r, best_of = 3) {
  x = set(p_s,p_r)
  if (best_of == 5) {
    return(10*x^3 - 15*x^4 + 6*x^5)    
  } else {
    return(3*x^2 - 2*x^3)
  }
}

# Test
match(best_of = 5, p_s = 0.63, p_r = 0.41)






##### 3D Plot - Sets Serve First
x <- seq(0,1,0.01)
y <- seq(0,1,0.02)
data3d2 <- expand.grid(x,y) %>% mutate(z = match(Var1,Var2, best_of = 5))

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "x Axis",
  titlefont = f
)
y <- list(
  title = "y Axis",
  titlefont = f
)

winningamatch <- plot_ly(x = data3d2$Var1, y = data3d2$Var2, z = data3d2$z, color = data3d2$z) %>% 
  layout(
    title = "Probability of Winning a 5-Set Tennis Match",
    scene = list(
      xaxis = list(title = "Pr(Win Point On-Serve)"),
      yaxis = list(title = "Pr(Win Point On Return)"),
      zaxis = list(title = "Pr(Win Match)"),
      camera = list(eye = list(x = 1.25, y = -1.25, z = 1.25))
    ))
winningamatch
api_create(winningamatch, filename = "Winning a 5-Set Tennis Match")


