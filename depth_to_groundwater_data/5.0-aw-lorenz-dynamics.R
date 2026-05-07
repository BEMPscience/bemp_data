alameda_sa <- y_t_y_1_t_data %>% filter(`Site name`== "Alameda") %>% 
  select(date, `Percent change`, `Mean depth to groundwater (cm)`) %>% 
  drop_na()

alameda_sa

lor = lorenz(do.plot = F, time=seq(0,50,by=0.1))
lor

scatter3D(lor$x, lor$y, lor$z,
          main = "Lorenz's system phase space t=500",
          col = 1, type="o",cex = 0.3)


# For only one measurement (the x-component) of the Lorenz system
# lor.x <- lor$x
lor.x <- alameda_sa$`Mean depth to groundwater (cm)`

# Can apply Taken’s embedding theorem for a measurment of part of the 
# space. 

old.par = par(mfrow = c(1, 2))
# tau-delay estimation based on the autocorrelation function
tau.acf = timeLag(lor.x, technique = "acf",
                  lag.max = 100, do.plot = T)

# tau-delay estimation based on the mutual information function
tau.ami = timeLag(lor.x, technique = "ami", 
                  lag.max = 100, do.plot = T)
par(old.par)

# Estimates the emedding dimensions
emb.dim = estimateEmbeddingDim(lor.x, time.lag = tau.ami,
                               max.embedding.dim = 15)

# 
tak = buildTakens(lor.x,embedding.dim = emb.dim, time.lag = tau.ami)
scatter3D(tak[,1], tak[,2], tak[,3],
          main = "Lorenz's system reconstructed phase space",
          col = 1, type="o",cex = 0.3)
