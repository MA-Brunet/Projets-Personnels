### Projet réalisé par Marc-Antoine Brunet
### Janvier 2025

# Contexte : 
#   En préparation à une soirée d'activité inter facultés dont le thème était
#   le casino. Des élèves de deuxième année ont décidé de programmer une
#   simulation boursière sur Python où on pouvait voir la valeur des actions
#   changer avec le temps. Je me suis donné le défi de recréer cette simulation
#   sur R afin d'approfondir mes connaissances du language.


### Simulation boursière

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)




## Compagnies (peut ajouter ou enlever autant que l'on veut)
c <- c("Microsoft", "Apple", "Tesla", "ACT", "Meta", "Amazon")

## Nombre d'expériences ou de "step"
n <- 100

## Valeur initiale des actions
v <- 50


## Formule de simulation et d'animation

simulate_and_animate <- function(n, c, v, sleep_time = 0.0) {
  
  # Generate Simulation Data
  simulate <- function(n, c, v) {
    company_data <- sapply(seq_along(c), function(i) {c(v, cumsum(rnorm(n, 0, 1)) + v)})
    data <- cbind(step = 0:n, company_data)
    colnames(data) <- c("step", c)
    return(as.data.frame(data))
  }
  
  data <- simulate(n, c, v)
  
  # Convert to long format for ggplot2
  long_data <- data %>%
    pivot_longer(cols = -step, names_to = "company", values_to = "value")
  
  # Animation Loop
  for (current_step in 0:max(long_data$step)) {
    
    # Filter data up to the current step
    current_data <- long_data %>% filter(step <= current_step)
    
    # Initialize plot
    p <- ggplot(current_data, aes(x = step, y = value, color = company)) +
      scale_x_continuous(limits = c(0, n), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
      labs(title = "Company Performance Over Time",
           subtitle = paste("Step:", current_step),
           x = "Temps",
           y = "Valeur",
           color = "Compagnie") +
      theme_minimal()  # Simpler theme for faster rendering
    
    # If it's the first step, it's a point
    if (current_step == 0) {
      p <- p + geom_point(size = 3)
    } else {
      p <- p + geom_path(linewidth = 1) # Efficient line drawing
    }
    
    # Display the plot
    print(p)
    flush.console()
    # Adjust speed (can improve performance)
    Sys.sleep(sleep_time)
  }
}

windows()
simulate_and_animate(n, c, v)
