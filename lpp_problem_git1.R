library(lpSolve)
library(ggplot2)

#Using R ggplot2 and lpsolve to plot feasible solution and find the solutions
#by Martina Bleta

f.obj <- c(300, 200) #objective function
f.con <- matrix(c(6, 4,
                  8, 4,
                  3, 3),
                nrow = 3, byrow = TRUE) #the matrix used in the problem from the book

f.dir <- c("<=", "<=", "<=") #constraints signs
f.rhs <- c(35, 40, 25) #availability 
solution <- lp("max", f.obj, f.con, f.dir, f.rhs)

results <- data.frame(
  x1 = solution$solution[1],
  x2 = solution$solution[2],
  Profit = solution$objval
)

print(results)

grid <- expand.grid(
  x1 = seq(0, 8, 0.05),
  x2 = seq(0, 8, 0.05)
)

grid$feasible <- with(grid,
                      6*x1 + 4*x2 <= 35 &
                        8*x1 + 4*x2 <= 40 &
                        3*x1 + 3*x2 <= 25
)

ggplot() +
  geom_point(data = subset(grid, feasible),
             aes(x1, x2),
             color = "lightpink", size = 1) +
  geom_point(data = results,
             aes(x1, x2),
             color = "blue", size = 4) +
  labs(
    title = "Graphical Solution after the change",
    x = "Grandfather Clocks (x1)",
    y = "Wall Clocks (x2)"
  ) +
  theme_minimal()
