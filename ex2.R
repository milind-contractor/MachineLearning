library(ggplot2)

# Build Logistic Regression Model.
# Read Data from File provide as part of Ex2

data.ExamScore = read.csv("ex2data1.txt", header = FALSE)
names(data.ExamScore) <- c("Ex1", "Ex2","Admission")

x = as.matrix(data.ExamScore[,1:2])


y = as.matrix(data.ExamScore[[3]])

p <- ggplot(data.ExamScore, aes(x = Ex1 , y= Ex2, color = as.factor(y) ))  + 
 geom_point( shape = 1, size = 2.5) 

assign("plot", p , .GlobalEnv)
rm(plot)
rm(p)

CostFunction <- function(X, y, Theta, Lambda){
  Hypothesis <- 1 / (1 + exp(- X %*% Theta)) # probability that y = 1 on input X
  J <- (1/m) * sum(- y * log(Hypothesis) - (1 - y) * log(1 - Hypothesis)) # compute J cost
  reg <- (Lambda / (2 * m)) * sum(Theta[2:length(Theta)] ^ 2)
  J <- J + reg
  return(J)
}

Theta =c(-30, 0.3, 0.3)
GradFunction <- function(X, y, Theta, Lambda){
  Hypothesis <- 1 / (1 + exp(- X %*% Theta))
  Grad <- ((t(X) %*% (Hypothesis - y)) / m) + ((Lambda / m) * c(0,Theta[2:length(Theta)]))
  print("is nanan.")
  #slope <- -(Theta[2] / Theta[3])
  #intercept <- (-Theta[1] / Theta[3])
  #if(!is.nan(slope) & !is.nan(intercept)){
    #p <- plot + geom_abline(slope= slope, intercept = intercept, colour= "red", size=1)
    #assign("plot", p , .GlobalEnv)
  #}
  return(Grad)
}
m <- nrow(x)
CostFunction(cbind(1,x),y,c(0, 0, 0),0)

CostFunction(cbind(1,x),y,c(-24, 0.2, 0.2),0)

GradFunction(cbind(1,x),y,c(-24, 0.2, 0.2),0)
plot

x <- cbind(1,x)
X= cbind(1, x)
Lambda=0
TrainLogisticReg <- function(X, y, Lambda){
  
  ifelse(is.vector(X), initial_theta <- c(0,0), initial_theta <- rep(0, ncol(X))) # initialize theta
  
  ifelse(is.vector(X), m <- length(X), m <- nrow(X))
  assign("m", m, .GlobalEnv)
  
  CostFunction(x,y,initial_theta,0)
  LogisticRec <- optim(par = initial_theta,
                       fn = CostFunction,
                       gr = GradFunction,
                       #method = "BFGS",
                       X = X,
                       y = y,
                       Lambda = Lambda,
                       control=list(trace=TRUE)
                       )
  
  print(paste("J", LogisticRec$value))
  return(LogisticRec$par)
}

theta_result <- TrainLogisticReg(x, y, 0)
theta_result
p <- p + geom_abline(slope= (-theta_result[2] / theta_result[3]), intercept = (-theta_result[1] / theta_result[3]), colour= "red", size=1)
plot
p
