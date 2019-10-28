# Mathematics and Statistics Coursework
# By Samuel John Malpass

# Question 1
# Write R code to reproduce a graph

question1exp<-expression(    (1/(2^(v/2)) * Gamma(v/2)) * (x^((v/2)-1)) * (exp(1)^-(x/2))	)
v<-seq(0,100,1)
curve(gamma(x,shape = v/2,rate = 0.1), from=0, to=0.5)

# Question 2

# Question 3
# Calculate the singular values and matrices in the singular value decomposition

A<-cbind(c(1,1,1,1),c(2,0,0,0)) # Declare matrix A
sv<-svd(A)
sv

# Question 4

# Question 5
# i) Write R code to solve a system of differential equations
library(deSolve)
LotVmod <- function (Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        dx = x * (a - (g * x) - (b * y))
        dy = y * (-c + (d * x))
        return(list(c(dx, dy)))
    })
}

# ii) Run code to obtain solution for given values
Pars <- c(a = 5, b = 0.01, c = 100, d = 0.01, g = 0.0001)
State <- c(x = 10000, y = 60)
Time <- seq(0, 5, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Fish", "Humans"), lty = c(1,2), col = c(1,2), box.lwd = 0)

# iii) Find the values of X and Y where the system achieves equilibrium
# One solution is x = 0, y = 0
# Other solution is x = (a-by)/g, y = (dxy)/c

# Question 6
y<-seq(-4.5,4.5,0.1)
x<-seq(-4.5,4.5,0.1)
question6<-function(x,y){
(x^2+y-11)^2+(x+y^2-7)^2
}
test<-function(z) {
(z[,1]^2+z[,2]-11)^2+(z[,1]+z[,2]^2-7)^2
}
hold<-function(z) {
z<-matrix(z,ncol=2)
f.z<-test(z)
return(f.z)
}
X<-as.matrix(expand.grid(x,y))
colnames(X)<-c("x","y")
testy<-hold(X)
df<-data.frame(X,y)
plot(question6(x,y))
out<-optim(c(-4.5,4.5),hold,method="Nelder-Mead")
out