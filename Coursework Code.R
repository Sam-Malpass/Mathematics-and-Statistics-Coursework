# Mathematics and Statistics Coursework
# By Samuel John Malpass

# Question 1
# Write R code to reproduce a graph

question1exp<-expression(    (1/(2^(v/2)) * Gamma(v/2)) * (x^((v/2)-1)) * (exp(1)^-(x/2))	)
v<-seq(0,100,1)
curve(gamma(x,shape = v/2,rate = 0.1), from=0, to=0.5)

# Question 2
# i) Calculate the average of the three faces and plot
library(bmp)
face1<- read.bmp('C:/Users/Sam/Documents/R Scripts/Mathematics-and-Statistics-Coursework/face1.bmp')
face2<- read.bmp('C:/Users/Sam/Documents/R Scripts/Mathematics-and-Statistics-Coursework/face2.bmp')
face3<- read.bmp('C:/Users/Sam/Documents/R Scripts/Mathematics-and-Statistics-Coursework/face3.bmp')
X <- list(face1, face2, face3)
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
av <- apply(Y, c(1,2), mean, na.rm=TRUE)
image(t(apply(av,2,rev)),col = gray((0:32)/32),axes=F)

# ii) Plot images of the differences in faces
par(mfrow=c(1,3))
diff1 <- av - face1
image(t(apply(diff1,2,rev)),col = gray((0:32)/32),axes=F)
diff2 <- av - face2
image(t(apply(diff2,2,rev)),col = gray((0:32)/32),axes=F)
diff3 <- av - face3
image(t(apply(diff3,2,rev)),col = gray((0:32)/32),axes=F)

# iii) Based on the covariance matrix of differences calulate the eigenfaces

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