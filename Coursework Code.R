# Mathematics and Statistics Coursework
# By Samuel John Malpass

# Question 1
# Write R code to reproduce a graph
v<-4	#Decide on arbitrary value for v
alpha<-v/2 #create alpha
beta<-2 #create beta
avrg<- alpha * beta #create average
std.dv<-sqrt(alpha*beta^2)	#create standard deviation
range<-seq(0, avrg + 5 * std.dv, 0.01) #create range
y=dgamma(range, alpha, rate=1/beta) #create the gamma function
plot(range,y,type='l', ylim=c(0,max(y)+0.01),bty="l",xaxt="n",yaxt="n",ylab="", xlab="x",xaxs="i", yaxs="i") #plot the gamma function
axis(1, at=0, "0") # add zero to x axis
axis(1, at=6, "a") # add a to x axis
axis(1,at=10, "b") # add b to x axis
title(main=expression(paste("X~",{Chi[v]}^{"2"})), cex.main=1.5) # add title
text(10.5,0.125,cex=1.2, label=expression(paste("f(x)=",frac(1,2^frac("v",2)*Gamma*bgroup("(", frac("v",2), ")"))*paste("x"^paste(frac("v",2)-1),"e"^-frac("x",2), " , x>0")))) #add equation
arrows(9.9,0.045, 8, 0.037, length=0.1, col="red", lwd=2) #add arrow
text(12.75,0.045,cex=1.2, label=expression(paste("P(a<",Chi,"<b)=",integral(f(x)*dx,a,b)))) #add integral part

cord.a=c(6,seq(min(6), 10, 0.01),10) #set x bounds
cord.b=c(0, dgamma(seq(min(6), 10, 0.01),alpha,rate = 1/beta),0) #set y bounds
polygon(cord.a,cord.b,col="blue") #plot the polygons in the bounds

# Question 2
# i) Calculate the average of the three faces and plot
library(bmp)
#read images in
face1<- read.bmp('C:/Users/Sam/Documents/R Scripts/Mathematics-and-Statistics-Coursework/face1.bmp')
face2<- read.bmp('C:/Users/Sam/Documents/R Scripts/Mathematics-and-Statistics-Coursework/face2.bmp')
face3<- read.bmp('C:/Users/Sam/Documents/R Scripts/Mathematics-and-Statistics-Coursework/face3.bmp')
#add them to a list and get the average
X <- list(face1, face2, face3)
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
av <- apply(Y, c(1,2), mean, na.rm=TRUE)
#rotate and plot the average
image(t(apply(av,2,rev)),col = gray((0:32)/32),axes=F)

# ii) Plot images of the differences in faces
#get the differences and plot them all
par(mfrow=c(1,3))
diff1 <- face1 - av
image(t(apply(diff1,2,rev)),col = gray((0:32)/32),axes=F)
diff2 <- face2 - av
image(t(apply(diff2,2,rev)),col = gray((0:32)/32),axes=F)
diff3 <- face3 - av
image(t(apply(diff3,2,rev)),col = gray((0:32)/32),axes=F)

# iii) Based on the covariance matrix of differences calulate the eigenfaces
#convert the matrices to vectors
diffvec1<-as.vector(diff1)
diffvec2<-as.vector(diff2)
diffvec3<-as.vector(diff3)
#concatenate all the vectors
diffall<-rbind(diffvec1,diffvec2,diffvec3)

covmat<-cov(diffall)
covmat
eigs<-eigen(covmat)$vectors
eigs

eigface1<-matrix(eigs[,1], nrow=51, byrow=TRUE)
eigface2<-matrix(eigs[,2], nrow=51, byrow=TRUE)
eigface3<-matrix(eigs[,3], nrow=51, byrow=TRUE)
par(mfrow=c(1,3))
image(t(apply(t(eigface1),2,rev)), col=gray((0:32)/32), axes=F)
image(t(apply(t(eigface2),2,rev)), col=gray((0:32)/32), axes=F)
image(t(apply(t(eigface3),2,rev)), col=gray((0:32)/32), axes=F)

# Question 3
# Calculate the singular values and matrices in the singular value decomposition
A<-cbind(c(1,1,1,1),c(2,0,0,0)) # Declare matrix A
sv<-svd(A)
sv

# Question 4






# Question 5
# i) Write R code to solve a system of differential equations
library(deSolve)
eqsystem <- function (Time, State, Pars) {
	x <- State[1]
	y <- State[2]
    with(as.list(c(Pars)), {
        dx = x * (a - g*x - b*y)
        dy = y * (-c + d*x)
        return(list(c(dx, dy)))
    })
}
# ii) Run code to obtain solution for given values
Pars <- c(a = 5, b = 0.01, c = 100, d = 0.01, g = 0.0001)
State <- c(x = 10000, y = 60)
Time <- seq(0, 5, by = 0.1)
out <-ode(func = eqsystem, y = State, parms = Pars, times = Time)
matplot(out[,1], (out[,2:3]), type = "l", xlab = "time", ylab = "population")
legend("topright", c("Fish", "Humans"), lty = c(1,2), col = c(1,2), box.lwd = 0)

# iii) Find the values of X and Y where the system achieves equilibrium
# One solution is x = 0, y = 0
# Other solution is x = (a-by)/g, y = (dxy)/c






# Question 6
question6<-function(vec) (vec[1]^2 + vec[2]-11)^2 + (vec[1] + vec[2]^2 - 7)^2
func<-function(x1,y1) (x1^2 + y1 - 11)^2 + (x1 + y1^2 - 7)^2
question6x<- seq(-4.5, 4.5, length.out=50)
question6y<-question6x
question6z<-outer(question6x,question6y,func)
persp(question6x,question6y,question6z,phi=45,theta=45,col="yellow",shade=.65 ,ticktype="detailed", xlab="x", ylab="y", zlab="f(x,y)")
# Find all minimums
min1<-optim(c(-4,-4), question6)$par
min2<-optim(c(2,-2), question6)$par
min3<-optim(c(2,2), question6)$par
min4<-optim(c(-4,4), question6)$par
# Find all maximums
max1<-optim(c(0,0), question6, control=list(fnscale=-1))$par

# Question 7
experiment<-function(n) 
{
iterations<-seq(1,100000,1)
actors<-seq(1,n,1)
count<-0
for(val in iterations)
{
babies<-sample(actors)
for(i in 1:length(actors))
{
if(actors[i] == babies[i]) 
{
count<-count+1
break
}
}
}
return(count / length(iterations))
}

testset<-seq(2,15,1)
results<-c()
for(val in testset)
{
results<-c(results, experiment(val))
}

plot(testset, results, type="l")

# Question 8
# Generate m datasets with size nn from a binomial distributions
generator<-function(nn, m)
{
p<-0.01
trialsize<-10
bd<-matrix(rbinom(m*nn, trialsize, p), nrow=nn, ncol=m)
gen<-apply(bd, 2, mean)
return(gen)
}

# get 10000 means fro two groups
sample_means<-10000 # Declare number of means
groups<-c(20,100) # declare group sizes
means<-lapply(groups,generator,sample_means) # generate the means
par(mfrow=c(1,length(means))) # set up the plots to show all samples
# Plot all
for(val in seq(1, length(groups),1)) 
{
	hist(means[[val]], main=paste("Sample Size", groups[val]), xlab="means")
}
# Calculate percentage of unique means in samples, this shows us that how the samples
# approach a normal distribution with increased size
for(val in seq(1,length(groups),1))
{
out<-(length(unique(means[[val]])) / groups[val] * 100)
print(out)
}
