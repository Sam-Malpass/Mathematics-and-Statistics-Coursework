# Mathematics and Statistics Coursework
# By Samuel John Malpass

# Question 1
# Write R code to reproduce a graph
v<-5	#Decide on arbitrary value for v
alpha<-v/2 #create alpha
beta<-2 #create beta
avrg<- alpha * beta #create average
std.dv<-sqrt(alpha*beta^2)	#create standard deviation
range<-seq(0, avrg + 5 * std.dv, 0.01) #create range
y=dchisq(range, df=v)#Define the chi squared function
#plot the chi function
plot(range,y,type='l', ylim=c(0,max(y)+0.01),bty="l",xaxt="n",yaxt="n",ylab="", xlab="x",xaxs="i", yaxs="i", lwd=1.2) 
axis(1, at=0, "0") # add zero to x axis
axis(1, at=7.5, "a") # add a to x axis
axis(1,at=12, "b") # add b to x axis
axis(2, at=0, " ") # add a little axis bit
title(main=expression(paste("X~",chi[nu]^{"2"})), cex.main=1.5) # add title
text(12.2,0.125,cex=1.2, label=expression(paste("f(x)=",frac(1,2^frac(nu,2)*Gamma*bgroup("(", frac(nu,2), ")"))*paste("x"^paste(frac(nu,2)-1),"e"^-frac("x",2), " , x>0"))))#add equation
arrows(12,0.037, 9.7, 0.032, length=0.1, col="red", lwd=2) #add arrow
text(14.5,0.038,cex=1.2, label=expression(paste("P(a<",Chi,"<b)=",integral(f(x)*dx,a,b)))) #add integral part

cord.a=c(7.5,seq(min(7.5), 12, 0.01),12) #set x bounds
cord.b=c(0, dchisq(seq(min(7.5), 12, 0.01), v), 0) #set y bounds
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

# Find average minus face instead
par(mfrow=c(1,3))
diff1 <- av - face1
image(t(apply(diff1,2,rev)),col = gray((0:32)/32),axes=F)
diff2 <- av - face2
image(t(apply(diff2,2,rev)),col = gray((0:32)/32),axes=F)
diff3 <- av - face3
image(t(apply(diff3,2,rev)),col = gray((0:32)/32),axes=F)

# iii) Based on the covariance matrix of differences calulate the eigenfaces
#convert the matrices to vectors
diffvec1<-as.vector(diff1)
diffvec2<-as.vector(diff2)
diffvec3<-as.vector(diff3)
#concatenate all the vectors
diffall<-rbind(diffvec1,diffvec2,diffvec3)

#Calculate covariance matrix and eigenvectors
covmat<-cov(diffall)
eigs<-eigen(covmat)$vectors

#Retrieve and Plot first the three eigenfaces
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
# Set up differential System
library(deSolve)
Ovini<-c(y=0, v=1, b=(-(5/2)))
derivs.Test<-function(t, Ov, parms)
{
	with(as.list(c(Ov, parms)),
	{
		dy<-v
		dy2<-b
		dy3<-exp(-t)-dy2+dy+y
		list(c(dy3, dy2, dy))
	})
}
# Set the time interval
times<-seq(0,5,0.01)
# calculate and plot the solution
out.Test<-ode(y=Ovini, times=times, func=derivs.Test, parms=NULL)
plot(out.Test[,"time"], out.Test[,"y"], type="l", xlab="time", ylab="O", col="green", lwd=2)

# Code using the calculated numbers
#tmp<-function(t) ((0)*exp(-t))+((1)*exp(-t)*t)+((0)*exp(t))-((1/4)*exp(-t)*(t^2))
#curve(tmp, 0, 5, add=T, col="black", lty=2)

# Code using approximated numbers
tmp<-function(t) ((-2.25)*exp(-t))+((0)*exp(-t)*t)+((2.25)*exp(t))-((1/4)*exp(-t)*(t^2))
curve(tmp, 0, 5, add=T, col="black", lty=2)

# Question 5
# i) Write R code to solve a system of differential equations
library(deSolve)
# Set up the system
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
# Find the rough time at which equilibrium is achieved
Pars <- c(a = 5, b = 0.01, c = 100, d = 0.01, g = 0.0001)
State <- c(x = 10000, y = 60)
Time <- seq(0, 13, by = 0.1)
out <-ode(func = eqsystem, y = State, parms = Pars, times = Time)
matplot(out[,1], (out[,2:3]), type = "l", xlab = "time", ylab = "population")
legend("topright", c("Fish", "Humans"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# Looking at graph we can see at time=12 system achieves equilibrium 
# Find x
Pars <- c(a = 5, b = 0.01, c = 100, d = 0.01, g = 0.0001)
State <- c(x = 10000, y = 60)
Time <- seq(0, 13, by = 0.1)
out <-ode(func = eqsystem, y = State, parms = Pars, times = Time)
matplot(out[,1], (out[,2:2]), type = "l", xlab = "time", ylab = "population")
abline(h=10000, col="red")
# Find y
Pars <- c(a = 5, b = 0.01, c = 100, d = 0.01, g = 0.0001)
State <- c(x = 10000, y = 60)
Time <- seq(0, 13, by = 0.1)
out <-ode(func = eqsystem, y = State, parms = Pars, times = Time)
matplot(out[,1], (out[,3:3]), type = "l", xlab = "time", ylab = "population")
abline(h=400, col="red")
# So x = 10,000, y = 400 

# Question 6
# Create a function for the equation both for values and optim
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
# Create experiment functions
experiment<-function(n) 
{
	iterations<-seq(1,100000,1) # Define iterations
	actors<-seq(1,n,1) # Set up the actors
	count<-0 # Set the count to zero
	for(val in iterations) # for all iterations
	{
		babies<-sample(actors) # randomly sample actors for the babies (Two vectors of the same number, but different order)
		for(i in 1:length(actors)) # for all the actors
		{
			if(actors[i] == babies[i]) # if the actor matches the baby at the same index
			{
				count<-count+1 # increment the count
				break
			}
		}
	}
	return(count / length(iterations)) # calculate the average number of matches
}

testset<-seq(2,15,1) # create the test set vector
results<-c() # create the empty vector
for(val in testset) # for all the test vals
{
	results<-c(results, experiment(val)) # run the experiment
}

plot(testset, results, type="l") # plot the results

# Question 8
# Generate m datasets with size nn from a binomial distributions
generator<-function(nn, m)
{
	p<-0.01 # set the p value
	trialsize<-10 # set the trial size
	bd<-matrix(rbinom(m*nn, trialsize, p), nrow=nn, ncol=m) # create the distribution
	gen<-apply(bd, 2, mean)
	return(gen)
}

# get 10000 means from two groups
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

# Question 9
# Create the normal distribution
question9x<-rnorm(20,100,4)
noise<-rnorm(20,0,1) # create the noise distribution
results<-c() # declare results array
for(i in 1:length(question9x)) # for all data points
{
	tmp<-(2+question9x[i]+noise[i]) # create a value
	results<-c(results, tmp) # add to the new vector
}

plot(question9x,results) # plot the results
meanx<-mean(question9x) # get the mean of x
meany<-mean(results) # get the mean of the other sample
abline(h=meany, v=meanx) # draw the lines for the samples

unpaired<-t.test(question9x, results, paired=FALSE) # do an unpaired test
unpaired
paired<-t.test(question9x, results, paired=TRUE) # do a paired test
paired

# Question 10
# read the data
data<-read.table("C:/Users/Sam/Documents/R Scripts/Mathematics-and-Statistics-Coursework/cheese.txt", header=TRUE, sep="\t")
colnames(data)<-abbreviate(colnames(data)) # Abbreviate Column names
head(data)

# Part i
processed<-data[-1] # Remove name data
data.pca<-prcomp(processed,scale=TRUE) # Peform PCA
data.pca
plot(data.pca, xlim=rev(c(1,12))) # Scree plot lowest to highest

# Part ii
biplot(data.pca) # Biplot how nutrients affect PCA

# Part iii
summary(data.pca) # Get the summary
# 10 components

# Part iv
component<-data.pca$rotation[,1] # Get First Principal Component
component
# Cheddar is index 6, Edam is index 15
calcScore<-function(A, B) # Calc score function
{
	total<-0 # total is 0
	for(i in 1:length(A)) # for all the elements in the vector
	{
		total<-total + (A[i] * B[i]) # times each element by the same element in the principle component score vector
	}
	return(total) # return the score
}
cheddar_row<-data[6,-1]
edam_row<-data[15, -1]
cheddar_score<-calcScore(component, cheddar_row)
edam_score<-calcScore(component, edam_row)
cheddar_score
edam_score
