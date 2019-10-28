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