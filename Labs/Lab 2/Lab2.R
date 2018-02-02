#Prem Shah
#Lab2_INFX 573

#Set the working directory
setwd("C:/Users/iGuest/Desktop/University of Washington/Quarter 2/INFX 573/Labs/Lab 2")

#Read the data
ratings <- read.csv("ratings.csv", stringsAsFactors=FALSE)
movies <- read.csv("movie.titles.csv", stringsAsFactors=FALSE)

dim(ratings)
#There are 100004 ratings in the dataset with 671 unique users (raters)

#The mean of ratings is 3.544, median is 4.000, standard deviation is 1.058064
summary(ratings)
sd(ratings$rating)

#We can observe that people seem to prefer round numbers
ggplot(data, aes(x = ratings$rating)) + geom_histogram()

#Merging the datasets
temp<-merge(ratings, movies, by="movieId")

#Plot relationships between rating and year
ggplot(data, aes(x = ratings$rating, y = ratings$year)) + geom_boxplot()
ggplot(data, aes(x = ratings$rating, y = ratings$year)) + geom_point()

#Extract the ones that are comedy movies
ratings$comedy <-rep(F, nrow=ratings)
ratings$comedy[grep("comedy",ratings$genre, ignore.case = T)] <-T

#Yes the ratings do vary by genre
ggplot(data, aes(x = ratings$comedy, y = ratings$rating)) + geom_boxplot()

#T-test
t.test(ratings$rating[ratings$comedy], ratings$ratings[!ratings$comedy])

#_________________________________________________________________________________________

#PART 2:
hist(rexp(1000))
hist(runif(1000))
hist(rpois(1000,0.5))

#In the below plot, we can see that it is a normal distribution for all the distributions, exponential, 
#We can see a normal distribution in all
N <-1000  
n.samp <-30 
M <-matrix(NA, nrow=N, ncol=n.samp) 
for(j in 1:n.samp) M[,j] <-rexp(N) 
hist(rowSums(M), freq = F)

curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)), max(rowSums(M)), add=T, col="red", lwd=2)

N <-1000  
n.samp <-30 
M <-matrix(NA, nrow=N, ncol=n.samp) 
for(j in 1:n.samp) M[,j] <-runif(N) 
hist(rowSums(M), freq = F)

curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)), max(rowSums(M)), add=T, col="red", lwd=2)

N <-1000
n.samp <-30 
M <-matrix(NA, nrow=N, ncol=n.samp) 
for(j in 1:n.samp) M[,j] <-rpois(N,0.5) 
hist(rowSums(M), freq = F)

curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)), max(rowSums(M)), add=T, col="red", lwd=2)
#When we increase the number of samples, it skews to the left and vice versa
#When we decrease the number of sums, the graph becomes coarser and if we increase the number of sums, the graph becomes finer. 