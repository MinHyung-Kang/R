hw8q1 <-
function(job=1){
   dump("hw8q1","C:\\RCode(Math70)\\Homework 8\\hw8q1.r")
   data = read.table("C:\\RCode(Math70)\\Homework 8\\movieFinal.txt",
	header=TRUE)
   n = nrow(data)

   #Simple logistic regression
   result = glm(data$y ~ data$Age + data$ed, family=binomial) #age, binomial
   coefAge = coef(result)["data$Age"]
   coefEd = coef(result)["data$ed"]
   intercept = coef(result)["(Intercept)"]

   #Run logistic regression model of y with respect to education status
   if(job == 2){
	#Simple logistic regression
	result = glm(data$y ~ data$Age + data$ed, family=binomial) 
	print(summary(result))

	#Using dummy variables
	ordereddata = data[order(data$ed),]
	n0=length(data$ed[data$ed==0])
	n1=length(data$ed[data$ed==1])
	n2=length(data$ed[data$ed==2])
	n3=length(data$ed[data$ed==3])

	#Using dummy variables
	delta = matrix(c(rep(0,n0),rep(1,n1),rep(2,n2),rep(3,n3)),ncol=1)
	noHS = rep(1,n)	

	result2 = glm(ordereddata$y ~ ordereddata$Age + noHS + delta -1,family=binomial)
	print(summary(result2))

	#Compute the likelihood test
	print(paste("Log Likelihood of Continuous model : ",round(logLik(result),4)))
	print(paste("Log Likelihood of Dummy model",round(logLik(result2),4)))
	print(paste("Chisq : ",round(pchisq(-2*(logLik(result2)-logLik(result)),df=1),4)))
   }
   
   #Plot y versus age for viewers with high school degree
   if(job == 3){
	#Select y values with high school degree
	yindex = data$y[data$ed==1]
	ageindex = data$Age[data$ed==1]
	edindex = data$ed[data$ed==1]
	plot(ageindex, yindex, xlab = "Age", ylab = "y",col="BLUE",
	    main="y versus age for viewers with high school degree")

	#construct the model
	modelVal = intercept + coefAge * ageindex + coefEd * edindex
	modelVal = exp(modelVal) / (1 + exp(modelVal))
	lines(ageindex, modelVal,col="RED")

	legend(50,0.8,c("Data Points","Fitted Model values"),
	   lwd=c(1,1),col=c("BLUE","RED"))
   }

   #Compute the proportion of people who liked the movie
   if(job == 4){
	count = length(data$y[data$y==1])
	print(count/n)

	#Compute the averages
	ageavg = mean(data$Age)
	edavg = mean(data$ed)

	#Estimate the proportion
	modelVal = intercept + coefAge * ageavg + coefEd * edavg
	modelVal = exp(modelVal) / (1 + exp(modelVal))
	print(modelVal)
   }

   #Compute the probability that a 32 year old person with college degree
   #likes the movie
   if(job == 5){

	#construct the model
	modelVal = intercept + coefAge * 32 + coefEd * 2
	modelVal = exp(modelVal) / (1 + exp(modelVal))
	print(modelVal)

	#Compute D Matrix
	Beta = rbind(intercept,coefAge, coefEd);
	D = matrix(0,nrow=n,ncol=n)
	for(i in 1:n){
	   xi = matrix(rbind(1,data$Age[i],data$ed[i]),ncol=1)
	   D[i,i] = exp(t(Beta)%*%xi)/(1+exp(t(Beta)%*%xi))^2
	}
	
	#Compute C Matrix
	X = cbind(rep(1,n),data$Age,data$ed)
	C= solve(t(X) %*% D %*% X)
	
	#Compute CI 
	x = rbind(1,32,2)
	teststat = 1.96 * sqrt(t(x)%*%C%*%x)
	logit = intercept + coefAge * 32 + coefEd * 2
	leftLogit = logit - teststat
	rightLogit = logit + teststat
	print(paste("95% CI on the logit scale : (",
	   round(leftLogit,4),",",round(rightLogit,4),")"))
	leftProb = exp(leftLogit)/(1+exp(leftLogit))
	rightProb = exp(rightLogit)/(1+exp(rightLogit))
	print(paste("95% CI on the probability scale : (",
	   round(leftProb,4),",",round(rightProb,4),")"))
   }
  
   if(job==6){
	#Using dummy variables
	ordereddata = data[order(data$ed),]
	n0=length(data$ed[data$ed==0])
	n1=length(data$ed[data$ed==1])
	n2=length(data$ed[data$ed==2])
	n3=length(data$ed[data$ed==3])

	ed1 = matrix(c(rep(1,n0),rep(0,n-n0)),ncol=1)
	ed2 = matrix(c(rep(0,n0),rep(1,n1),rep(0,n2+n3)),ncol=1)
	ed3 = matrix(c(rep(0,n-n3),rep(1,n3)),ncol=1)
	
	result2 = glm(ordereddata$y ~ ordereddata$Age + ed1 + ed2 + ed3,family=binomial)
	print(summary(result2))
   }
}
