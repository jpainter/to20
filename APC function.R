

apc <- function (group=1, test="QFT", start=start) {
  

  data<- subset(to20, as.numeric(TB)==group )
  test.var<-data[,test]
  glm.mod<- glm( test.var~as.numeric(age.5yr), family=quasibinomial(link="log"), 
                     data=data,                  
                     ## starting values set to values modeled for QFT, normal-CXR (TB=1)
                     start=c(-1.6, 0.1)
                     )
  # summary 
  summary(glm.mod)
  
  # Calculate annual percent change
  apc.frame<- data.frame( group= group, test=test , 
                            apc= (exp(coef(glm.mod)[2]/5)-1 ) *100  ,
                            apc.ll= (exp(confint(glm.mod)[2,1]/5)-1 ) *100 ,
                            apc.ul= (exp(confint(glm.mod)[2,2]/5)-1 ) *100 ,
                            p= summary(glm.mod)$coefficients[2,4]
                            )
  
  # Fitted values
  #   Mean value when x=mean(age)
  newdata<- data.frame( age.5yr = mean(as.numeric(data$age.5yr)) )
  apc.frame$mean.age<- 10+(5*newdata[1,])
  glm.pred<- predict.glm(glm.mod, 
                               newdata=newdata[1], se=TRUE, type='response')
  apc.frame$mean<- glm.pred[1]
#   

  return(apc.frame)
}

# x<-apc()
# x
