###
#

#

library(ReBoost)
data(iris)
form<-Sepal.Length ~ .

m <- SmoteBoost(form, iris, modeltype = "RQ",
              model_pars = NULL)

m2<-SmoteBoost(form, iris, modeltype = "R2",
               model_pars = NULL)

m3<-SmoteBoost(form, iris, modeltype = "RT",
               model_pars = NULL)

m4<-SmoteBoost(form, iris, modeltype = "RTPlus",
               model_pars = NULL)

m5<-SmoteBoost(form, iris, modeltype = "BEM",
  model_pars = NULL)


m6<-AdaBoost(form, iris, modeltype = "RQ",
             model_pars = NULL)

m7<-AdaBoost(form, iris, modeltype = "R2",
             model_pars = NULL)

m8<-AdaBoost(form, iris, modeltype = "RT",
             model_pars = NULL)

m9<-AdaBoost(form, iris, modeltype = "RTPlus",
             model_pars = NULL)

m10<-AdaBoost(form, iris, modeltype = "BEM",
  model_pars = NULL)

predict(m, iris)
predict(m2, iris)
predict(m3, iris)
predict(m4, iris)
predict(m5, iris)

predict(m6, iris)
predict(m7, iris)
predict(m8, iris)
predict(m9, iris)
predict(m10, iris)


m10<-AdaBoost(form, iris, modeltype = "BEM",
              model_pars = NULL)

predict(m10, iris)


