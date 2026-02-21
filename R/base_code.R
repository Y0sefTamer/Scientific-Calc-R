add <- function(x, y) { return(x + y) }
subtract <- function(x, y) { return(x - y) }
multiply <- function(x, y) { return(x * y) }
divide <- function(x, y) { if (y==0)return("Math Error")
  return(x / y) }
m_sqrt <- function(x) {
  if (x < 0) return("Error: Negative Input")
  return(sqrt(x))
}

m_log <- function(x){if(x<=0)return("Error: log of 0 or Negative")
  return(log(x))}



# الدوال المثلثية (بتحول للراديان تلقائياً)
m_sin <- function(x){return(sin(x))}
m_cos <- function(x){return(cos(x))}
m_tan <- function(x){return(tan(x))}

power <- function(x, y) {
  return(x^y)
}

# حساب القيمة المطلقة
m_abs <- function(x) { return(abs(x)) }



# حساب المضروب (Factorial)
m_factorial <- function(x) {
  if (x < 0) return("Error: Negative Factorial")
  return(factorial(as.integer(x)))
}



#حل المعادلات التربيعية
Quadric_solver <- function(a,b,c){D <- b^2-4*a*c
#يوجد حلين حقيقيين
if(D>0){x1 <- (-b+sqrt(D))/(2*a)
x2 <- (-b-sqrt(D))/(2*a)
return(c(x1,x2))
}
#يوجد حل واحد
else if(D==0){
  x1 <- -b/(2*a)
  return(x1)}
#تخيلية  
else {
  x1 <- (-b+sqrt(as.complex(D)))/(2*a)
  x2 <- (-b-sqrt(as.complex(D)))/(2*a)
  return(c(x1,x2))
  
}       } 
                      #ggplot2
                    library(ggplot2)
                          draw_plot<-function(eq_string,x1=NULL,x2=NULL){
                          # تحويل النص لدالة رياضية
                                my_func<-function(x){
                                  eval(parse(text = eq_string))}
                                        
                                      p <- ggplot() +
                                        xlim(-10,10) +
                                          geom_function(fun=my_func,color="blue",linewidth =1) +
                                            theme_minimal()
                                      # لو فيه حلول (نقط)، ضيفها على الرسمة كلون أحمر
                          if(!is.na(x1)){
                            p <- p + geom_point(aes(x=x1,y=0),color="red",size=4)
                            
                          }
                                      if(!is.na(x1)){
                                        p <- p + geom_point(aes(x=x2,y=0),color="red",size=4)
                                        
                                      }
                                      return(p)
                                
                          } 
                          
                          
                          #Quadric_solver + ggplot2
                            
                              process_the_equation <- function(a,b,c) {
                                #تكوين النص بتاع المعادلة عشان الرسم
                                    eq_text <- paste0(a, "*x^2 + ", b, "*x + ", c)
                                        
                                      #Quadric_solver
                                    solution <- Quadric_solver(a,b,c)
                                          #ggplot2
                                         plot_f <- draw_plot(eq_text,solution[1],solution[2])
                                         return(plot_f)
                                
                                 }