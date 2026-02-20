add <- function(x, y) { return(x + y) }
subtract <- function(x, y) { return(x - y) }
multiply <- function(x, y) { return(x * y) }
divide <- function(x, y) { if (y==0){return("Math Error")}return(x / y) }
    sqrt <- function(x) {
      if (x < 0) return("Error: Negative Input")
      return(sqrt(x))
    }

        log <- function(x){if(x<=0){return("Error: log of 0 or Negative")}return(log(x))}



            # الدوال المثلثية (بتحول للراديان تلقائياً)
            sin <- function(x){return(sin(x))}
            cos <- function(x){return(cos(x))}
            tan <- function(x){return(tan(x))}



# حساب القيمة المطلقة
abs <- function(x) { return(abs(x)) }



                  # حساب المضروب (Factorial)
                  factorial <- function(x) {
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
