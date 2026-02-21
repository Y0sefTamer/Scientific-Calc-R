add <- function(x, y) { return(x + y) }
subtract <- function(x, y) { return(x - y) }
multiply <- function(x, y) { return(x * y) }
divide <- function(x, y) { if (y==0)return("Math Error")
  return(x / y) }
            m_sqrt <- function(x) {
          if (x < 0) return("Error: Negative Input")
          return(sqrt(x))
        }
                  #ln
                m_ln <- function(x){if(x<=0)return("Error: log of 0 or Negative")
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



                                          # حساب المضروب 
                                          m_factorial <- function(x) {
                                            if (x < 0) return("Error: Negative Factorial")
                                            return(factorial(as.integer(x)))
                                          }

m_remainder <- function(a, b) {
  if (b == 0) {
    return("Error: Division by zero is not allowed!")
  }
  return(a %% b)
}

            m_log <- function(x, base = exp(1)) {
              if (x <= 0) {
                return("Error: Logarithm undefined for non-positive numbers!")
              }
              return(log(x, base = base))
            }

