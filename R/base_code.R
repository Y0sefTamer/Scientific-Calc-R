# الدوال الحسابية البسيطة
add <- function(x, y) { return(x + y) }

subtract <- function(x, y) { return(x - y) }

multiply <- function(x, y) { return(x * y) }

divide <- function(x, y) { 
  if (y == 0) return("Math Error")
  return(x / y) 
}

Remainder <- function(a, b) {
  if (b == 0) {
    return("Error: Division by zero is not allowed!")
  }
  return(a %% b)
}

# الدوال العلميه
Sqrt <- function(x) {
  if (x < 0) return("Error: Negative Input")
  return(base::sqrt(x))
}

Log <- function(x) {
  if (x <= 0) return("Error: log of 0 or Negative")
  return(base::log10(x))
}

Ln <- function(x) {
  if (x <= 0) return("Error: ln of 0 or Negative")
  return(base::log(x))
}

Square <- function(x) { return(x^2) }

# الدوال المثلثيه
Sin <- function(x) { return(base::sin(x * pi / 180)) }
Cos <- function(x) { return(base::cos(x * pi / 180)) }
Tan <- function(x) { return(base::tan(x * pi / 180)) }

# abs
Abs <- function(x) { return(base::abs(x)) }

#factorial

Fact <- function(x) {
  if (x < 0) return("Error: Negative Factorial")
  return(base::factorial(as.integer(x)))
}