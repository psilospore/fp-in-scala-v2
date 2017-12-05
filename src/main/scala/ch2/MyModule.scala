package ch2
import Math._

object MyModule {

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }

  // Exercise 2.1

  // Non tail-rec solution
  def fib2(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  // tail-rec solution
  def fib(n: Int): Int = {
    if (n == 1) return 0
    def fibHelper(n: Int, prevResults: (Int, Int)): Int = {
      if (n == 1)
        prevResults._2
      else {
        val currSum = prevResults._1 + prevResults._2
        fibHelper(n - 1, (prevResults._2, currSum))
      }
    }
    fibHelper(n, (0, 1))
  }

  private def formatAbs(x: Int) = {
    "The absolute value fo %d is %d".format(x, abs(x))
  }
  private def formatFactorial(x: Int) = {
    "The factorial of %d is %d".format(x, abs(x))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    "The %s of %d is %d".format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  // Exercise 2.2
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i == 0) true
      else ordered(arr(i + 1), arr(i)) && loop(i - 1)
    }

    if (arr.length > 1) loop(arr.length - 2)
    else true
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    a: A => b: B => f(a, b)
  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B ): A => C = {
    a: A => f(g(a))
  }

  def main(args: Array[String]) : Unit = {
    print(isSorted(Array(1, 2, 3, 4, 5), (next: Int, prev: Int) => next > prev))
  }
}
