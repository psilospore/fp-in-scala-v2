package ch3

sealed trait List[+A]

// Constructors
case object Nil extends List[Nothing] // Empty List
case class Cons[+A](head: A, tail: List[A]) extends List[A] // recursively defined constructor that has an element of A at the Head and either a another List[A] or Nil (which is also a List)

object List { // companion object

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head: Int, tail: List[Int]) => head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(head, tail) => head * product(tail)
  }

  //arr: A* variadic function syntax
  def apply[A](arr: A*):  List[A] = {
    if (arr.isEmpty) Nil
    else Cons[A](arr.head, apply[A](arr.tail: _*))
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, t) => t
      case Nil => Nil
    }
  }

  // 3.3
  def setHead[A](t: List[A], head: A): List[A] = {
    Cons(head, t)
  }

  // 3.4
  // Should I throw an exception if n is greater than the length?
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (p(h)) dropWhile(t, p)
        else t

    }
  }


  // 3.6
  def init[A](l: List[A]) : List[A] = {
    l match {
      case Nil => Nil // initial empty list will just return an empty list
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A,B](as: List[A], acc: B)(f: (A, B) => B): B =
    as match {
      case Nil => acc
      case Cons(h, t) => f(h, foldRight(t, acc)(f))
    }

  def sum2(ne: List[Int]): Int = foldRight(ne, 0)(_ + _)
  def product2(ne: List[Double]): Double = foldRight(ne, 1.0)(_ * _)

  // 3.7 no

  // 3.8 it would reconstruct the list

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // 3.10
  def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B =
    as match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h))(f)
    }

  // 3.11

  // 3.12 TODO test
  def reverseFold[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

  // 3.13 don't feel like it since it's trivial

  // 3.14
  def append[A](as: List[A], last: A): List[A] =
    foldLeft(as, Nil: List[A])((acc, x) => x match {
      case Nil => reverseFold(Cons(last, acc))
      case _ => Cons(x, acc) // reverse so we can add to begining and then reverse
    })

  // 3.15
  def concat[A](first: List[A], second: List[A]): List[A] =
    foldRight(l , Nil: List[A])(append)

  // 3.16
  def addOne(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  // 3.17
  def dString(as: List[Double]) = List[String] =
    foldRight(as, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((x, acc) => {
      if (f(x)) Cons(x, acc)
      else acc
    })
  
  // 3.20 TODO test
  def flatMap[A, B](as: List[A])(f: A => List[B]) : List[B] =
    foldRight(as, Nil: List[B])((x, acc) => concat(f(x), acc))

  // 3.21
//  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
//    flatMap(as)(a: A => f(a))

  // 3.22
  def combineAdd(l1: List[Int], l2: List[Int]) =
    ???



}
