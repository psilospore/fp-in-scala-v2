package ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None() => None()
    case Some(a) => Some(f(a))
  }
  def flapMap[B](f: A => Option[B]): Option[B] = this match {
    case None() => None()
    case Some(a) => f(a)
  }

  // B >: A mean B is a super type of A. => lazy evaluate
  def getOrElse[B >: A](default: => B): B = this match {
    case None() => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None() => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None() => None()
    case Some(a) => if(f(a)) this else None()
  }
}

case class Some[+A](get: A) extends Option[A]
case class None() extends Option[Nothing]


