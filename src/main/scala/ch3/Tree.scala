package ch3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Leaf[A], right: Leaf[A]) extends Tree[A]

