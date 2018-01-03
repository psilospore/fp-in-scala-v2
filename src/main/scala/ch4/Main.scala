package ch4

object Main {

  def mean(xs: Seq[Double]): Option[Double] = ???

  def variance(xs:Seq[Double]): Option[Double] = {
    mean(xs).flatMap( m => mean(xs.map( x => Math.pow(x - m, 2))))

  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None()}
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flapMap(a => b.map(b => f(a, b)))
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x/y)
    catch {case e: Exception => Left(e)}

  //Abstrated out
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e: Exception => Left(e)}
  }

  

}
