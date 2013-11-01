package example

import opt._

object Example extends App {
  
  implicit class AnyOps[A](self: A) {
    def must_==(that: A) = {
      if(self == that) println(s"$self is equal to $that")
      else assert(false, s"$self is not equal to $that")
    }
  }

  Opt(1).isEmpty must_== false

  Opt(1).filter(_ > 10).isEmpty must_== true

  Opt(1).get must_== 1

  Opt(1).map(_ + 2) must_== Opt(3)
  not[Int].map(_ + 10) must_== not[Int]

  not[Int].toOption must_== None
  Opt(1).toOption must_== Option(1)

  Opt(1).toList must_== List(1)

  Opt(1).toRight("x") must_== Right(1)
  Opt(1).toLeft("x") must_== Left(1)
  not[Int].toRight("x") must_== Left("x")

  Opt("a").collect{ case a if a.size == 1 => a * 3 } must_== Opt("aaa")

  not[Int].isDefined must_== false
  Opt("a").isDefined must_== true

  not[Int] orElse not[Int] must_== not[Int]
  Opt(1)   orElse Opt(2)   must_== Opt(1)
  Opt(1)   orElse not[Int] must_== Opt(1)
  not[Int] orElse Opt(2)   must_== Opt(2)

  Opt(1)   getOrElse 2 must_== 1
  not[Int] getOrElse 2 must_== 2

  val x1 = for{
    a <- Opt("a")
    if a.size > 0
    b <- Opt(1)
  } yield (a, b)

  x1 must_== Opt(("a", 1))


  val x2 = for{
    a <- Opt("a")
    b <- not[Int]
  } yield (a, b)

  x2 must_== not[(String, Int)]


  val x3 = Opt(0) match {
    case Opt(a) if a > 10 => 1
    case Opt(a) => 2
    case Not() => 3
  }
 
  x3 must_== 2
 
}
