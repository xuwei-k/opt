package opt

import java.{lang => jl}
import Opt._not

final class Opt[+A <: AnyRef, +B] private[opt](val value: A) extends AnyVal {

  def isEmpty: Boolean = value eq null

  def get: B = {
    if(isEmpty) throw new java.util.NoSuchElementException("Not.get")
    else value.asInstanceOf[B]
  }

  def map[C](f: B => C)(implicit C: Box[C]): Opt[C.B, C] =
    if(isEmpty) not[C] else new Opt[C.B, C](f(get).asInstanceOf[C.B])

  def withFilter(f: B => Boolean): Opt[A, B] = filter(f)

  def filter(f: B => Boolean): Opt[A, B] =
    if(isEmpty) this
    else if(f(get)) this
    else _not

  def flatMap[X <: AnyRef, Y](f: B => Opt[X, Y]): Opt[X, Y] =
    if(isEmpty) _not[X, Y] else f(get)

  def collect[C](pf: PartialFunction[B, C])(implicit C: Box[C]): Opt[C.B, C] =
    if(isEmpty) _not
    else if(pf isDefinedAt get) map(pf)
    else _not

  def toList: List[B] =
    if(isEmpty) Nil
    else get :: Nil

  def toOption: Option[B] =
    if(isEmpty) None
    else Some(get)

  def toRight[C](left: => C): Either[C, B] =
    if(isEmpty) Left(left)
    else Right(get)

  def toLeft[C](right: => C): Either[B, C] =
    if(isEmpty) Right(right)
    else Left(get)

  def filterNot(f: B => Boolean): Opt[A, B] = filter(!f(_))

  def isDefined: Boolean = !isEmpty

  def foreach[U](f: B => U): Unit =
    if(!isEmpty) f(get)

  def orElse[C >: A <: AnyRef, D >: B](alternative: => Opt[C, D]): Opt[C, D] =
    if(isEmpty) alternative
    else this

  def getOrElse[C >: B](default: => C): C =
    if(isEmpty) default
    else get

  def forall(f: B => Boolean): Boolean =
    if(isEmpty) true
    else f(get)

  def exists(f: B => Boolean): Boolean =
    if(isEmpty) false
    else f(get)

  def fold[X](opt: B => X, not: => X): X =
    if(isEmpty) not
    else opt(get)

  override def toString = if(isEmpty) "Not" else "Opt(" + value + ")"
} 

object Opt{

  def apply[A](value: A)(implicit A: Box[A]): Opt[A.B, A] = new Opt[A.B, A](value.asInstanceOf[A.B])

  private def _not[A <: AnyRef, B]: Opt[A, B] = new Opt[A, B](null.asInstanceOf[A])

  def unapply[A <: AnyRef, B](value: Opt[A, B]): Opt[A, B] = value

}

object Not {
  def unapply[A <: AnyRef, B](value: Opt[A, B]): Boolean = value.isEmpty
}

sealed abstract class Box[A] {
  type B <: AnyRef
}

object Box {

  implicit val int     = new Box[Int]{     type B = jl.Integer }
  implicit val long    = new Box[Long]{    type B = jl.Long }
  implicit val double  = new Box[Double]{  type B = jl.Double }
  implicit val float   = new Box[Float]{   type B = jl.Float }
  implicit val byte    = new Box[Byte]{    type B = jl.Byte }
  implicit val char    = new Box[Char]{    type B = jl.Character }
  implicit val short   = new Box[Short]{   type B = jl.Short }
  implicit val boolean = new Box[Boolean]{ type B = jl.Boolean }

  private[this] val _ref = new Box[Nothing]{type B = Nothing}

  implicit def ref[A <: AnyRef]: Box[A]{type B = A} =
    _ref.asInstanceOf[Box[A]{type B = A}]

}
