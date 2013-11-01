package opt

import Opt.not

final class Opt[+A] private[opt](val value: A) extends AnyVal {

  def isEmpty: Boolean = value == null

  def get: A = {
    if(isEmpty) throw new java.util.NoSuchElementException("Not.get")
    else value
  }

  def map[B](f: A => B): Opt[B] =
    if(isEmpty) not[B] else new Opt(f(get))

  def withFilter(f: A => Boolean): Opt[A] = filter(f)

  def filter(f: A => Boolean): Opt[A] =
    if(isEmpty) this
    else if(f(get)) this
    else not

  def flatMap[B](f: A => Opt[B]): Opt[B] =
    if(isEmpty) not[B] else f(get)

  def collect[B](pf: PartialFunction[A, B]): Opt[B] =
    if(isEmpty) not
    else if(pf isDefinedAt get) map(pf)
    else not

  def toList: List[A] =
    if(isEmpty) Nil
    else get :: Nil

  def toOption: Option[A] =
    if(isEmpty) None
    else Some(get)

  def toRight[B](left: => B): Either[B, A] =
    if(isEmpty) Left(left)
    else Right(get)

  def toLeft[B](right: => B): Either[A, B] =
    if(isEmpty) Right(right)
    else Left(get)

  def filterNot(f: A => Boolean): Opt[A] = filter(!f(_))

  def isDefined: Boolean = !isEmpty

  def foreach[U](f: A => U): Unit =
    if(!isEmpty) f(get)

  def orElse[B >: A](alternative: => Opt[B]): Opt[B] =
    if(isEmpty) alternative
    else this

  def getOrElse[B >: A](default: => B): B =
    if(isEmpty) default
    else get

  def forall(f: A => Boolean): Boolean =
    if(isEmpty) true
    else f(get)

  def exists(f: A => Boolean): Boolean =
    if(isEmpty) false
    else f(get)

  def fold[X](opt: A => X, not: => X): X =
    if(isEmpty) not
    else opt(get)

  override def toString = if(isEmpty) "Not" else "Opt(" + value + ")"
}

object Opt{

  def apply[A](value: A): Opt[A] = new Opt[A](value)

  def not[A]: Opt[A] = new Opt[A](null.asInstanceOf[A])

  def unapply[A](value: Opt[A]): Opt[A] = value

}

object Not {
  def unapply[A](value: Opt[A]): Boolean = value.isEmpty
}

