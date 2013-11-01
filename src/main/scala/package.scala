import java.{lang => jl}

package object opt {

  type OptInt     = Opt[jl.Integer, Int]
  type OptLong    = Opt[jl.Long, Long]
  type OptDouble  = Opt[jl.Double, Double]
  type OptFloat   = Opt[jl.Float, Float]
  type OptByte    = Opt[jl.Byte, Byte]
  type OptChar    = Opt[jl.Character, Char]
  type OptShort   = Opt[jl.Short, Short]
  type OptBoolean = Opt[jl.Boolean, Boolean]

  type OptRef[+A <: AnyRef] = Opt[A, A]

  def not[A](implicit A: Box[A]): Opt[A.B, A] = new Opt(null.asInstanceOf[A.B])

  def NotRef[A <: AnyRef]: OptRef[A] = new Opt(null.asInstanceOf[A])
  val NotInt: OptInt                 = new Opt(null.asInstanceOf[jl.Integer])
  val NotLong: OptLong               = new Opt(null.asInstanceOf[jl.Long])
  val NotDouble: OptDouble           = new Opt(null.asInstanceOf[jl.Double])
  val NotFloat: OptFloat             = new Opt(null.asInstanceOf[jl.Float])
  val NotByte: OptByte               = new Opt(null.asInstanceOf[jl.Byte])
  val NotChar: OptChar               = new Opt(null.asInstanceOf[jl.Character])
  val NotShort: OptShort             = new Opt(null.asInstanceOf[jl.Short])
  val NotBoolean: OptBoolean         = new Opt(null.asInstanceOf[jl.Boolean])

}
