object TypeclassStyle extends App {
  trait Monoid[A] {
    def zero:A
    def append (a:A, b:A) : A
  }
  implicit object AdditiveInt extends Monoid[Int] {
    def zero = 0
    def append (a:Int, b:Int) = a + b
  }


  object MultiplicativeInt extends Monoid[Int] {
    def zero = 1
    def append(a:Int, b:Int) = a * b
  }


  class Semiring[A](additive: Monoid[A], multiplicative: Monoid[A]) {
    def zero = additive.zero
    def one = multiplicative.zero
    def add(a:A, b:A) = additive.append(a, b)
    def mul(a:A, b:A) = multiplicative.append(a, b)
  }

  object MyRing extends Semiring[Int](AdditiveInt, MultiplicativeInt)


  def foo (implicit ev : Monoid[Int]) : Int = {
    import ev._
    zero
  }

}




object ModuleStyle {
  trait Monoid {
    type T
    def zero : T
    def append (a:T, b:T) :T
  }

  object Monoid {
    implicit object Unit extends Monoid {
      type T = Unit
      def zero = ()
      def append (a:Unit, b:Unit) = ()

    }
  }
  object StringMonoid extends Monoid {
    type T = String
    def zero = ""
    def append(a:String, b:String) = a + b
  }

  object AdditiveInt extends Monoid {
    type T = Int
    def zero = 0
    def append(a:Int, b:Int) = a + b
  }

  object MultiplicativeInt extends Monoid {
    type T = Int
    def zero = 1
    def append(a:Int, b:Int) = a * b
  }


  class Cayley[A <: Monoid](val a: A) extends Monoid{
    type T = (a.T => a.T)
    def zero = (x: a.T) => a.append(x , a.zero)
    def append (f: T, g: T) = (x: a.T) => g (f(x))
  }

  object CayleyAdditive extends Cayley(AdditiveInt)


  class Semiring [A <: Monoid, B <: Monoid] (val a: A, val b: B) (implicit ev : A#T =:= B#T){
    type T = a.T
    def zero : T = a.zero
    def one : T = b.zero.asInstanceOf[T]
  }

  object MyRing extends Semiring(AdditiveInt, MultiplicativeInt)



  def foo ( implicit ev: Monoid) : ev.T = {
    ev.zero
  }

  def bar = {
    import CayleyAdditive._

    zero
  }

  def testExistential = {
    val monoids = Seq(StringMonoid, AdditiveInt, MultiplicativeInt, CayleyAdditive) map (x => x.append (x.zero,x.zero))
    monoids
  }



}

