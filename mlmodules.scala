object TypeclassStyle extends App {

  trait Monoid[A] {
    def zero:A
    def append (a:A, b:A) : A
  }
  object Monoid {

    implicit object AdditiveInt extends Monoid[Int] {
      def zero = 0
      def append (a:Int, b:Int) = a + b
    }

  }
  object MultiplicativeInt extends Monoid[Int] {
    def zero = 1
    def append(a:Int, b:Int) = a * b
  }

  def foo (implicit ev : Monoid[Int]) : Int = {
    ev.zero
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


  // swag swag swag swag 

  class Cayley[A <: Monoid](val a: A) extends Monoid{
    type T = (a.T => a.T)
    def zero = (x: a.T) => a.append(x , a.zero)
    // def zero = (x: a.T) => x
    def append (f: T, g: T) = (x: a.T) => g (f(x))
  }

  object CayleyAdditive extends Cayley(AdditiveInt)


  // How do I constrain that A#T and B#T are equal?
  class Semiring [A <: Monoid, B <: Monoid] (val a: A, val b: B) (implicit ev : A#T =:= B#T){
    type T = a.T
    def zero : T = a.zero
    def one : T = b.zero.asInstanceOf[T]
  }

  object MyRing extends Semiring(AdditiveInt, MultiplicativeInt)

  // def foobar : Int






  def foo ( implicit ev: Monoid) : ev.T = {
    ev.zero
  }

  def bar = {
    import CayleyAdditive._

    zero
  }

}

