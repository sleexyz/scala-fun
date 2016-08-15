class ContextBoundsSyntax {

  trait Monoid[A] {
    def zero:A
    def append (a:A, b:A) : A
  }

  implicit object AdditiveInt extends Monoid[Int] {
    def zero = 0
    def append (a:Int, b:Int) = a + b
  }

  def gimmeZero[T : Monoid](x: T): T = {
    implicitly[Monoid[T]].zero
  }

  def gimmeZero2[T: Monoid](x: T): T = {
    implicitly[Monoid[T]].zero
  }

  def foo() {
    gimmeZero(100)
  }
  
}
