

// http://existentialtype.net/2008/05/26/modules-in-scala/


object Foo {

  trait Nat {
    type T
    val z: T
    def s(x: T): T
  }

  object Int extends Nat {
    type T = Int
    val z = 0
    def s(x: Int) = x + 1
  }

  // todo fix
  object BinaryInt extends Nat {
    type T = List[Boolean]
    val z = List(false)
    def s(list: List[Boolean]) = list match {
      case x :: xs => {
        if (x)
          x :: s(xs)
        else
          true :: xs
      }
      case Nil => {
        List(false)
      }
    }
  }
}
