package freecli
package core
package free

import cats.~>
import cats.instances.all._

import testkit.Test

class FreeAlternativeTest extends Test {

  describe("FreeAlternative") {
    trait Algebra[A]
    case class One[A](name: String, c: Int => A) extends Algebra[A]
    case class Two[A](name: String, c: Int => A) extends Algebra[A]
    case class Three[A](name: String, c: Int => A) extends Algebra[A]
    case class Four[A](name: String, c: Int => A) extends Algebra[A]

    def one: FreeAlternative[Algebra, Int] =
      FreeAlternative.lift(One("One", identity))

    def two: FreeAlternative[Algebra, Int] =
      FreeAlternative.lift(Two("Two", identity))

    def three: FreeAlternative[Algebra, Int] =
      FreeAlternative.lift(Three("Three", identity))

    def four: FreeAlternative[Algebra, Int] =
      FreeAlternative.lift(Four("Four", identity))


    it("should interpret as expected") {
      def nat = new (Algebra ~> Option) {
        override def apply[A](fa: Algebra[A]): Option[A] = {
          fa match {
            case One(_, c) => Option(c(1))
            case Two(_, c) => Option(c(2))
            case Three(_, c) => Option(c(3))
            case Four(_, c) => Option.empty[A]
          }
        }
      }

      one.combineK(two).foldMap(nat) should === (Option(1))
      one.combineK(two).combineK(three).foldMap(nat) should === (Option(1))
      four.combineK(two).combineK(three).foldMap(nat) should === (Some(2))
      four.combineK(four).combineK(three).foldMap(nat) should === (Some(3))
      four.combineK(four).combineK(four).foldMap(nat) should === (None)
    }

    it("should analyze as expected") {
      def analyze = new (Algebra ~> Lambda[a => List[String]]) {
        override def apply[A](fa: Algebra[A]): List[String] = {
          fa match {
            case One(n, _) => List(n)
            case Two(n, _) => List(n)
            case Three(n, _) => List(n)
            case Four(n, _) => List(n)
          }
        }
      }

      one.combineK(two).combineK(three).combineK(four).analyze(analyze) should
        contain theSameElementsAs (List("One", "Two", "Three", "Four"))

    }
  }
}
