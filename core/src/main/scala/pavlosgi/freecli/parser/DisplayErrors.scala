package pavlosgi.freecli.parser

import cats.Semigroup

abstract class DisplayErrors[E: Semigroup] {
  def display(errors: E): String
}
