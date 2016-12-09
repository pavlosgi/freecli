package pavlosgi.freecli

package object core extends formatting.Formatting {
  type ResultT[E, S, A] = data.ResultT[E, S, A]
  val ResultT = data.ResultT
}
