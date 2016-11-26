package pavlosgi.freecli.core.dsl.config

private[config] trait CanProduceConfigDsl[B[_], T, A] {
  def apply(a: B[T]): ConfigDsl[A]
}
