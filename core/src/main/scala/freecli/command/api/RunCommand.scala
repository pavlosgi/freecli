package freecli
package command
package api

case class RunCommand[T](f: T => Unit)
