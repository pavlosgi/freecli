package freecli
package command
package api

case class PartialCommand[P](f: P => Command)
