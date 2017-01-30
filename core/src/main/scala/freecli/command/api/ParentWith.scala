package freecli
package command
package api

case class ParentWith[P, B](parent: P, value: B)