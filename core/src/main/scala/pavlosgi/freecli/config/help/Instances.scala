package pavlosgi
package freecli
package config
package help

import java.io.File

import cats.Show

trait Instances {
  implicit def strShow = new Show[String] {
    override def show(f: String): String = f
  }

  implicit def boolShow = new Show[Boolean] {
    override def show(f: Boolean): String = f.toString
  }

  implicit def intShow = new Show[Int] {
    override def show(f: Int): String = f.toString
  }

  implicit def fileShow = new Show[File] {
    override def show(f: File): String = f.getAbsolutePath
  }
}

object Instances extends Instances