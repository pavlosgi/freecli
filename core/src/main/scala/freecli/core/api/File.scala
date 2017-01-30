package freecli
package core
package api

case class ExistentFile(file: java.io.File)
object ExistentFile {
  implicit def file2JavaFile(f: ExistentFile): java.io.File = f.file
}

case class NewFile(file: java.io.File)
object NewFile {
  implicit def file2JavaFile(f: NewFile): java.io.File = f.file
}