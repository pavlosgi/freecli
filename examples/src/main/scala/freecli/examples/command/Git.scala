package freecli
package examples
package command

import java.io.File

import freecli.core.all._
import freecli.config.all._
import freecli.command.all._

object Git extends App {
  case class CommitConfig(all: Boolean, message: String)
  val commitCommand =
    cmd("commit") {
      takesG[CommitConfig] {
        O.help --"help" ::
        flag --"all" -'a' -~ des("Add changes from all known files") ::
        O.string -'m' -~ req -~ des("Commit message")
      } ::
      runs[CommitConfig] { config =>
        if (config.all) {
          println(s"Commited all ${config.message}!")
        } else {
          println(s"Commited ${config.message}!")
        }
      }
    }

  val rmCommand =
    cmd("rm") {
      takesG[File] {
        O.help --"help" ::
        file -~ des("File to remove from git")
      } ::
      runs[File] { f =>
        println(s"Removed file ${f.getAbsolutePath} from git")
      }
    }

  val remoteCommand =
   cmd("remote") {
     takes(O.help --"help") ::
     cmd("add") {
       takesT {
         O.help --"help" ::
         string -~ des("Remote name") ::
         string -~ des("Remote url")
       } ::
       runs[(String, String)] {
         case (s, u) => println(s"Remote $s $u added")
       }
     } ::
     cmd("rm") {
       takesG[String] {
         O.help --"help" ::
         string -~ des("Remote name")
       } ::
       runs[String] { s =>
         println(s"Remote $s removed")
       }
     }
   }

  val git =
    cmd("git", des("Version control system")) {
      takes(help --"help" :: version --"version" -~ value("v1.0")) ::
      commitCommand ::
      rmCommand ::
      remoteCommand
    }

  val res = runCommandOrFail(git)(args).run
}
