package pavlosgi.freecli.examples.command

import java.io.File

import pavlosgi.freecli.config._
import pavlosgi.freecli.command._

object Git extends App {
  case class CommitConfig(all: Boolean, message: String)
  val commitCommand =
    cmd("commit") {
      takesG[CommitConfig] {
        o.help --"help" ::
        flag --"all" -'a' -~ des("Add changes from all known files") ::
        o.string -'m' -~ req -~ des("Commit message")
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
        o.help --"help" ::
        file -~ des("File to remove from git")
      } ::
      runs[File] { f =>
        println(s"Removed file ${f.getAbsolutePath} from git")
      }
    }

  val remoteCommand =
   cmd("remote") {
     cmd("add") {
       takesT {
         o.help --"help" ::
         string -~ des("Remote name") ::
         string -~ des("Remote url")
       } ::
       runs[(String, String)] {
         case (s, u) => println(s"Remote $s $u added")
       }
     } ::
     cmd("rm") {
       takesG[String] {
         o.help --"help" ::
         string -~ des("Remote name")
       } ::
       runs[String] { s =>
         println(s"Remote $s removed")
       }
     }
   }

  val git =
    cmd("git", des("Version control system")) {
      commitCommand ::
      rmCommand ::
      remoteCommand
    }

  val res = parseCommandOrHelp(args)(git).run
}
