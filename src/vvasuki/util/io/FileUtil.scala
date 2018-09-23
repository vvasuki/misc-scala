package vvasuki.util.io

import java.io.FileInputStream
import java.io._
import scala.io.Source

object fileUtil {
/*  Confidence in correctness: High
  Reason: Well tested.*/
  def getFilePath(dir: String, condition: String => Boolean) = {
    // println("dir "+ dir)
    val filesInDir = new File(dir).list.toList
    // println("files " + filesInDir)
    val files = filesInDir.filter(condition).sorted
    dir + files.head
  }

  
/*  Confidence in correctness: High
  Reason: Adapted from internet.*/
  def write(fileName: String)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new java.io.File(fileName))
    try { op(p) } finally { p.close() }
  }
}

