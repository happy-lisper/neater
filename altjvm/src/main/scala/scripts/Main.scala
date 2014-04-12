/*
Copyright 2013 AndyL

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

//SPEC:
// .neater is stored within maintained dir for following reasons:
// + one can 'mv' tree together with stamp
// + chmod -w guards stamp
// - need to chmod +w to stamp
// - cannot do single files

//FEATURES:
// - http://stackoverflow.com/questions/1390592/java-check-if-file-is-already-open
// - split stamp to override and check (not sure why wanted this)
// - check - make more intelligent diff which collapses branches
// - combine buildTree and scanTree, rename/refactor
// - test on SNAPSHOTS!
// - deep and skimcheck (no sha1)
// - list of duplicated non-empty files!

//FIXES:
// - common ".neater"
// - report soft links
// - ...neater if . is an argument
// - check if compare sensiteve to timestamp

//OTHER:

//ssh
//thundrbird email
//github repos
//other libraries
//list of apps and downloads
//soft links
//bookmarks
//clean reflog

package scripts

import edu.knowitall.collection.immutable.Bag
import java.io.{ File => JFile }
import java.security.DigestInputStream

abstract class Elem { //???? is it okay to override val over def ?
  def name: String
  def hash: String
  def modified: Long
  def path: List[String]
}

case class File(name: String, hash: String, modified: Long, size: Long, path: List[String]) extends Elem
case class Directory(name: String, files: Set[File], dirs: Set[Directory], hash: String, subhash: Bag[String], modified: Long, path: List[String], level: Int) extends Elem {
  override def toString() = "Directory" + (level, name, path, files, dirs, modified, hash.substring(0, 4), subhash.size, subhash.map(_.substring(0, 4)))
}

trait Funcs {
  def file_digest(f: JFile) = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    val is = new java.io.FileInputStream(f)
    val dis = new DigestInputStream(is, md)
    val buffer = new Array[Byte](1024 * 1024)
    Iterator.continually(dis.read(buffer)).takeWhile(_ != -1).foreach(x => Unit)
    is.close
    dis.close
    md.digest.map(_ & 0xFF).map("%02x".format(_)).mkString
  }

  def isPartOf[T1 <: Elem, T2 <: Elem](d: T1, dd: T2) = dd.path.reverse == d.path.reverse.take(dd.path.length)

  def isPartOfAny[T1 <: Elem, T2 <: Elem](d: T1, ds: Set[T2]) = ds.find(isPartOf(d, _)).isDefined

  def isAnyPartOfAny[T1 <: Elem, T2 <: Elem](d: T1, dss: Set[Set[T2]]) = dss.find(isPartOfAny(d, _)).isDefined

  def flatFiles(d: Directory): Set[File] = d.files ++ d.dirs.flatMap(flatFiles)

  def flatDirectories(d: Directory): Set[Directory] = d.dirs ++ d.dirs.flatMap(flatDirectories)

  def isSoftLink(f: JFile) = {
    val canon = if (f.getParent == null)
      f
    else
      new JFile(f.getParentFile.getCanonicalFile, f.getName)
    !canon.getCanonicalFile.equals(canon.getAbsoluteFile)
  }

  def listValidFiles(d: JFile) = {
    val lf = d.listFiles.filter(_.exists).filter(!isSoftLink(_)).filter(_.getName != ".neater")
    if (lf.map(_.getName).contains(".git"))
      List()
    else
      lf.toList
  }

  def getNeater(root: java.io.File) = new JFile(root, ".neater")

  def readNeater(neater: java.io.File) = scala.io.Source.fromFile(neater).getLines.toList

  def formatNeater(l: List[List[String]]) = l.map(_.mkString("\t")).sorted

  def saveNeater(neater: java.io.File, l: List[List[String]]) {
    val p = new java.io.PrintWriter(neater)
    try { formatNeater(l).foreach(p.println(_)) }
    finally { p.close }
  }

  def scanTree(f: JFile) = {
    def helper(f: JFile, p: List[String]): List[List[String]] = {
      if (f.exists && !isSoftLink(f)) {
        if (f.isFile)
          List(List(file_digest(f), f.lastModified.toString, f.length.toString, p.reverse.mkString("/")))
        else
          listValidFiles(f).map(x => helper(x, x.getName :: p)).flatten
      } else
        List()
    }
    helper(f, List())
  }

  def buildTree(root: JFile): Directory = {
    def helper(root: JFile, path: List[String], l: Int): Directory = {
      val these = listValidFiles(root)
      val files = these.filter(_.isFile).map(f => File(f.getName, file_digest(f), f.lastModified, f.length, f.getName :: path))
      val dirs = these.filter(_.isDirectory).map(d => { helper(d, d.getName :: path, l + 1) })
      val md = java.security.MessageDigest.getInstance("SHA-1")
      val d = md.digest(files.map(_.hash).sorted.mkString.getBytes).map(_ & 0xFF).map("%02x".format(_)).mkString
      val sh = Bag.from(dirs.map(_.subhash).flatten).merge(Bag.from(files.map(_.hash)))
      //val sh=files.map(_.hash)++dirs.map(_.subhash).flatten
      Directory(root.getName, files.toSet, dirs.toSet, d, sh, root.lastModified, path, l)
    }
    helper(root, List(), 0)
  }

  def tree2Flat(d: Directory): List[List[String]] = d.dirs.toList.map(tree2Flat).flatten ::: d.files.toList.map(x => List(x.hash, x.modified.toString, x.size.toString, x.path.reverse.mkString("/")))
}

object Stamp extends Funcs {
  def main(args: Array[String]) {
    val root = new JFile(args(0))
    assert(root.isDirectory)

    val neater = getNeater(root)
    //????handle Exception in thread "main" java.io.IOException: Permission denied
    if (neater.createNewFile) {
      println(" ...creating neater")
      val all_files = scanTree(root)
      all_files.foreach(println)
      saveNeater(neater, all_files)
    } else {
      println(" ... this directory is already stamped.")
    }
  }
}

object Check extends Funcs {
  def main(args: Array[String]) {
    val root = new JFile(args(0))
    assert(root.isDirectory)
    val neater = getNeater(root)
    if (neater.exists) {
      println(" ...checking neater")
      val all_files = formatNeater(scanTree(root))
      val all_files_neater = readNeater(neater)
      val set_files_neater = all_files_neater.toSet
      val set_files = all_files.toSet
      val added = set_files -- set_files_neater
      val deleted = set_files_neater -- set_files
      if (added.size > 0) {
        println("added:")
        added.foreach(println)
      }
      if (deleted.size > 0) {
        println("deleted:")
        deleted.foreach(println)
      }
    } else {
      println(" ...this directory needs to be stamped first.")
    }
  }
}

object Compare extends Funcs {
  def main(args: Array[String]) {
    val root1 = new JFile(args(0))
    val root2 = new JFile(args(1))
    assert(root1.isDirectory)
    assert(root2.isDirectory)
    val all_files1 = scanTree(root1)
    val grp_files1 = all_files1.groupBy(_(0)).mapValues(_.map(_.mkString(",")).mkString("; "))
    val all_files2 = scanTree(root2)
    val grp_files2 = all_files2.groupBy(_(0)).mapValues(_.map(_.mkString(",")).mkString("; "))

    {
      val set_files1 = all_files1.map(_(0)).toSet
      val set_files2 = all_files2.map(_(0)).toSet
      val extra = set_files2 -- set_files1
      val missing = set_files1 -- set_files2
      if (extra.size > 0) {
        println("extra:")
        extra.foreach(f => println(grp_files2(f)))
      }
      if (missing.size > 0) {
        println("missing:")
        missing.foreach(f => println(grp_files1(f)))
      }
    }
    {
      val set_files1 = all_files1.map(x => (x(0), x(1), x(3))).toSet
      val set_files2 = all_files2.map(x => (x(0), x(1), x(3))).toSet
      val extra = set_files2 -- set_files1
      val missing = set_files1 -- set_files2
      val all = extra ++ missing
      val all2 = all.groupBy(x => (x._1, x._3)).filter(_._2.size > 1)
      println("touched:")
      all2.keys.foreach(f => println(f._2 + "->" + f._1))
    }
  }
}

object Analyze extends Funcs {
  def similarBags[T](b1: Bag[T], b2: Bag[T]) = {
    val THRESH = 32
    val b1s = b1.size
    val b2s = b2.size
    if (b1s > THRESH && b2s > THRESH) {
      if ((b1s - b2s).toDouble.abs / (b1s + b2s) > 0.03)
        false
      else {
        val s1 = b1.toSet
        val s2 = b2.toSet
        val d = (s1 -- s2) ++ (s2 -- s1)
        d.size.toDouble / (s1.size + s2.size) < 0.2
      }
    } else
      b1 == b2
  }
  def find_similars(i: List[Directory]) = {
    def find_similar(s: Directory, i: List[Directory]) = {
      var similar = Set[Directory](s)
      def next(s: Directory, i: List[Directory]): Unit = {
        if (i != Nil) {
          //          if(s.subhash==i.head.subhash)
          if (similarBags(s.subhash, i.head.subhash))
            if (!isPartOfAny(i.head, similar))
              similar = similar + i.head
          next(s, i.tail)
        }
      }
      next(s, i)
      similar
    }

    var similars = Set[Set[Directory]]()
    def next(s: Directory, i: List[Directory]): Unit = {
      if (i != Nil) {
        if (!isAnyPartOfAny(s, similars)) {
          val fs = find_similar(s, i)
          if (fs.size > 1)
            similars = similars + fs
        }
        next(i.head, i.tail)
      }
    }
    next(i.head, i.tail)
    similars
  }

  def find_equal(similars: Set[Set[Directory]], files: Set[File]) = {
    val potentially_unique_files = files.filter(!isAnyPartOfAny(_, similars))
    val grouped_files = potentially_unique_files.groupBy(_.hash).filter(_._2.size > 1)
    val grouped_files2 = grouped_files.toSet
    grouped_files2.map(_._2)
  }

  def main(args: Array[String]) {
    val root = new JFile(args(0))
    assert(root.isDirectory)
    val t = buildTree(root)
    println(".. built")
    val all_files = flatFiles(t)
    val all_dirs = flatDirectories(t).toList.sortBy(_.level)
    val sims = find_similars(all_dirs)
    println("-- similar dirs:")
    sims.foreach(f => println("* " + f.map(_.path.reverse.mkString("/")).mkString(" ")))
    println("-- equal files:")
    val eqs = find_equal(sims, all_files)
    eqs.foreach(f => println("* " + f.map(_.path.reverse.mkString("/")).mkString(" ")))
  }
}
