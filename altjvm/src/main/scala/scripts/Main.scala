/*
Copyright 2013, 2014, 2015, 2016 AndyL

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

//inspired by git - sha1
//SPEC:
// .neater is stored within maintained directory for following reasons:
// + one can 'mv' tree together with stamp
// + chmod -w guards stamp
// - need to chmod +w to stamp
// - cannot do single files
// example: hash/sha-1 , time stamp, size path (without main dir)
//   fda523c1169de1908b6cc35c6d190172eaa9e67c        1386386536000   914461  ml/ex6.zip

//FEATURES:
// - http://stackoverflow.com/questions/1390592/java-check-if-file-is-already-open
// - split stamp to override and check, make it an atomic operation (useing neater-master)
// - combine buildTree and scanTree, rename/refactor
// - test on SNAPSHOTS!
// - deep and skip check (no sha1)
// - list of duplicated non-empty files!
// - detect same, renamed in groups
// - add all check/compare/deep which scans entire home run those one by one

//FIXES:
// - common ".neater"
// - report soft links
// - ...neater if . is an argument
// - check if compare sensitive to timestamp
// - cannot check.sh .
// - compare - if files are ducplicated and moved to "trash" later removed, compare will not catch it since all_files2.map(_._1).toSet removes duplicates

//OTHER:
//ssh
//thunderbird email
//github repos
//other libraries
//list of apps and downloads
//soft links
//bookmarks
//permissions
//check git reflog for clean history

/*
HINTs:
:power
vals.isettings.maxPrintString = 0
 */

package scripts

import edu.knowitall.collection.immutable.Bag
import java.io.{ File => JFile }
import java.security.DigestInputStream

abstract class Elem { //TODO is it okay to override def with val?
  def name: String
  def hash: String
  def modified: Long
  def path: List[String]
}

case class File(name: String,
    hash: String,
    modified: Long,
    length: Long,
    path: List[String]) extends Elem {
  override def toString() = "File:" + (name, path, modified, hash.substring(0, 4))
}

case class Directory(name: String,
    files: Set[File],
    dirs: Set[Directory],
    hash: String,
    subhash: Bag[String],
    modified: Long,
    path: List[String],
    level: Int) extends Elem {
  override def toString() = {
    val fs = files.map(f => "\t" * (level + 1) + f).mkString("\n")
    val ds = dirs.mkString("\n")
    "\t" * level + "Directory:" +
      (level, name, path, modified, hash.substring(0, 4), subhash.size, subhash.map(_.substring(0, 4))) +
      "\n" + fs + "\n" + ds
  }
}

trait Funcs {
  val PRX="^^^"

  //util
  def removeFirst[T](xs: List[T])(pred: (T) => Boolean): (Boolean, List[T]) = {
    xs match {
      case x :: tail => {
        if (pred(x))
          (true, tail)
        else {
          val (np, ntail) = removeFirst(tail)(pred)
          (np, x :: ntail)
        }
      }
      case Nil => (false, List())
    }
  }
  def applyOnFirst[T](xs: List[T])(fAndPred: (T) => (Boolean, T)): (Boolean, List[T]) = {
    xs match {
      case x :: tail => {
        val (p, nx) = fAndPred(x)
        if (p)
          (true, nx :: tail)
        else {
          val (np, ntail) = applyOnFirst(tail)(fAndPred)
          (np, x :: ntail)
        }
      }
      case Nil => (false, List())
    }
  }

  type FileLine = (String, Long, Long, List[String], String) //hash, modified, length, path, name

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

  def isSoftLink(f: JFile) = {
    //TODO fix handling . - getParent returns null
    val canon = if (f.getParent == null)
      f
    else
      new JFile(f.getParentFile.getCanonicalFile, f.getName)
    !canon.getCanonicalFile.equals(canon.getAbsoluteFile)
  }

  def listValidFiles(d: JFile) = {
    val lf = d.listFiles.filter(_.exists).filter(!isSoftLink(_)).
      filter(_.getName != ".neater").
      filter(_.getName != ".neater-master").
      filter(_.getName != ".DS_Store")
    if (lf.map(_.getName).contains(".git"))
      List()
    else
      lf.toList
  }

  val STAMP_FIRST = "This directory needs to be stamped first."
  val ALREADY_STAMPED = "This directory is already stamped."
  def getNeater(root: java.io.File): JFile = new JFile(root, ".neater")
  def readNeater(n: java.io.File): Option[List[String]] = {
    if (n.exists)
      Option(scala.io.Source.fromFile(n).getLines.toList)
    else
      None
  }

  def formatNeater(l: List[(String, String, String, String)]): List[String] = l.map(_.productIterator.toList.mkString("\t")).sorted

  def formatNeaterLine(l: FileLine): String = List(l._1, l._2, l._3, (l._5 :: l._4.reverse).reverse.mkString("/")).mkString("\t")

  def parseNeaterLine(l: String): FileLine = {
    val List(hash, modified, length, full_path) = l.split('\t').toList
    val name :: path = full_path.split('/').toList.reverse
    (hash, modified.toLong, length.toLong, path.reverse, name)
  }
  def parseNeaterLine2(l: String): (String, String, String, String) = {
    val List(hash, modified, length, full_path) = l.split('\t').toList
    (hash, modified, length, full_path)
  }

  def saveNeater(neater: java.io.File, l: List[(String, String, String, String)]) {
    val p = new java.io.PrintWriter(neater)
    try { formatNeater(l).foreach(p.println(_)) }
    finally { p.close }
  }

  def scanTree(f: JFile): List[(String, String, String, String)] = {
    def helper(f: JFile, p: List[String]): List[(String, String, String, String)] = {
      if (f.exists && !isSoftLink(f)) {
        if (f.isFile)
          List((file_digest(f), f.lastModified.toString, f.length.toString, p.reverse.mkString("/")))
        else
          listValidFiles(f).map(x => helper(x, x.getName :: p)).flatten
      } else
        List()
    }
    helper(f, List())
  }

  //TODO move into Directory constructor
  val md = java.security.MessageDigest.getInstance("SHA-1")
  def hashStuff(ds: Set[Directory], fs: Set[File]) = {
    val p1 = md.digest(fs.toList.map(_.hash).sorted.mkString.getBytes).map(_ & 0xFF).map("%02x".format(_)).mkString
    val p2 = Bag.from(ds.map(_.subhash).flatten).merge(Bag.from(fs.map(_.hash)))
    //val sh=files.map(_.hash)++dirs.map(_.subhash).flatten
    (p1, p2)
  }

  def hashStuffTmp(ds: Set[Directory], fs: Set[File]) = {
    ("xxxx", Bag[String]())
  }

  //we might need to remove file name itself from path in File and Directory: e.g.  name :: path_to_here and d.getName :: path
  def buildTree(root: JFile, dir_ts: Boolean = false): Directory = {
    def helper(root: JFile, path: List[String], l: Int): Directory = {
      val these = listValidFiles(root)
      val files = these.filter(_.isFile).map(f => File(f.getName, file_digest(f), f.lastModified, f.length, f.getName :: path)).toSet
      val all_dirs = these.filter(_.isDirectory).map(d => { helper(d, d.getName :: path, l + 1) })
      val dirs = all_dirs.filter(x => !(x.files.isEmpty && x.dirs.isEmpty)).toSet
      val (p1, p2) = hashStuff(dirs, files)
      Directory(root.getName, files, dirs, p1, p2, if (dir_ts) root.lastModified else 0, path, l)
    }
    helper(root, List(), 0)
  }

  def tree2Flat(d: Directory): Set[File] = d.dirs.map(tree2Flat).flatten ++ d.files
  def flat2Tree(f: List[FileLine]) = {
    def assoc(ds: Set[Directory], fs: Set[File], path_to_here: List[String], path_to_go: List[String], fl: FileLine, level: Int): (Set[Directory], Set[File]) = {
      //println(":" + "\t" * level + "->" + "," + path_to_here + "," + path_to_here + "," + fl)
      val (hash, modified, length, path, name) = fl

      if (path_to_go.isEmpty) {
        assert(!fs.map(_.name).contains(name))
        (ds, fs ++ Set(File(name, hash, modified, length, name :: path_to_here.reverse)))
      } else {
        val curdir = path_to_go.head
        val new_path_to_go = path_to_go.tail
        val new_path_to_here = path_to_here ::: List(curdir)
        val new_path = curdir :: path_to_here.reverse

        if (ds.map(_.name).contains(curdir)) {
          val other_dirs = ds.filterNot(_.name == curdir)
          val changed_dir = ds.filter(_.name == curdir).head
          val (new_dirs, new_files) = assoc(changed_dir.dirs, changed_dir.files, new_path_to_here, new_path_to_go, fl, level + 1)
          val (p1, p2) = hashStuffTmp(new_dirs, new_files)
          val new_dir = Directory(curdir, new_files, new_dirs, p1, p2, 0, new_path, level)
          (other_dirs + new_dir, fs)
        } else {
          val (new_dirs, new_files) = assoc(Set(), Set(), new_path_to_here, new_path_to_go, fl, level + 1)
          val (p1, p2) = hashStuffTmp(new_dirs, new_files)
          val new_dir = Directory(curdir, new_files, new_dirs, p1, p2, 0, new_path, level)
          (ds + new_dir, fs)
        }
      }
    }
    val (ds, fs) = f.foldLeft((Set[Directory](), Set[File]()))((a, b) => assoc(a._1, a._2, List(), b._4, b, 1))
    val (p1, p2) = hashStuff(ds, fs)
    Directory("root", fs, ds, p1, p2, 0, List(), 0)
  }

  def sToTree(f: java.io.File) = {
    if (f.isDirectory) {
      Option(scanTree(f))
    } else {
      val neater = readNeater(f)
      if (neater.isDefined)
        Some(neater.get.map(parseNeaterLine2))
      else
        None
    }
  }

  // it provides diff (both ways) between sha1 sets as well as idenitical but touched files
  // it does not check directories
  def compare(files1: Option[List[(String, String, String, String)]], files2: Option[List[(String, String, String, String)]]) {
    if (files1.isEmpty || files2.isEmpty) {
      println(STAMP_FIRST)
      return
    }
    val (all_files1, all_files2) = (files1.get, files2.get)
    val grp_files1 = all_files1.groupBy(_._1).mapValues(_.map(_.productIterator.toList.mkString(",")).mkString("; "))
    val grp_files2 = all_files2.groupBy(_._1).mapValues(_.map(_.productIterator.toList.mkString(",")).mkString("; "))

    {
      val set_files1 = all_files1.map(_._1).toSet
      val set_files2 = all_files2.map(_._1).toSet
      val extra = set_files2 -- set_files1
      val missing = set_files1 -- set_files2
      if (extra.size > 0) {
        println(PRX+"extra:"+extra.size)
        extra.foreach(f => println(grp_files2(f)))
      }
      if (missing.size > 0) {
        println(PRX+"missing:"+missing.size)
        missing.foreach(f => println(grp_files1(f)))
      }
    }

    {
      val all1 = all_files1.map(x => (x._1, -1 * x._2.toLong, x._3, x._4))
      val all2 = all_files2.map(x => (x._1,  1 * x._2.toLong, x._3, x._4))
      val all = all1 ::: all2
      val g = all.groupBy(x => (x._1, x._4)).filter(_._2.size == 2).mapValues(x=>x.map(_._2).sum).filterNot(_._2==0)
      val t1 = g.filter(_._2.abs<=1000)
      val t2 = g.filter(_._2.abs>1000)
      if (t1.size > 0) {
        println(PRX+"touched <=1000:"+t1.size)
        t1.foreach(x => println(x._1._2 + "->" + x._1._1+":"+x._2))
      }
      if (t2.size > 0) {
        println(PRX+"touched >1000:"+t2.size)
        t2.foreach(x => println(x._1._2 + "->" + x._1._1+":"+x._2))
      }
    }
  }
}

//elementary
object Stamp extends Funcs {
  def main(args: Array[String]) {
    val root = new JFile(args(0))
    assert(root.isDirectory)//TODO fails on "Local Folders"

    //TODO ask to override (always mv old to .neater.1 etc) if checks fine
    val neater = getNeater(root)
    //????handle Exception in thread "main" java.io.IOException: Permission denied
    if (neater.createNewFile) {
      println(" ...stamping")
      val all_files = scanTree(root)
      all_files.foreach(println)
      saveNeater(neater, all_files)
    } else {
      println(ALREADY_STAMPED)
    }
  }
}

//elementary
object Verify extends Funcs {
  def sort(s: Set[String]) = s.map(parseNeaterLine).toList.sortBy(_._4.mkString("/n")).map(formatNeaterLine)
  def main(args: Array[String]) {
    val root = new JFile(args(0))
    assert(root.isDirectory)
    val neater = readNeater(getNeater(root))
    if (neater.isDefined) {
      println(" ...verifying")
      val all_files = formatNeater(scanTree(root))
      val all_files_neater = neater.get
      val set_files_neater = all_files_neater.toSet
      val set_files = all_files.toSet
      val added = set_files -- set_files_neater
      val deleted = set_files_neater -- set_files
      if (added.size > 0) {
        println(PRX+"added:"+added.size)
        sort(added).foreach(println)
      }
      if (deleted.size > 0) {
        println(PRX+"deleted:"+deleted.size)
        sort(deleted).foreach(println)
      }
      if (added.size == 0 && deleted.size == 0)
        println(PRX+"untouched.")

    } else {
      println(STAMP_FIRST)
    }
  }
}

//file based
object Check extends Funcs {
  def main(args: Array[String]) {
    println(" ...checking")
    val f = new JFile(args(0))
    assert(f.isDirectory)
    compare(sToTree(getNeater(f)), sToTree(f))
  }
}

//file based
object Compare extends Funcs {
  def main(args: Array[String]) {
    println(" ...comparing")
    val f1 = new JFile(args(0))
    val f2 = new JFile(args(1))
    compare(sToTree(f1), sToTree(f2))
  }
}

case class DiffTree(name: String, dirs: List[DiffTree], hash: String, touched: Boolean) {
  private def toStringHelper(level: Int): String = {
    if (touched) {
      val ds = dirs.map(_.toStringHelper(level + 1))
      "\t" * level + name + "/" + touched + "(" + hash + ")" + "\n" + ds.map("\t" + _).mkString("\n")
    } else
      "\t" * level + name + "/" + touched + "(" + hash + ")"
  }
  override def toString() = {
    toStringHelper(0)
  }
}

object DiffTree {
  val md = java.security.MessageDigest.getInstance("SHA-1")
  def fromDirectory(d: Directory): DiffTree = {
    val dts = d.dirs.map(fromDirectory)
    val tmp1 = (dts.map(_.hash) ++ d.files.map(_.hash)).toList.sorted.mkString.getBytes
    val h = md.digest(tmp1).map(_ & 0xFF).map("%02x".format(_)).mkString
    DiffTree(d.name, d.dirs.map(fromDirectory).toList, h, false)
  }
}

trait FuncsTree extends Funcs {
  def pruneOne(from: DiffTree, that: DiffTree): (Boolean, DiffTree) = {
    val (removed, without_first_match) = removeFirst(from.dirs)(_.hash == that.hash)
    if (removed)
      (true, DiffTree(from.name, without_first_match, from.hash, true))
    else {
      val (replaced, with_first_replaced) = applyOnFirst(from.dirs)(pruneOne(_, that))
      (replaced, DiffTree(from.name, with_first_replaced, from.hash, replaced))
    }
  }

  def pruneAll(from: DiffTree, that: DiffTree): (Boolean, DiffTree) = {
    //pruneOne that from from
    val (r, nf) = pruneOne(from, that)
    //if succeded, finish and return new from
    if (r)
      (true, nf)
    else {
      // for every that[i] pruneOne it from from
      // if succeded, use new from
      // else incude that[i] to reclist
      def helper1(from: DiffTree, todos: List[DiffTree], reclist: List[DiffTree] = List(), changed: Boolean = false): (Boolean, DiffTree, List[DiffTree]) = {
        todos match {
          case todo :: tail => {
            val (r, nf) = pruneOne(from, todo)
            if (r)
              helper1(nf, tail, reclist, true)
            else
              helper1(from, tail, todo :: reclist, changed)
          }
          case Nil => (changed, from, reclist)
        }
      }
      val (changed1, from1, reclist1) = helper1(from, that.dirs)
      //for every reclist[i] pruneAll it from from
      //return last from
      def helper2(from: DiffTree, todos: List[DiffTree], changed: Boolean): (Boolean, DiffTree) = {
        todos match {
          case todo :: tail => {
            val (r, nf) = pruneAll(from, todo)
            helper2(nf, tail, r)
          }
          case Nil => (changed, from)
        }
      }
      helper2(from1, reclist1, changed1)
    }
  }

  def sToDirectory(f: java.io.File) = {
    if (f.isDirectory)
      Some(buildTree(f))
    else {
      val neater = readNeater(f)
      if (neater.isDefined)
        Some(flat2Tree(neater.get.map(parseNeaterLine)))
      else
        None
    }
  }

  def pruneAndDiff(tree1: Option[Directory], tree2: Option[Directory]) {
    if (tree1.isEmpty || tree2.isEmpty) {
      println(STAMP_FIRST)
      return
    }
    val (dt1, dt2) = (DiffTree.fromDirectory(tree1.get), DiffTree.fromDirectory(tree2.get))
    val added = pruneAll(dt1, dt2)
    println(PRX+"added:")
    println(added)

    val deleted = pruneAll(dt2, dt1)
    println(PRX+"deleted:")
    println(deleted)
  }
}

//tree based
object Changed extends FuncsTree {
  def main(args: Array[String]) {
    println(" ...changed") //TODO replace with ing
    val f = new JFile(args(0))
    assert(f.isDirectory)
    pruneAndDiff(sToDirectory(f), sToDirectory(getNeater(f)))
  }
}

//tree based
object Diff extends FuncsTree {
  def main(args: Array[String]) {
    println(" ...diff") //TODO replace with ing
    val f1 = new JFile(args(0))
    val f2 = new JFile(args(1))
    pruneAndDiff(sToDirectory(f1), sToDirectory(f2))
  }
}

//experimental
object Analyze extends Funcs {
  def isPartOf[T1 <: Elem, T2 <: Elem](d: T1, dd: T2) = dd.path.reverse == d.path.reverse.take(dd.path.length)

  def isPartOfAny[T1 <: Elem, T2 <: Elem](d: T1, ds: Set[T2]) = ds.find(isPartOf(d, _)).isDefined

  def isAnyPartOfAny[T1 <: Elem, T2 <: Elem](d: T1, dss: Set[Set[T2]]) = dss.find(isPartOfAny(d, _)).isDefined

  def flatFiles(d: Directory): Set[File] = d.files ++ d.dirs.flatMap(flatFiles)

  def flatDirectories(d: Directory): Set[Directory] = d.dirs ++ d.dirs.flatMap(flatDirectories)

  def similarBags[T](b1: Bag[T], b2: Bag[T]) = {
    val THRESH = 32
    val b1s = b1.size
    val b2s = b2.size
    val dd = (b1s - b2s).toDouble
    val ddd = dd.abs //TODO why implicit?
    if (b1s > THRESH && b2s > THRESH) {
      if ((b1s - b2s).toDouble.abs / (b1s + b2s).toDouble > 0.03) //TODO constant
        false
      else {
        val s1 = b1.toSet
        val s2 = b2.toSet
        val d = (s1 -- s2) ++ (s2 -- s1)
        d.size.toDouble / (s1.size + s2.size) < 0.2 //TODO constant
      }
    } else
      b1 == b2
  }
  def find_similars(i: List[Directory]) = {
    def find_similar(s: Directory, i: List[Directory]) = {
      var similar = Set[Directory](s)
      def next(s: Directory, i: List[Directory]): Unit = {
        if (i != Nil) {
          // if(s.subhash==i.head.subhash)
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
    println(" ... analyzing")
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

