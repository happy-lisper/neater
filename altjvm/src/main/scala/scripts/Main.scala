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

//inpired by GitHub
//SPEC:
// .neater is stored within maintained dir for following reasons:
// + one can 'mv' tree together with stamp
// + chmod -w guards stamp
// - need to chmod +w to stamp
// - cannot do single files
// example: hash/sha-1 , time stamp, size path (without main dir)
//   fda523c1169de1908b6cc35c6d190172eaa9e67c        1386386536000   914461  ml/ex6.zip

//FEATURES:
// - http://stackoverflow.com/questions/1390592/java-check-if-file-is-already-open
// - split stamp to override and check (not sure why wanted this)
// - check - make more intelligent diff which collapses branches
// - combine buildTree and scanTree, rename/refactor
// - test on SNAPSHOTS!
// - deep and skip check (no sha1)
// - list of duplicated non-empty files!

//FIXES:
// - common ".neater"
// - report soft links
// - ...neater if . is an argument
// - check if compare sensitive to timestamp

//OTHER:
//ssh
//thunderbird email
//github repos
//other libraries
//list of apps and downloads
//soft links
//bookmarks
//permissions


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

case class File(name: String, hash: String, modified: Long, length: Long, path: List[String]) extends Elem {
  override def toString() = "File:" + (name, path, modified, hash.substring(0, 4))
}

case class Directory(name: String, files: Set[File], dirs: Set[Directory], hash: String, subhash: Bag[String], modified: Long, path: List[String], level: Int) extends Elem {
  override def toString() = {
    val fs = files.map(f => "\t" * (level + 1) + f).mkString("\n")
    val ds = dirs.mkString("\n")
    "\t" * level + "Directory:" +
      (level, name, path, modified, hash.substring(0, 4), subhash.size, subhash.map(_.substring(0, 4))) +
      "\n" + fs + "\n" + ds
  }
}

case class DiffDirectory(name: String, dirs: Set[DiffDirectory], files: Set[File], path: List[String], identical: Boolean) {
  def isEmpty = dirs.isEmpty && files.isEmpty
  override def toString() = {
    val fs = files.map(f => "\t" * (path.size + 1) + f).mkString("\n")
    val fss = (if (!identical) fs else "\t" * (path.size + 1) + files.size + " file(s) in dir are identical")
    val ds = dirs.mkString("\n")
    val dss = if (identical) "" else ds
    "\t" * path.size + "DiffDirectory:" +
      (name, path, if (identical) "IDENTICAL" else "...", files.size, dirs.size) +
      "\n" + fss + "\n" + dss
  }
}

case class SimpleFile(name: String, hash: String, modified: Long, length: Long) {
  override def toString() = "SimpleFile:" + (name, modified, hash.substring(0, 4))
}

case class SimpleDirectory(name: String, dirs: Set[SimpleDirectory], files: Set[SimpleFile]) {
  def isEmpty = dirs.isEmpty && files.isEmpty
  override def toString() = {
    val fs = files.mkString("\n")
    val ds = dirs.mkString("\n")
    "SimpleDirectory:" + (name, files.size, dirs.size) + "\n" + fs + "\n" + ds
  }
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

  def getNeater(root: java.io.File): JFile = new JFile(root, ".neater")

  def readNeater(neater: java.io.File): List[String] = scala.io.Source.fromFile(neater).getLines.toList

  def formatNeater(l: List[List[String]]): List[String] = l.map(_.mkString("\t")).sorted

  def parseNeaterLine(l: String) = {
    val List(hash, modified, length, full_path) = l.split('\t').toList
    val name :: path = full_path.split('/').toList.reverse
    (hash, modified.toLong, length.toLong, path.reverse, name)
  }

  def saveNeater(neater: java.io.File, l: List[List[String]]) {
    val p = new java.io.PrintWriter(neater)
    try { formatNeater(l).foreach(p.println(_)) }
    finally { p.close }
  }

  def scanTree(f: JFile): List[List[String]] = {
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
}

trait FuncsDeep extends Funcs {
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
      val dirs = these.filter(_.isDirectory).map(d => { helper(d, d.getName :: path, l + 1) }).toSet
      val (p1, p2) = hashStuff(dirs, files)
      Directory(root.getName, files, dirs, p1, p2, if (dir_ts) root.lastModified else 0, path, l)
    }
    helper(root, List(), 0)
  }

  type FileLine = (String, Long, Long, List[String], String) //hash, modified, length, path, name
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

  def diff1(d1: Directory, d2: Directory) = {
    val files = tree2Flat(d2);
    def disass(d: Directory): Option[DiffDirectory] = {
      val delta = d.files -- files //use Bag instead of set - might not work with multiple copies
      val tmp = d.dirs.map(disass).flatten
      val dd = DiffDirectory(d.name, tmp, delta, d.path, tmp.forall(_.identical) && d.files == delta)
      if (dd.isEmpty) None else Option(dd)
    }
    disass(d1)
  }

  def toSimpleFile(f: File): SimpleFile = SimpleFile(f.name, f.hash, f.modified, f.length)
  def toSimpleDirectory(d: Directory): SimpleDirectory = SimpleDirectory(d.name, d.dirs.map(toSimpleDirectory), d.files.map(toSimpleFile))

  def dirSub(from: SimpleDirectory, that: SimpleDirectory): (Boolean, SimpleDirectory) = {
    if (from.dirs.contains(that))
      (true, SimpleDirectory(from.name, from.dirs - that, from.files))
    else
      (false, from)
  }

  def diff(d1: Directory, d2: Directory) = {
    val sd1 = toSimpleDirectory(d1)
    val sd2 = toSimpleDirectory(d2)
    
    Some(d1.dirs)
  }

  /*
val d=".../copy/managed.mac"
:power
vals.isettings.maxPrintString = 0
import scripts._;object A extends FuncsDeep;import A._
val t1=flat2Tree(readNeater(getNeater(new java.io.File(d))).map(parseNeaterLine))
val t2=buildTree(new java.io.File(d))
diff(t1,t2)
 */
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

object DeepCheck extends FuncsDeep {
  def main(args: Array[String]) {
    val root = new JFile(args(0))
    assert(root.isDirectory)
    val neater = getNeater(root)
    if (neater.exists) {
      println(" ...deep checking neater")

      //val t1=flat2Tree(readNeater(getNeater(new java.io.File(root))).map(parseNeaterLine))
      //val t2=buildTree(new java.io.File(d))
      //diff(t1,t2)

      val tree = buildTree(root)
      val tree_neater = flat2Tree(readNeater(neater).map(parseNeaterLine))

      val added = diff(tree, tree_neater)
      val deleted = diff(tree_neater, tree)
      if (added.isDefined) {
        println("added:")
        println(added.get)
      }
      if (deleted.isDefined) {
        println("deleted:")
        println(deleted.get)
      }
    } else {
      println(" ...this directory needs to be stamped first.")
    }
  }
}

object DeepCompare extends FuncsDeep {
  def main(args: Array[String]) {
    val root1 = new JFile(args(0))
    val root2 = new JFile(args(1))
    assert(root1.isDirectory)
    assert(root2.isDirectory)
    val tree1 = buildTree(root1)
    val tree2 = buildTree(root2)

    val added = diff(tree1, tree2)
    val deleted = diff(tree2, tree1)
    if (added.isDefined) {
      println("only in first:")
      println(added.get)
    }
    if (deleted.isDefined) {
      println("only in second:")
      println(deleted.get)
    }
  }
}

object Analyze extends FuncsDeep {
  def isPartOf[T1 <: Elem, T2 <: Elem](d: T1, dd: T2) = dd.path.reverse == d.path.reverse.take(dd.path.length)

  def isPartOfAny[T1 <: Elem, T2 <: Elem](d: T1, ds: Set[T2]) = ds.find(isPartOf(d, _)).isDefined

  def isAnyPartOfAny[T1 <: Elem, T2 <: Elem](d: T1, dss: Set[Set[T2]]) = dss.find(isPartOfAny(d, _)).isDefined

  def flatFiles(d: Directory): Set[File] = d.files ++ d.dirs.flatMap(flatFiles)

  def flatDirectories(d: Directory): Set[Directory] = d.dirs ++ d.dirs.flatMap(flatDirectories)

  def similarBags[T](b1: Bag[T], b2: Bag[T]) = {
    val THRESH = 32
    val b1s = b1.size
    val b2s = b2.size
    val dd=(b1s - b2s).toDouble
    val ddd=dd.abs//TODO why implicit?
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
