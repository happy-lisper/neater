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
package scripts

import edu.washington.cs.knowitall.collection.immutable.Bag
import java.io.{File=>JFile}
import java.security.DigestInputStream

//list of duplicated non-empty files!

abstract class Tree //checkme: is it okay to val over def ?
{
  def name:String
  def hash:String
  def modified:Long
  def path:List[String]
}
case class File(name:String,hash:String,modified:Long,size:Long,path:List[String]) extends Tree
case class Directory(name:String,files:Set[File],dirs:Set[Directory],hash:String,subhash:Bag[String],modified:Long,path:List[String],level:Int) extends Tree
{
  override def toString()="Directory"+(level,name,path,files,dirs,modified,hash.substring(0,4),subhash.size,subhash.map(_.substring(0,4)))
}

class Funcs
{  
  def file_digest(f:JFile)=
  {
    val md=java.security.MessageDigest.getInstance("SHA-1")
    val is=new java.io.FileInputStream(f)
    val dis=new DigestInputStream(is,md)
    val buffer=new Array[Byte](1024*1024)
    Iterator.continually(dis.read(buffer)).takeWhile(_!= -1).foreach(x=>Unit)
    is.close
    dis.close
    md.digest.map(_&0xFF).map("%02x".format(_)).mkString
  }
  def isPartOf[T1<:Tree,T2<:Tree](d:T1,dd:T2)=dd.path.reverse==d.path.reverse.take(dd.path.length)
  def isPartOfAny[T1<:Tree,T2<:Tree](d:T1,ds:Set[T2])=ds.find(isPartOf(d,_)).isDefined
  def isAnyPartOfAny[T1<:Tree,T2<:Tree](d:T1,dss:Set[Set[T2]])=dss.find(isPartOfAny(d,_)).isDefined
  def flatFiles(d:Directory):Set[File]=d.files++d.dirs.flatMap(flatFiles)
  def flatDirectories(d:Directory):Set[Directory]=d.dirs++d.dirs.flatMap(flatDirectories)
  def isSoftLink(f:JFile)=
  {
    val canon=if(f.getParent==null)
      f
    else
      new JFile(f.getParentFile.getCanonicalFile,f.getName)
    !canon.getCanonicalFile.equals(canon.getAbsoluteFile)
  }
  def find_equal(similars:Set[Set[Directory]],files:Set[File])=
  {
    val potentially_unique_files=files.filter(!isAnyPartOfAny(_,similars))
    val grouped_files=potentially_unique_files.groupBy(_.hash).filter(_._2.size>1)
    val grouped_files2=grouped_files.toSet
    grouped_files2.map(_._2)
  }
  def listValidFiles(d:JFile)=
  {
    val lf=d.listFiles.filter(_.exists).filter(!isSoftLink(_)).filter(!_.getName.endsWith(".andysum"))
    if(lf.map(_.getName).contains(".git"))
      List()
    else
      lf.toList
  }
  def scanTree(f:JFile,p:List[String]):List[List[String]]=
  {
    if(f.exists && !isSoftLink(f))
    {
      if(f.isFile)
        List(List(file_digest(f),f.lastModified.toString,f.length.toString,p.reverse.mkString("/")))
      else
        listValidFiles(f).map(x=>scanTree(x,x.getName::p)).flatten
    }
    else
      List()
  }
  def scanTree2(f:JFile,p:List[String]):List[List[String]]=
  {
    if(f.exists && !isSoftLink(f))
    {
      if(f.isFile)
        List(List(file_digest(f),f.lastModified.toString,f.length.toString,p.reverse.mkString("/")))
      else
{
        val xx=listValidFiles(f).map(x=>scanTree2(x,x.getName::p))
	//println("###",xx)
        xx.flatten
}
    }
    else
      List()
  }
  def buildTree(f:JFile,p:List[String],l:Int):Directory=
  {
    val path=f.getName::p
    val these=listValidFiles(f)
    val files=these.filter(_.isFile).map(f=>File(f.getName,file_digest(f),f.lastModified,f.length,f.getName::path))
    val dirs=these.filter(_.isDirectory).map(d=>{buildTree(d,path,l+1)})
    val md=java.security.MessageDigest.getInstance("SHA-1")
    val d=md.digest(files.map(_.hash).sorted.mkString.getBytes).map(_&0xFF).map("%02x".format(_)).mkString
    val sh=Bag.from(dirs.map(_.subhash).flatten).merge(Bag.from(files.map(_.hash)))
    //val sh=files.map(_.hash)++dirs.map(_.subhash).flatten
    Directory(f.getName,files.toSet,dirs.toSet,d,sh,f.lastModified,path,l)
  }
}

object Stamp extends Funcs
{
  def main(args: Array[String])
  {
    val root=new JFile(args(0))
    assert(root.isDirectory)

    val andysum=new JFile(root.getParentFile,"."+root.getName+".andysum")
    //val andysum=new JFile(root,".andysum")
    if(andysum.exists)
    {
      println(" ... this directory is already stampled.")
    }
    else
    {
      println(" ...creating andysum")
      val all_files=scanTree2(root,List(root.getName)).map(_.mkString("\t")).sorted
      all_files.foreach(println)
      //val t=buildTree(root,List(),0)
      //println(t)
      val p=new java.io.PrintWriter(andysum)
      try {all_files.foreach(p.println(_))}
      finally {p.close}
    }
  }  
}

object Check extends Funcs
{
  def main(args: Array[String])
  {
    val root=new JFile(args(0))
    assert(root.isDirectory)

    val andysum=new JFile(root.getParentFile,"."+root.getName+".andysum")
    //val andysum=new JFile(root,".andysum")
    if(andysum.exists)
    {
      println(" ...checking andysum")
      val all_files=scanTree(root,List(root.getName)).map(_.mkString("\t")).sorted
      val all_files_andysum=scala.io.Source.fromFile(andysum).getLines.toList
      val set_files_andysum=all_files_andysum.toSet
      val set_files=all_files.toSet
      val added=set_files--set_files_andysum
      val deleted=set_files_andysum--set_files
      if(added.size>0)
      {
        println("added:")
        added.foreach(println)
      }
      if(deleted.size>0)
      {
        println("deleted:")
        deleted.foreach(println)
      }
    }
    else
    {
      println(" ...this directory needs to be stamped first.")
    }
  }  
}

object Compare extends Funcs
{
  def main(args: Array[String])
  {
    val root1=new JFile(args(0))
    val root2=new JFile(args(1))
    assert(root1.isDirectory)
    assert(root2.isDirectory)
    val all_files1=scanTree(root1,List())
    val grp_files1=all_files1.groupBy(_(0)).mapValues(_.map(_.mkString(",")).mkString("; "))
    val all_files2=scanTree(root2,List())
    val grp_files2=all_files2.groupBy(_(0)).mapValues(_.map(_.mkString(",")).mkString("; "))

    {
      val set_files1=all_files1.map(_(0)).toSet
      val set_files2=all_files2.map(_(0)).toSet
      val extra=set_files2--set_files1
      val missing=set_files1--set_files2
      if(extra.size>0)
      {
        println("extra:")
        extra.foreach(f=>println(grp_files2(f)))
      }
      if(missing.size>0)
      {
        println("missing:")
        missing.foreach(f=>println(grp_files1(f)))
      }
    }
    {
      val set_files1=all_files1.map(x=>(x(0),x(1),x(3))).toSet
      val set_files2=all_files2.map(x=>(x(0),x(1),x(3))).toSet
      val extra=set_files2--set_files1
      val missing=set_files1--set_files2
      val all=extra++missing
      val all2=all.groupBy(x=>(x._1,x._3)).filter(_._2.size>1)
      println("touched:")
      all2.keys.foreach(f=>println(f._2+"->"+f._1))
    }
  }  
}

object Analyze extends Funcs
{
  def similarBags[T](b1:Bag[T],b2:Bag[T])=
  {
    val THRESH=32
    val b1s=b1.size
    val b2s=b2.size
    if(b1s>THRESH && b2s>THRESH)
    {
      if((b1s-b2s).toDouble.abs/(b1s+b2s)>0.03)
        false
      else
      {
        val s1=b1.toSet
        val s2=b2.toSet
        val d=(s1--s2)++(s2--s1)
        d.size.toDouble/(s1.size+s2.size)<0.2
      }
    }
    else
      b1==b2
  }  
  def find_similars(i:List[Directory])=
  {
    def find_similar(s:Directory,i:List[Directory])=
    {
      var similar=Set[Directory](s)
      def next(s:Directory,i:List[Directory]):Unit=
      {
        if(i!=Nil)
        {
//          if(s.subhash==i.head.subhash)
          if(similarBags(s.subhash,i.head.subhash))
            if(!isPartOfAny(i.head,similar))
              similar=similar+i.head
          next(s,i.tail)
        }
      }
      next(s,i)
      similar
    }      
     
    var similars=Set[Set[Directory]]()
    def next(s:Directory,i:List[Directory]):Unit=
    {
      if(i!=Nil)
      {
        if(!isAnyPartOfAny(s,similars))
        {
          val fs=find_similar(s,i)
          if(fs.size>1)
            similars=similars+fs
        }
        next(i.head,i.tail)          
      }
    }
    next(i.head,i.tail)
    similars
  }

  def main(args: Array[String])
  {
    val root=new JFile(args(0))
    assert(root.isDirectory)
    val t=buildTree(root,List(),0)
    println(".. built")
    val all_files=flatFiles(t)
    val all_dirs=flatDirectories(t).toList.sortBy(_.level)
    val sims=find_similars(all_dirs)
    println("-- similar dirs:")
    sims.foreach(f=>println("* "+f.map(_.path.reverse.mkString("/")).mkString(" ")))
    println("-- equal files:")
    val eqs=find_equal(sims,all_files)
    eqs.foreach(f=>println("* "+f.map(_.path.reverse.mkString("/")).mkString(" ")))
  }
}
