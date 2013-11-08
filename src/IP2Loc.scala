import java.io.{PrintWriter, File}
import scala.io.{Codec, Source}

/**
 * Copyright by <a href="http://crazyadam.net"><em><i>Joseph J.C. Tang</i></em></a> <br/>
 * Email: <a href="mailto:jinntrance@gmail.com">jinntrance@gmail.com</a>
 * @author joseph
 * @since 11/8/13 4:24 PM
 * @version 1.0
 */
object IP2Loc {
  def ip2long(ip:String)=ip.split('.').foldLeft(0l){case (a,i)=>(a<<8) + i.toInt}


  def merge(s1:(Long,Long),s2:(Long,Long)):List[(Long,Long)]={
    def mergeHelper(s1:(Long,Long),s2:(Long,Long))= if(s1._2+1>=s2._1) List((s1._1,s2._2)) else List(s2,s1)
    if(s1._1<=s2._1) mergeHelper(s1,s2) else mergeHelper(s2,s1)
  }

  def main(args: Array[String]) {
    implicit val codec = Codec.UTF8
    if(args.size<2) println("Usage: IP2Loc source destination")
    else {
      val writer=new PrintWriter(args(1))
      Source.fromFile(args(0)).getLines().toStream.foldLeft(List[((Long,Long),String)]())((a,e) => {
        val Array(start, end, loc) = e.split(',')
        val number=(ip2long(start),ip2long(end))
        a match {
          case h::tail =>{
            if(loc == h._2)
              merge(h._1, number).map((_, loc)) ::: tail
            else {
              a.reverse.foreach(e=>writer.write(s"${e._1._1},${e._1._2},${e._2}\n"))
              writer.flush()
              (number,loc)::Nil
            }
          }
          case _=>{
            (number,loc)::a
          }
        }
      }).reverse.foreach(e=>writer.write(s"${e._1._1},${e._1._2},${e._2}\n"))
    }

  }
}
