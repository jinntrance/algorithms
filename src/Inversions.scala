/**
 * Copyright by <a href="http://crazyadam.net"><em><i>Joseph J.C. Tang</i></em></a> <br/>
 * Email: <a href="mailto:jinntrance@gmail.com">jinntrance@gmail.com</a>
 * @author joseph
 * @since 7/4/13 9:53 PM
 * @version 1.0
 */

import io.Source

object Inversions{
  def main(args: Array[String]) {
    val ls=Source.fromFile(args(0)).getLines().toStream.map(_.toInt)
    val s1=System.currentTimeMillis()

    println(count(ls.toArray))
  }

  def count(ls:Array[Int],answer:Long=0):Long={
    val len=ls.length
    if(1>=len) answer
    else {
      val (left,right)=ls.splitAt(len/2)
      count(left,count(right,countMerge(left,right,answer)))
    }
  }
  def countMerge(left:Array[Int],right:Array[Int],answer:Long):Long={
    val (l,llen)=(left.sorted,left.length)
    val (r,rlen)=(right.sorted,right.length)
    var i,j,m=0
    while(i<llen&&j<rlen){
      if(l(i) > r(j)){
        j+=1
        m+=llen-i
      }
      else i+=1
    }
    answer+m
  }

}