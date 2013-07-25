/**
 * Copyright by <a href="http://crazyadam.net"><em><i>Joseph J.C. Tang</i></em></a> <br/>
 * Email: <a href="mailto:jinntrance@gmail.com">jinntrance@gmail.com</a>
 * @author joseph
 * @since 7/4/13 9:53 PM
 * @version 1.0
 */

import io.Source
import scala.util.Random

object RandomContract{
  val size=200
  def main(args: Array[String]) {
    val m=Array.ofDim[Int](size,size)
    val ls=
      for{l<-Source.fromFile(args(0)).getLines()
          list=l.split(' ');key=list.head.toInt-1
          v<-list.tail
      } m(key)(v.toInt-1)=1
    val r=new Random(System.currentTimeMillis())
    for(i<- 0 until size-1) contract(r, m)
    for(i<- 0 until size if m(i)(i)>0) println(m(i)(i))
  }


  def contract(r: Random, m: Array[Array[Int]]) {
    var i, j = 0
    do {
      i = r.nextInt(size)
      j = r.nextInt(size)
    } while (0 == m(i)(j) && i == j)

    for (k <- 0 until size) {
      m(k)(k) = 0
      m(i)(k) = m(i)(k) + m(j)(k)
      m(j)(k) = 0
      m(k)(i) = m(k)(i) + m(k)(j)
      m(k)(j)
    }
  }
}