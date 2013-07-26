/**
 * Copyright by <a href="http://crazyadam.net"><em><i>Joseph J.C. Tang</i></em></a> <br/>
 * Email: <a href="mailto:jinntrance@gmail.com">jinntrance@gmail.com</a>
 * @author joseph
 * @since 7/4/13 9:53 PM
 * @version 1.0
 */

import io.Source
import scala.util.Random

object RandomContract {
  val size = 200
  val loopTimes = 100000
  var minCut = Int.MaxValue

  def main(args: Array[String]) {
    val m = Array.ofDim[Int](size, size)

    for {l <- Source.fromFile(args(0)).getLines()
         list = l.split("\\s"); key = list.head.toInt - 1
         v <- list.tail
    } m(key)(v.toInt - 1) = 1

    for (i <- 1 to loopTimes) {
      print(s"executing $i/$loopTimes\r")
      minCut = loop(m.map(_.clone()))
    }
    println(s"minCut: $minCut")

  }

  def loop(m: Array[Array[Int]]) = {
    val r = new Random(System.currentTimeMillis())
    var index = 0
    for (i <- 1 to size - 2) index = contract(r, m)
    val cut = m(index).filter(e => e > 0 && e < minCut)
    if (0 < cut.size && cut(0) < minCut) cut(0) else minCut
  }


  def contract(r: Random, m: Array[Array[Int]]) = {
    var i, j = 0
    do {
      i = r.nextInt(size)
      j = r.nextInt(size)
    } while (0 >= m(i)(j))

    m(i)(j) = 0
    m(j)(i) = 0
    for (k <- 0 until size) {
      m(i)(k) = m(i)(k) + m(j)(k)
      m(k)(i) = m(k)(i) + m(k)(j)
      m(k)(j) = 0
      m(j)(k) = 0
    }
    i
  }
}