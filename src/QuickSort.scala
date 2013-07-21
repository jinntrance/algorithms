import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.io.Source
/**
 * Copyright by <a href="http://crazyadam.net"><em><i>Joseph J.C. Tang</i></em></a> <br/>
 * Email: <a href="mailto:jinntrance@gmail.com">jinntrance@gmail.com</a>
 * @author joseph
 * @since 7/20/13 11:13 AM
 * @version 1.0
 */


object QuickSort{
  var a:mutable.Buffer[Int]=null
  var ia:List[Int]=null
  def main(args: Array[String]) {
    ia=Source.fromFile(args(0)).getLines().map(_.toInt).toList
    a=ia.toBuffer
    val end=a.length-1
    println(headQuickSort(0,end))
    println(tailQuickSort(0,end))
    println(mediaQuickSort(0,end))
  }

  /**
   * swap the pivot to the middle after the partition
   */
  def swap(i:Int,j:Int){
    val med=a(i)
    a(i)=a(j)
    a(j)=med
  }
  def partition(begin:Int,end:Int)={
    val pivot=a(begin)
    var i=begin+1
    for(j<- i to end) {
      if(pivot>a(j)) {
        swap(i,j)
        i+=1
      }
    }
    swap(begin,i-1)
    i-1
  }

def quickSort(begin:Int,end:Int,p: (Int,Int)=>Int=partition):Long=if(end-begin<=0) 0 else{
  val count: Int = end-begin
  val splitIndex=p(begin,end)
  count+quickSort(begin,splitIndex-1,p)+ quickSort(splitIndex+1,end,p)
}
  
 def headQuickSort(begin:Int,end:Int):Long= {
   quickSort(begin,end)
 }

  def tailQuickSort(begin:Int,end:Int):Long= {
    def pt(begin:Int,end:Int)={
      swap(begin,end)
      partition(begin,end)
    }
    a=ia.toBuffer
    quickSort(begin,end,pt)
  }

  def mediaQuickSort(begin:Int,end:Int):Long={
    a=ia.toBuffer
    def pt(begin:Int,end:Int)={
      val remainLen: Int = end-begin
      val FIRST=a(begin)
      val LAST=a(end)
      val MEDIAN=a(begin+remainLen/2)
      val sorted=Set(FIRST,LAST,MEDIAN).toBuffer.sorted
      sorted((sorted.length-1)/2) match {
        case LAST=>swap(begin,end)
        case MEDIAN=>swap(begin,begin+remainLen/2)
        case FIRST=>
      }
      partition(begin,end)
    }
    quickSort(begin,end,pt)
  }



}