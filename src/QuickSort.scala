import scala.io.Source

/**
 * Copyright by <a href="http://crazyadam.net"><em><i>Joseph J.C. Tang</i></em></a> <br/>
 * Email: <a href="mailto:jinntrance@gmail.com">jinntrance@gmail.com</a>
 * @author joseph
 * @since 7/20/13 11:13 AM
 * @version 1.0
 */
val ls=Source.fromFile(args(0)).getLines().map(_.toInt).toArray
val start=System.currentTimeMillis()
println(headQuickSort(ls))
println(tailQuickSort(ls))
println(mediaQuickSort(ls))

def headQuickSort(a:Array[Int]):Long=if(a.length<=1) 0 else{
  val remainLen: Int = a.length - 1
  val pivot=a.head
  val (first,second)=a.tail.span(_<pivot)
  remainLen+headQuickSort(first)+headQuickSort(second)
}

def tailQuickSort(a:Array[Int]):Long=  if(a.length<=1) 0 else{
  val remainLen: Int = a.length - 1
  val pivot=a(remainLen)
  val (first,second)=a.slice(0,remainLen).span(_<pivot)
  remainLen+headQuickSort(first)+headQuickSort(second)
}
def mediaQuickSort(a:Array[Int]):Long=if(a.length<=1) 0 else{
  val remainLen: Int = a.length - 1
  val FIRST=a.head
  val LAST=a(remainLen)
  val MEDIAN=a(remainLen/2)
  val sorted=Set(FIRST,LAST,MEDIAN).toArray.sorted
  val (first,second)=sorted((sorted.length-1)/2) match {
    case FIRST=>a.slice(1,remainLen+1).span(_< FIRST)
    case LAST=>a.slice(0,remainLen).span(_< LAST)
    case MEDIAN=>(a.slice(0,remainLen/2),a.slice(remainLen/2+1,remainLen+1))
  }
  remainLen+headQuickSort(first)+headQuickSort(second)
}


