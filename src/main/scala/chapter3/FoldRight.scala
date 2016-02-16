package chapter3

/**
  * Created by jose on 14/02/2016.
  */
object FoldRight {


  def foldRight[A,B](as:List[A],z:B)(f: (A,B) => B) : B ={
  as match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }
  }

  def main (args : Array[String]) : Unit = {
    print(foldRight(List(1,2,3),Nil: List[Int])(Cons(_,_)))
  }

}
