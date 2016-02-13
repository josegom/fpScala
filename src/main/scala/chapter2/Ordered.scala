package chapter2

/**
  * Exercise 2.2
  */
object Ordered {

  def apply[A] (as: Array[A], ordered: (A,A) => Boolean) : Boolean ={
    @annotation.tailrec
    def go (pos: Int): Boolean ={

     if (pos == as.length - 1 ) true
     else if (ordered(as(pos),as(pos+1)))
       go(pos + 1)
     else false
   }
    go (0)
  }

}
