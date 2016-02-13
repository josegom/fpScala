package chapter2

/**
  * Created by jose on 13/02/2016.
  */
object Fibonnaci {



   def apply(n :Int) : Int = {
     @annotation.tailrec
    def go(a: Int, pprev: Int, prev: Int): Int ={
       if (a >= n-1) prev
      else go (a+1,prev,pprev+prev)
    }
     if (n == 0) 0
     else if (n ==1 || n== 2) 1
     else go(1,1,1)
  }

}
