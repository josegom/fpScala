package chapter2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jose on 13/02/2016.
  */
@RunWith(classOf[JUnitRunner])
class FibonnaciTest extends FlatSpec with Matchers {

  val SFibonnaci = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811)
  "Fibonacci method" should "return the fibonnaci number" in {

    for (i <- 0 to SFibonnaci.length-1)
      Fibonnaci(i) should be (SFibonnaci(i))
  }


}
