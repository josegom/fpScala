package chapter2

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jose on 13/02/2016.
  */
@RunWith(classOf[JUnitRunner])
class OrderedTest extends FlatSpec with Matchers {


  val orderedList = List (Array(1,2,3,4), Array(1,1,1,1,1), Array(1,5,7,8,11), Array(1,2,2,4,6,7,7), Array(1))
  val disorganizedList = List (Array(2,1), Array(2,2,2,1), Array(1,2,3,4,1), Array(7,1,2,3,4))

  "A ordered array" should "return true" in {
    for (orderedArray <- orderedList)
      Ordered(orderedArray, (x: Int, y: Int) => x <= y) should be(true)
  }


  "A disorganized array" should "return false" in {
    for (disorganizedArray <- disorganizedList)
      Ordered(disorganizedArray, (x: Int, y: Int) => x <= y) should be(false)
  }


}
