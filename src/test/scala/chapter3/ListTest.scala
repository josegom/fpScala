package chapter3

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jose on 13/02/2016.
  */
@RunWith(classOf[JUnitRunner])
class ListTest extends FlatSpec with Matchers {


  val orderedList = List (Array(1,2,3,4), Array(1,1,1,1,1), Array(1,5,7,8,11), Array(1,2,2,4,6,7,7), Array(1))
  val disorganizedList = List (Array(2,1), Array(2,2,2,1), Array(1,2,3,4,1), Array(7,1,2,3,4))

  "I " should "wish a match guess" in {

    val x = List(1,2,3,4,5) match {
      case Cons(x,Cons(2, Cons(4,_))) => x
      case Nil => 42
      case Cons(x,Cons(y,Cons(3,Cons(4,_)))) => x + y
      case Cons(h,t) => h + sum(t)
      case _ => 101
    }

    x should be(3)
  }

  "A tail in nil List" should "return nil" in {
    List.tail(Nil) should be(Nil)
  }

  "one member List tail " should "return nil" in {
    List.tail(List("x")) should be(Nil)
  }

  "List tail" should "return tail" in {
    List.tail(List("x","y","z")) should be(List("y","z"))
  }


  "A head in a nil List" should "return the element" in {
    List.setHead(Nil,1) should be(List(1))
  }

  "A head in a List" should "return the list with a new head" in {
    List.setHead(List(1),0) should be(List(0,1))
  }

  "A head in a List with nil" should "return the same list" in {
    val list= List(1)
    List.setHead(list,Nil) should be(list)
  }

  "A drop 2 " should "return a list without 2 elements" in {
    List.drop(List(1,2,3,4,5),2) should be (List(3,4,5))
  }

  "A drop while" should "drop while the condition is true" in {
    List.dropWhile(List(1,1,1,2,1,1), (x: Int)=>x==1) should be(List(2,1,1))
  }

  "Wit init" should "create a list without the last element" in {
    List.init(List(1,2,3,4,5,6)) should be(List(1,2,3,4,5))
  }

  "List leghth" should "be correct" in {
    List.length(List(1,2,3,4,5,6,7,8,9,10)) should be(10)
  }


  "Sum list wit foldLEft" should "be correct" in {
    List.sum3(List(1,2,2,3,0)) should be(8)
  }

  "product list wit foldLEft" should "be correct" in {
    List.product3(List(1,2,2,3)) should be(12)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }



}
