package chapter4

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jose on 13/02/2016.
  */
@RunWith(classOf[JUnitRunner])
class OptionTest extends FlatSpec with Matchers {




  "a option map " should "change the option" in {

    Some(4).map( x => x+1) should be(Some(5))
    Some(4).map( x => x.toString) should be(Some("4"))
    None.map(x=>x) should be(None)

  }

  " a option flatMap " should "I don knpw  " in{
    Some(4).flatMap(v => Some(v+1)) should be(Some(5))
    Some(4).flatMap(v => None) should be(None)
    val option: Option[Int] = None
    option.flatMap(v => Some(v+1)) should be(None)
  }

  "a get or else " should "return the value or other" in {
    Some(4).getOrElse(10) should be (4)
    None.getOrElse(10) should be (10)
  }

  "a orElse" should " I don't know" in {
    Some(4).orElse(None) should be(Some(4))
    None.orElse(None) should be(None)
    None.orElse(Some(7)) should be(Some(7))
  }

  "a filter" should "filter the value" in {
    Some(4).filter(x => x!=4) should be(None)
    Some(4).filter(x => x==4) should be(Some(4))
  }

  "a map2 " should "works fine" in {
    None.map2(Some(1),Some(true))((x,y) => x.toString + y.toString) should be (Some("1true"))
    None.map2(None,Some(true))((x,y) => x.toString + y.toString) should be (None)
    None.map2(None,None)((x,y) => x.toString + y.toString) should be (None)

  }
}
