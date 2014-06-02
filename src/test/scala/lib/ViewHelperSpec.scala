package lib

import org.scalatra.test.scalatest._
import skinny.test.SkinnyTestSupport

class ViewHelperSpec extends ScalatraFlatSpec with SkinnyTestSupport {
  it should "line-break to <br>" in {
    ViewHelper.newlineToBr(
      """one
        |two
        |three""".stripMargin) should be("one<br>two<br>three")
  }

  it should "put option tags" in {
    ViewHelper.options(Map(
      "value1" -> "key1",
      "value2" -> "key2",
      "value3" -> "key3"
    ), "value2") should be(
      """<option value="value1">key1</option>
        |<option value="value2" selected="selected">key2</option>
        |<option value="value3">key3</option>""".stripMargin)
  }

  it should "escape" in {
    ViewHelper.options(Map("<" -> ">"), "") should be("""<option value="&lt;">&gt;</option>""".stripMargin)
  }

}
