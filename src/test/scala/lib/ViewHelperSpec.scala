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

  it should "input type=text" in {
    ViewHelper.text("<foo", "<bar", "class='class1'") should be("""<input type="text" name="&lt;foo" value="&lt;bar" class='class1' />""")
  }

  it should "input type=password" in {
    ViewHelper.password("<foo", "<bar", "class='class1'") should be("""<input type="password" name="&lt;foo" value="&lt;bar" class='class1' />""")
  }

  it should "textarea" in {
    val value = """foo
              |bar""".stripMargin
    ViewHelper.textarea("<foo", value, "class='foo'", "placeholder='bar'") should be(s"""<textarea name="&lt;foo" class='foo' placeholder='bar'>$value</textarea>""".stripMargin)
  }

}
