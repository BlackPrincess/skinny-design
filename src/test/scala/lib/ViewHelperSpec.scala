package lib

import org.scalatra.test.scalatest._
import skinny.test.SkinnyTestSupport

class ViewHelperSpec extends ScalatraFlatSpec with SkinnyTestSupport {
  it should "line-break to <br>" in {
    MultiLineText(
      """one
        |two
        |three""".stripMargin).toString should be("one<br>two<br>three")
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
    Text("<foo", "<bar", 'class -> "class1").toString should be("""<input type="text" name="&lt;foo" value="&lt;bar" class="class1" />""")
  }

  it should "input type=password" in {
    Password("<foo", "<bar", 'class -> "class1").toString should be("""<input type="password" name="&lt;foo" value="&lt;bar" class="class1" />""")
  }

  it should "textarea" in {
    val value = """foo
              |bar""".stripMargin
    TextArea("<foo", value, 'class -> "foo", 'placeholder -> "bar").toString should 
      be(s"""<textarea name="&lt;foo" class="foo" placeholder="bar">$value</textarea>""".stripMargin)
  }
  
  
  it should "checkbox" in {
    Checkbox("foo", "value1").toString should be("""<input type="checkbox" name="foo" value="value1" />""")
    Checkbox("foo", "value2", true).toString should be("<input type=\"checkbox\" name=\"foo\" value=\"value2\" checked=\"checked\" />")
  }
  
  it should "radio" in {
    Radio("foo", "value1").toString should be("""<input type="radio" name="foo" value="value1" />""")
    Radio("foo", "value2", true).toString should be("<input type=\"radio\" name=\"foo\" value=\"value2\" checked=\"checked\" />")
  }
}
