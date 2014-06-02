package lib

package integrationtest

import org.scalatra.test.scalatest._
import skinny.test.SkinnyTestSupport
import _root_.lib.ViewHelper

class ViewHelperSpec extends ScalatraFlatSpec with SkinnyTestSupport {
  it should "options" in {
    ViewHelper.newlineToBr(
      """one
        |two
        |three""".stripMargin) should be("one<br>two<br>three")
  }

}
