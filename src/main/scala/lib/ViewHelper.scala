package lib

object ViewHelper {
  def options(data: Map[String, String], selected : String, attr : String*) : String = {
    val attributeString = attr.foldLeft("") {_ + " " + _}
    (for { (k, v) <- data } yield
      s"<option value='$k' ${if (selected == k) "selected='selected'" else ""} $attributeString>$v</option>"
      ).foldLeft("") { _ + _ }
  }

  def newlineToBr(s: String) : String = {
    s.replaceAll("\r\n", "<br>").replaceAll("\r", "<br>").replaceAll("\n", "<br>")
  }
}