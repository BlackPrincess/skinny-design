package lib

import org.fusesource.scalate.support.RenderHelper

object ViewHelper {
  def options(data: Map[String, String], selected : String, attr : String*) : String = {
    val attributeString = attr.foldLeft("") {_ + " " + _}
    (for { (k, v) <- data } yield
      s"""<option value="${RenderHelper.sanitize(k)}"${if (selected == k) """ selected="selected"""" else ""}$attributeString>${RenderHelper.sanitize(v)}</option>"""
      ).reduceLeftOption{ _ + "\n" + _ }.getOrElse("")
  }

  def newlineToBr(s: String) : String = {
    s.replaceAll("\r\n", "<br>").replaceAll("\r", "<br>").replaceAll("\n", "<br>")
  }
}