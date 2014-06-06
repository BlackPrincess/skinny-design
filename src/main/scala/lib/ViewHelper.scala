package lib

import org.fusesource.scalate.support.RenderHelper

/**
 * note:
 */
object ViewHelper {

  /**
   * attr should be (Symbol, String)
   */
  private def attributeString(attr: Seq[String]) = attr.foldLeft("") {_ + " " + _}

  private def s(value: String) = RenderHelper.sanitize(value)

  def options(data: Map[String, String], selected: String, attr: String*) : String = {
    (for { (k, v) <- data } yield
      s"""<option value="${s(k)}"${if (selected == k) """ selected="selected"""" else ""}${attributeString(attr)}>${s(v)}</option>"""
      ).reduceLeftOption{ _ + "\n" + _ }.getOrElse("")
  }

  def radios(name: String, data: Map[String, String], selected: String, attr: String*) : String = {
    /**
     * under consideration.
     */
    (for { (k, v) <- data} yield "<label>" + radio(name, k, k == selected, attributeString(attr))  + v + "</label>"
    ).reduceLeftOption { _ + "\n" + _ }.getOrElse("")
  }

  def checkboxes(name: String, data: Map[String, String], selected: Set[String], attr: String*) : String = {
    /**
     * under consideration.
     */
    (for { (k, v) <- data} yield "<label>" + checkbox(name, k, selected.contains(k), attributeString(attr)) + v + "</label>"
    ).reduceLeftOption { _ + "\n" + _ }.getOrElse("")
  }

  def input(typ: String, name: String, value: String, attr: Seq[String]) : String = {
    s"""<input type="${s(typ)}" name="${s(name)}" value="${s(value)}"${attributeString(attr)} />"""
  }

  def text(name: String, value: String, attr: String*) : String = {
    input("text", name, value, attr)
  }

  def password(name:String, value: String, attr: String*) : String = {
    input("password", name, value, attr)
  }

  def radio(name: String, value: String, checked: Boolean, attr: String*) : String = {
    s"""<input type="radio" name="${s(name)}" value="${s(value)}"${if(checked) """ checked="checked"""" else ""}${attributeString(attr)} />"""
  }

  def checkbox(name: String, value: String, checked: Boolean, attr: String*) : String = {
    s"""<input type="checkbox" name="${s(name)}" value="${s(value)}"${if(checked) """ checked="checked"""" else ""}${attributeString(attr)} />"""
  }

  def textarea(name:String, value: String, attr: String*) : String = {
    s"""<textarea name="${s(name)}"${attributeString(attr)}>${s(value)}</textarea>"""
  }

  def newlineToBr(s: String) : String = {
    s.replaceAll("\r\n", "<br>").replaceAll("\r", "<br>").replaceAll("\n", "<br>")
  }
}