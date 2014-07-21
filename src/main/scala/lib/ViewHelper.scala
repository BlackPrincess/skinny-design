package lib

import org.fusesource.scalate.support.RenderHelper

/**
 * note: this code is under consideration.
 */
object ViewHelper {
  
  // TODO:
  def options(data: Map[String, String], selected: String, attr: String*) : String = {
    (for { (k, v) <- data } yield
      SelectOption(v, k, k == selected, attr).toString
      ).reduceLeftOption{ _ + "\n" + _ }.getOrElse("")
  }

  // TODO:
  def radios(name: String, data: Map[String, String], selected: String, attr: String*) : String = {
    /**
     * under consideration.
     */
    (for { (k, v) <- data} yield "<label>" + Radio(name, k, k == selected, attr)  + v + "</label>"
    ).reduceLeftOption { _ + "\n" + _ }.getOrElse("")
  }

  // TODO:
  def checkboxes(name: String, data: Map[String, String], selected: Set[String], attr: String*) : String = {
    /**
     * under consideration.
     */
    (for { (k, v) <- data} yield "<label>" + Checkbox(name, k, selected.contains(k), attr) + v + "</label>"
    ).reduceLeftOption { _ + "\n" + _ }.getOrElse("")
  }

  def input(typ: String, name: String, value: String, attr: Seq[String]) : String = Input(typ, name, value, attr).toString

  def text(name: String, value: String, attr: String*) : String = Input("text", name, value, attr).toString

  def password(name:String, value: String, attr: String*) : String = Password(name, value, attr).toString

  def textarea(name:String, value: String, attr: String*) : String = TextArea(name, value, attr).toString

  def newlineToBr(s: String) : String = MultiLineText(s).toString
}

trait HtmlHelper {
  protected def attributeString(attr: Iterable[String]) : String = attr.foldLeft("") {_ + " " + _}
  protected def attributeString(attr: Map[Symbol, String]) : String = attributeString(attr.map { kv => s"""${kv._1}="${kv._2}""""" })

  /**
   * returns sanitized value.
   */
  protected def s(value: String) = RenderHelper.sanitize(value)
}

/**
 * 
 */
trait HtmlTag extends HtmlHelper

case class MultiLineText(s: String) extends HtmlTag {
  override def toString = s(s).replaceAll("\r\n", "<br>").replaceAll("\r", "<br>").replaceAll("\n", "<br>")
}

class TextArea(name: String, value: String, attr: String) extends HtmlTag {
  override def toString = s"""<textarea name="${s(name)}"${attr}>${s(value)}</textarea>"""
}

object TextArea extends HtmlHelper {
  def apply(name: String) = new TextArea(name, "", "")
  def apply(name: String, value: String) = new TextArea(name, value, "")
  def apply(name: String, value: String, attr: String*) = new TextArea(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Iterable[String]) = new TextArea(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Map[Symbol, String]) = new TextArea(name, value, attributeString(attr))
}

class Input(typ: String, name: String, value: String, attr: String) extends HtmlTag {
  override def toString = s"""<input type="${s(typ)}" name="${s(name)}" value="${s(value)}"${attr} />"""
}

object Input extends HtmlHelper {
  def apply(typ: String, name: String) = new Input(typ, name, "", "")
  def apply(typ: String, name: String, value: String) = new Input(typ, name, value, "")
  def apply(typ: String, name: String, value: String, attr: String*) = new Input(typ, name, value, attributeString(attr))
  def apply(typ: String, name: String, value: String, attr: Iterable[String]) = new Input(typ, name, value, attributeString(attr))
  def apply(typ: String, name: String, value: String, attr: Map[Symbol, String]) = new Input(typ, name, value, attributeString(attr))
}

class Text(name:String, value: String, attr: String) extends Input("text", name, value, attr)

object Text extends HtmlHelper {
  def apply(name: String) = new Text(name, "", "")
  def apply(name: String, value: String) = new Text(name, value, "")
  def apply(name: String, value: String, attr: String*) = new Text(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Iterable[String]) = new Text(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Map[Symbol, String]) = new Text(name, value, attributeString(attr))
}

class Password(name:String, value: String, attr: String) extends Input("password", name, value, attr)

object Password extends HtmlHelper {
  def apply(name: String) = new Password(name, "", "")
  def apply(name: String, value: String) = new Password(name, value, "")
  def apply(name: String, value: String, attr: String*) = new Password(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Iterable[String]) = new Password(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Map[Symbol, String]) = new Password(name, value, attributeString(attr))
}

class Checkbox(name: String, value: String, checked: Boolean, attr: String) extends HtmlTag {
  override def toString = s"""<input type="checkbox" name="${s(name)}" value="${s(value)}"${if(checked) """ checked="checked"""" else ""}${attr} />"""
}

object Checkbox extends HtmlHelper {
  def apply(name: String) = new Checkbox(name, "", false, "")
  def apply(name: String, value: String) = new Checkbox(name, value, false, "")
  def apply(name: String, value: String, checked: Boolean) = new Checkbox(name, value, checked, "")
  def apply(name: String, value: String, checked: Boolean, attr: String*) = new Checkbox(name, value, checked, attributeString(attr))
  def apply(name: String, value: String, checked: Boolean, attr: Iterable[String]) = new Checkbox(name, value, checked, attributeString(attr))
  def apply(name: String, value: String, checked: Boolean, attr: Map[Symbol, String]) = new Checkbox(name, value, checked, attributeString(attr))
}

class Radio(name: String, value: String, checked: Boolean, attr: String) extends HtmlTag {
  override def toString = s"""<input type="radio" name="${s(name)}" value="${s(value)}"${if(checked) """ checked="checked"""" else ""}${attr} />"""
}

object Radio extends HtmlHelper{
  def apply(name: String) = new Radio(name, "", false, "")
  def apply(name: String, value: String) = new Radio(name, value, false, "")
  def apply(name: String, value: String, checked: Boolean) = new Radio(name, value, checked, "")
  def apply(name: String, value: String, checked: Boolean, attr: String*) = new Radio(name, value, checked, attributeString(attr))
  def apply(name: String, value: String, checked: Boolean, attr: Iterable[String]) = new Radio(name, value, checked, attributeString(attr))
  def apply(name: String, value: String, checked: Boolean, attr: Map[Symbol, String]) = new Radio(name, value, checked, attributeString(attr))
}

class SelectOption(key: String, value: String, selected:Boolean, attr: String) extends HtmlTag {
  override def toString = s"""<option value="${s(value)}"${if (selected) """ selected="selected"""" else ""}${attr}>${s(key)}</option>"""
}

object SelectOption extends HtmlHelper {
  def apply(key: String) = new SelectOption(key, "", false, "")
  def apply(key: String, value: String) = new SelectOption(key, value, false, "")
  def apply(key: String, value: String, selected: Boolean) = new SelectOption(key, value, selected, "")
  def apply(key: String, value: String, selected: Boolean, attr: String*) = new SelectOption(key, value, selected, attributeString(attr))
  def apply(key: String, value: String, selected: Boolean, attr: Iterable[String]) = new SelectOption(key, value, selected, attributeString(attr))
  def apply(key: String, value: String, selected: Boolean, attr: Map[Symbol, String]) = new SelectOption(key, value, selected, attributeString(attr))
}