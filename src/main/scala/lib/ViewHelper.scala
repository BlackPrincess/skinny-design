package lib

import org.fusesource.scalate.support.RenderHelper
// import scala.language.experimental.macros

/**
 * note: this code is under consideration.
 * 
 */
trait ViewHelper { self =>
  
  // TODO:
  def options(data: Map[String, String], selected: String, attr: (Symbol, String)*) : String = 
    (for { (k, v) <- data } yield SelectOption(v, k, selected, attr.toMap).toString).reduceLeftOption{ _ + "\n" + _ }.getOrElse("")

  // TODO:
  def radios(name: String, data: Map[String, String], selected: String, attr: (Symbol, String)*) : String = {
    /**
     * under consideration.
     */
    (for { (k, v) <- data} yield "<label>" + Radio(name, k, selected, attr.toMap)  + v + "</label>"
    ).reduceLeftOption { _ + "\n" + _ }.getOrElse("")
  }

  // TODO:
  def checkboxes(name: String, data: Map[String, String], selected: Set[String], attr: (Symbol, String)*) : String = {
    /**
     * under consideration.
     */
    (for { (k, v) <- data} yield "<label>" + Checkbox(name, k, selected, attr.toMap) + v + "</label>"
    ).reduceLeftOption { _ + "\n" + _ }.getOrElse("")
  }
}

object ViewHelper extends ViewHelper

trait HtmlHelper {
  /**
   * returns sanitized value.
   */
  protected def s(value: String) = RenderHelper.sanitize(value)
}

/**
 * 
 */
trait TagCompanion extends HtmlHelper {
  protected def attributeString(attr: Iterable[String]) : String = attr.foldLeft("") {_ + " " + _}
  protected def attributeString(attr: Map[Symbol, String]) : String = attributeString(attr.map { kv => s"""${kv._1.name}="${kv._2}" """.trim })
}
trait HtmlTag extends HtmlHelper
trait FormTag extends HtmlTag

/*
 package tag
 */
case class MultiLineText(s: String) extends HtmlHelper {
  override def toString : String = s(s).replaceAll("\r\n", "<br>").replaceAll("\r", "<br>").replaceAll("\n", "<br>")
}

/*
 package tag.form
 */

class TextArea(name: String, value: String, attr: String) extends HtmlTag {
  override def toString : String = s"""<textarea name="${s(name)}"${attr}>${s(value)}</textarea>""".toString
}

object TextArea extends TagCompanion {
  def apply(name: String) = new TextArea(name, "", "")
  def apply(name: String, value: String) = new TextArea(name, value, "")
  def apply(name: String, value: String, attr: (Symbol, String)*) = new TextArea(name, value, attributeString(attr.toMap))
  def apply(name: String, value: String, attr: Iterable[String]) = new TextArea(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Map[Symbol, String]) = new TextArea(name, value, attributeString(attr))
}

class Input(typ: String, name: String, value: String, attr: String) extends FormTag {
  override def toString : String = s"""<input type="${s(typ)}" name="${s(name)}" value="${s(value)}"${attr} />""".toString
}

object Input extends TagCompanion {
  def apply(typ: String, name: String) = new Input(typ, name, "", "")
  def apply(typ: String, name: String, value: String) = new Input(typ, name, value, "")
  def apply(typ: String, name: String, value: String, attr: (Symbol, String)*) = new Input(typ, name, value, attributeString(attr.toMap))
  def apply(typ: String, name: String, value: String, attr: Iterable[String]) = new Input(typ, name, value, attributeString(attr))
  def apply(typ: String, name: String, value: String, attr: Map[Symbol, String]) = new Input(typ, name, value, attributeString(attr))
}

class Text(name:String, value: String, attr: String) extends Input("text", name, value, attr)

object Text extends TagCompanion {
  def apply(name: String) = new Text(name, "", "")
  def apply(name: String, value: String) = new Text(name, value, "")
  def apply(name: String, value: String, attr: (Symbol, String)*) = new Text(name, value, attributeString(attr.toMap))
  def apply(name: String, value: String, attr: Iterable[String]) = new Text(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Map[Symbol, String]) = new Text(name, value, attributeString(attr))
}

class Password(name:String, value: String, attr: String) extends Input("password", name, value, attr)

object Password extends TagCompanion {
  def apply(name: String) = new Password(name, "", "")
  def apply(name: String, value: String) = new Password(name, value, "")
  def apply(name: String, value: String, attr: (Symbol, String)*) = new Password(name, value, attributeString(attr.toMap))
  def apply(name: String, value: String, attr: Iterable[String]) = new Password(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Map[Symbol, String]) = new Password(name, value, attributeString(attr))
}

class Button(name:String, value: String, attr: String) extends Input("button", name, value, attr)

object Button extends TagCompanion {
  def apply(name: String) = new Password(name, "", "")
  def apply(name: String, value: String) = new Password(name, value, "")
  def apply(name: String, value: String, attr: (Symbol, String)*) = new Password(name, value, attributeString(attr.toMap))
  def apply(name: String, value: String, attr: Iterable[String]) = new Password(name, value, attributeString(attr))
  def apply(name: String, value: String, attr: Map[Symbol, String]) = new Password(name, value, attributeString(attr))
}

class Checkbox(name: String, value: String, checked: Boolean, attr: String) extends FormTag {
  override def toString : String = s"""<input type="checkbox" name="${s(name)}" value="${s(value)}"${if(checked) """ checked="checked"""" else ""}${attr} />""".toString
}

object Checkbox extends TagCompanion {
  def apply(name: String) = new Checkbox(name, "", false, "")
  def apply(name: String, value: String) = new Checkbox(name, value, false, "")
  def apply(name: String, value: String, checkedValues: Set[String]) = new Checkbox(name, value, checkedValues.contains(value), "")
  def apply(name: String, value: String, checkedValues: Set[String], attr: String) = new Checkbox(name, value, checkedValues.contains(value), attr)
  def apply(name: String, value: String, checkedValues: Set[String], attr: (Symbol, String)*) = new Checkbox(name, value, checkedValues.contains(value), attributeString(attr.toMap))
  def apply(name: String, value: String, checkedValues: Set[String], attr: Iterable[String]) = new Checkbox(name, value, checkedValues.contains(value), attributeString(attr))
  def apply(name: String, value: String, checkedValues: Set[String], attr: Map[Symbol, String]) = new Checkbox(name, value, checkedValues.contains(value), attributeString(attr))
  def apply(name: String, value: String, checked: Boolean) = new Checkbox(name, value, checked, "")
  def apply(name: String, value: String, checked: Boolean, attr: String) = new Checkbox(name, value, checked, attr)
  def apply(name: String, value: String, checked: Boolean, attr: (Symbol, String)*) = new Checkbox(name, value, checked, attributeString(attr.toMap))
  def apply(name: String, value: String, checked: Boolean, attr: Iterable[String]) = new Checkbox(name, value, checked, attributeString(attr))
  def apply(name: String, value: String, checked: Boolean, attr: Map[Symbol, String]) = new Checkbox(name, value, checked, attributeString(attr))
}

class Radio(name: String, value: String, checked: Boolean, attr: String) extends FormTag {
  override def toString : String = s"""<input type="radio" name="${s(name)}" value="${s(value)}"${if(checked) """ checked="checked"""" else ""}${attr} />""".toString
}

object Radio extends TagCompanion{
  def apply(name: String) = new Radio(name, "", false, "")
  def apply(name: String, value: String) = new Radio(name, value, false, "")
  def apply(name: String, value: String, checkedValue: String) = new Radio(name, value, value == checkedValue, "")
  def apply(name: String, value: String, checkedValue: String, attr: String) = new Radio(name, value, value == checkedValue, attr)
  def apply(name: String, value: String, checkedValue: String, attr: (Symbol, String)*) = new Radio(name, value, value == checkedValue, attributeString(attr.toMap))
  def apply(name: String, value: String, checkedValue: String, attr: Iterable[String]) = new Radio(name, value, value == checkedValue, attributeString(attr))
  def apply(name: String, value: String, checkedValue: String, attr: Map[Symbol, String]) = new Radio(name, value, value == checkedValue, attributeString(attr))
  def apply(name: String, value: String, checked: Boolean) = new Radio(name, value, checked, "")
  def apply(name: String, value: String, checked: Boolean, attr: String) = new Radio(name, value, checked, attr)
  def apply(name: String, value: String, checked: Boolean, attr: (Symbol, String)*) = new Radio(name, value, checked, attributeString(attr.toMap))
  def apply(name: String, value: String, checked: Boolean, attr: Iterable[String]) = new Radio(name, value, checked, attributeString(attr))
  def apply(name: String, value: String, checked: Boolean, attr: Map[Symbol, String]) = new Radio(name, value, checked, attributeString(attr))
}

class SelectOption(key: String, value: String, selected:Boolean, attr: String) extends FormTag {
  override def toString : String = s"""<option value="${s(value)}"${if (selected) """ selected="selected"""" else ""}${attr}>${s(key)}</option>""".toString
}

object SelectOption extends TagCompanion {
  def apply(key: String) = new SelectOption(key, "", false, "")
  def apply(key: String, value: String) = new SelectOption(key, value, false, "")
  def apply(key: String, value: String, selectedValue: String) = new SelectOption(key, value, value == selectedValue, "")
  def apply(key: String, value: String, selectedValue: String, attr: String) = new SelectOption(key, value, value == selectedValue, attr)
  def apply(key: String, value: String, selectedValue: String, attr: (Symbol, String)*) = new SelectOption(key, value, value == selectedValue, attributeString(attr.toMap))
  def apply(key: String, value: String, selectedValue: String, attr: Iterable[String]) = new SelectOption(key, value, value == selectedValue, attributeString(attr))
  def apply(key: String, value: String, selectedValue: String, attr: Map[Symbol, String]) = new SelectOption(key, value, value == selectedValue, attributeString(attr))
  def apply(key: String, value: String, selected: Boolean) = new SelectOption(key, value, selected, "")
  def apply(key: String, value: String, selected: Boolean, attr: String) = new SelectOption(key, value, selected, attr)
  def apply(key: String, value: String, selected: Boolean, attr: (Symbol, String)*) = new SelectOption(key, value, selected, attributeString(attr.toMap))
  def apply(key: String, value: String, selected: Boolean, attr: Iterable[String]) = new SelectOption(key, value, selected, attributeString(attr))
  def apply(key: String, value: String, selected: Boolean, attr: Map[Symbol, String]) = new SelectOption(key, value, selected, attributeString(attr))
}

/*
 Multi
*/

class Checkboxes(name: String, data: Map[String, String], selected: Set[String], attr: String) {
  override def toString : String = (for { (k, v) <- data} yield "<label>" + Checkbox(name, k, selected, attr) + v + "</label>").reduceLeftOption { _ + "\n" + _ }.getOrElse("")
}

object Checkboxes extends TagCompanion {
  def apply(name: String, data: Map[String, String]) = new Checkboxes(name, data, Set(), "")
  def apply(name: String, data: Map[String, String], selected: Set[String]) = new Checkboxes(name, data, selected, "")
  def apply(name: String, data: Map[String, String], selected: Set[String], attr: String) = new Checkboxes(name, data, selected, attr)
  def apply(name: String, data: Map[String, String], selected: Set[String], attr: (Symbol, String)*) = new Checkboxes(name, data, selected, attributeString(attr.toMap))
  def apply(name: String, data: Map[String, String], selected: Set[String], attr: Iterable[String]) = new Checkboxes(name, data, selected, attributeString(attr))
  def apply(name: String, data: Map[String, String], selected: Set[String], attr: Map[Symbol, String]) = new Checkboxes(name, data, selected, attributeString(attr))
}

class Radios(name: String, data: Map[String, String], selected: String, attr: String) {
  override def toString : String = (for { (k, v) <- data} yield "<label>" + Radio(name, k, selected, attr)  + v + "</label>").reduceLeftOption { _ + "\n" + _ }.getOrElse("")
}

object Radios extends TagCompanion {
  def apply(name: String, data: Map[String, String]) = new Radios(name, data, "", "")
  def apply(name: String, data: Map[String, String], selected: String) = new Radios(name, data, selected, "")
  def apply(name: String, data: Map[String, String], selected: String, attr: String) = new Radios(name, data, selected, attr)
  def apply(name: String, data: Map[String, String], selected: String, attr: (Symbol, String)*) = new Radios(name, data, selected, attributeString(attr.toMap))
  def apply(name: String, data: Map[String, String], selected: String, attr: Iterable[String]) = new Radios(name, data, selected, attributeString(attr))
  def apply(name: String, data: Map[String, String], selected: String, attr: Map[Symbol, String]) = new Radios(name, data, selected, attributeString(attr))
}

class SelectOptions(data: Map[String, String], selected: String, attr: String) {
  override def toString : String = (for { (k, v) <- data } yield SelectOption(v, k, selected, attr).toString).reduceLeftOption{ _ + "\n" + _ }.getOrElse("")
}

object SelectOptions extends TagCompanion {
  def apply(data: Map[String, String]) = new SelectOptions(data, "", "")
  def apply(data: Map[String, String], selected: String) = new SelectOptions(data, selected, "")
  def apply(data: Map[String, String], selected: String, attr: String) = new SelectOptions(data, selected, attr)
  def apply(data: Map[String, String], selected: String, attr: (Symbol, String)*) = new SelectOptions(data, selected, attributeString(attr.toMap))
  def apply(data: Map[String, String], selected: String, attr: Iterable[String]) = new SelectOptions(data, selected, attributeString(attr))
  def apply(data: Map[String, String], selected: String, attr: Map[Symbol, String]) = new SelectOptions(data, selected, attributeString(attr))
}