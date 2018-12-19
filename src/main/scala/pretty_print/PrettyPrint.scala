package pretty_print

sealed abstract class Doc {
  def <>(that: Doc): Doc = new <>(this, that)
  implicit def stringToDoc(s: String): Doc = Text(s)
}
case class Text(s: String) extends Doc
case class LB() extends Doc
case class <>(d1: Doc, d2: Doc) extends Doc
case class Group(d: Doc) extends Doc
case class Nest(in: Int, d: Doc) extends Doc


object Doc {
  private type Width = Int
  private type WidthLeft = Int
  private type PageWidth = Int
  private type Fit = Boolean

  def prettyPrint(w: PageWidth, d: Doc): String = {
    def format(f: Fit, r: WidthLeft, i: Int): Doc => (String, WidthLeft) = {
      case Text(s) => (s, r - s.length)
      case LB() => if (f) (" ", r - 1) else ("\n" + (" " * i), w - i)
      case d1 <> d2 =>
        val (s1, r1) = format(f, r, i)(d1)
        val (s2, r2) = format(f, r1, i)(d2)
        (s1 + s2, r2)
      case Group(d) =>
        val fit = f || (width(d) <= r)
        format(fit, r, i)(d)
      case Nest(in, d) => format(f, r, i + in)(d)
    }

    def width: Doc => Width = {
      case Text(s) => s.length
      case LB() => 1
      case d1 <> d2 => width(d1) + width(d2)
      case Group(d) => width(d)
      case Nest(in, d) => width(d)
    }

    format(false, w, 0)(d)._1
  }
}

object PrettyPrint {
  def main(args: Array[String]): Unit = {
    val doc = Text("foo(") <> Group(
      Nest(2, LB() <>
        Text("arg1") <>
        LB() <>
        Text("arg2") <>
        LB() <>
        Text("arg3")) <> LB() <> Text(")"))

    println(Doc.prettyPrint(5, doc))
  }
}
