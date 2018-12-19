package Imperative
import language.{higherKinds, implicitConversions}

case class Ref[A](var contents: A) {
  def :=(a: A): A = {
    contents = a
    a
  }
  def unary_!(): A = contents
}

trait Code {
  type Rep[A]
  type StmtList

  def int(i: Int): Rep[Int]
  def bool(b: Boolean): Rep[Boolean]

  def add(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def mul(x: Rep[Int], y: Rep[Int]): Rep[Int]
  def leq(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def lt(x: Rep[Int], y: Rep[Int]): Rep[Boolean]
  def ifExpr[A](c: Rep[Boolean], cnsq: Rep[A], alt: Rep[A]): Rep[A]

  def array[A](a: IndexedSeq[Rep[A]]): Rep[IndexedSeq[A]]
  def arrayLen[A](a: Rep[IndexedSeq[A]]): Rep[Int]
  def arrayRef[A](a: Rep[IndexedSeq[A]], i: Rep[Int]): Rep[A]

  def none[A]: Rep[Option[A]]
  def some[A](e: Rep[A]): Rep[Option[A]]
  def matchOpt[A, B](o: Rep[Option[A]], noneCase: Rep[B], someCase: Rep[A] => Rep[B]): Rep[B]

  def rget[A](r: Rep[Ref[A]]): Rep[A]
  def rset[A](r: Rep[Ref[A]], a: Rep[A]): StmtList
  def fail[A]: Rep[A]

  implicit def toStmt[A](e: Rep[A]): StmtList
  def seqExpr[A](s: StmtList, ss: Rep[A]): Rep[A]
  def seq(s: StmtList, ss: StmtList): StmtList
  def letExpr[A, B](v: Rep[A], in: Rep[A] => Rep[B]): Rep[B]
  def let[A](v: Rep[A], in: Rep[A] => StmtList): StmtList
  def letMutExpr[A, B](v: Rep[A], in: Rep[Ref[A]] => Rep[B]): Rep[B]
  def letMut[A](v: Rep[A], in: Rep[Ref[A]] => StmtList): StmtList
  def if_(c: Rep[Boolean], cnsq: StmtList): StmtList
  def if_(c: Rep[Boolean], cnsq: StmtList, alt: StmtList): StmtList

  def while_(c: Rep[Boolean], body: StmtList): StmtList
}

object CodeI extends Code {
  type Rep[A] = () => A
  type StmtList = () => Unit

  def int(i: Int): Rep[Int] = () => i
  def bool(b: Boolean): Rep[Boolean] = () => b

  def add(x: Rep[Int], y: Rep[Int]): Rep[Int] = () => x() + y()
  def mul(x: Rep[Int], y: Rep[Int]): Rep[Int] =  () => x() * y()
  def leq(x: Rep[Int], y: Rep[Int]): Rep[Boolean] =  () => x() <= y()
  def lt(x: Rep[Int], y: Rep[Int]): Rep[Boolean] =  () => x() < y()
  def ifExpr[A](c: Rep[Boolean], cnsq: Rep[A], alt: Rep[A]): Rep[A] =
    () => if (c()) cnsq() else alt()

  def array[A](a: IndexedSeq[Rep[A]]): Rep[IndexedSeq[A]] = () => a.map(el => el())
  def arrayLen[A](a: Rep[IndexedSeq[A]]): Rep[Int] = () => a().length
  def arrayRef[A](a: Rep[IndexedSeq[A]], i: Rep[Int]): Rep[A] = () => a()(i())

  def none[A]: Rep[Option[A]] = () => None
  def some[A](e: Rep[A]): Rep[Option[A]] = () => Some(e())
  def matchOpt[A, B](o: Rep[Option[A]], noneCase: Rep[B], someCase: Rep[A] => Rep[B]): Rep[B] =
    () => o() match {
      case None => noneCase()
      case Some(x) => someCase(() => x)()
    }

  def rget[A](r: Rep[Ref[A]]): Rep[A] = () => !r()
  def rset[A](r: Rep[Ref[A]], a: Rep[A]): StmtList = () => r() := a()
  def fail[A]: Rep[A] = ???

  implicit def toStmt[A](e: Rep[A]): StmtList = () => {e();}
  def seqExpr[A](s: StmtList, ss: Rep[A]): Rep[A] = () => {s(); ss()}
  def seq(s: StmtList, ss: StmtList): StmtList = () => {s(); ss()}
  def letExpr[A, B](v: Rep[A], in: Rep[A] => Rep[B]): Rep[B] = () => {
    val vv = v()
    in(() => vv)()
  }
  def let[A](v: Rep[A], in: Rep[A] => StmtList): StmtList = () => {
    val vv = v()
    in(() => vv)()
  }
  def letMutExpr[A, B](v: Rep[A], in: Rep[Ref[A]] => Rep[B]): Rep[B] = () => {
    val vv = Ref(v())
    in(() => vv)()
  }
  def letMut[A](v: Rep[A], in: Rep[Ref[A]] => StmtList): StmtList = () => {
    val vv = Ref(v())
    in(() => vv)()
  }
  def if_(c: Rep[Boolean], cnsq: StmtList): StmtList = () => if (c()) cnsq()
  def if_(c: Rep[Boolean], cnsq: StmtList, alt: StmtList): StmtList =
    () => if(c()) cnsq() else alt()

  def while_(c: Rep[Boolean], body: StmtList): StmtList =
    () => while(c()) body()
}

object CodeString extends Code {
  private var n: Int = 0
  private def fresh: String = {
    n += 1
    s"x_$n"
  }

  type Rep[A] = String
  type StmtList = String

  def int(i: Int): Rep[Int] = i.toString
  def bool(b: Boolean): Rep[Boolean] = b.toString

  implicit def toStmt[A](e: Rep[A]): StmtList = s"$e;"
  def seqExpr[A](s1: StmtList, s2: Rep[A]): Rep[A] = s"$s1\n$s2"
  def seq(s1: StmtList, s2: StmtList): StmtList = s"$s1\n$s2"
  def letExpr[A, B](v: Rep[A], in: Rep[A] => Rep[B]): Rep[B] = {
    val x = fresh
    s"val $x = $v;\n${ in(x) }"
  }
  def let[A](v: Rep[A], in: Rep[A] => StmtList): StmtList = {
    val x = fresh
    s"val $x = $v;\n${ in(x) }"
  }
  def letMutExpr[A, B](v: Rep[A], in: Rep[Ref[A]] => Rep[B]): Rep[B] = {
    val x = fresh
    s"var $x = $v;\n${ in(x) }"
  }
  def letMut[A](v: Rep[A], in: Rep[Ref[A]] => StmtList): StmtList = {
    val x = fresh
    s"var $x = $v;\n${ in(x) }"
  }
  def if_(c: Rep[Boolean], cnsq: StmtList): StmtList =
    s"if ($c) {\ncnsq\n}"
  def if_(c: Rep[Boolean], cnsq: StmtList, alt: StmtList): StmtList =
    s"if ($c) {\n$cnsq\n} else {\n$alt\n}"

  def add(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"($x + $y)"
  def mul(x: Rep[Int], y: Rep[Int]): Rep[Int] = s"($x * $y)"
  def leq(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = s"($x <= $y)"
  def lt(x: Rep[Int], y: Rep[Int]): Rep[Boolean] = s"($x < $y)"
  def ifExpr[A](c: Rep[Boolean], cnsq: Rep[A], alt: Rep[A]): Rep[A] =
    s"($c) ? ($cnsq) : ($alt)"

  def rget[A](r: Rep[Ref[A]]): Rep[A] = s"!($r)"
  def rset[A](r: Rep[Ref[A]], a: Rep[A]): StmtList = s"($r) := ($a);"
  def fail[A]: Rep[A] = "fail"

  def array[A](a: IndexedSeq[Rep[A]]): Rep[IndexedSeq[A]] = a.mkString("[", ", ", "]")
  def arrayLen[A](a: Rep[IndexedSeq[A]]): Rep[Int] = s"($a).length"
  def arrayRef[A](a: Rep[IndexedSeq[A]], i: Rep[Int]): Rep[A] = s"($a)[$i]"

  def none[A]: Rep[Option[A]] = "None"
  def some[A](e: Rep[A]): Rep[Option[A]] = s"Some($e)"
  def matchOpt[A, B](o: Rep[Option[A]], noneCase: Rep[B], someCase: Rep[A] => Rep[B]): Rep[B] = {
    val x = fresh
    s"(o) match { case None => $noneCase case Some($x) => someCase($x) }"
  }

  def while_(c: Rep[Boolean], body: StmtList): StmtList = s"while ($c) {\n$body\n}"
}

object Main {
  def main(args: Array[String]): Unit = {
  }
}
