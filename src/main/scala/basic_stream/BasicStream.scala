package basic_stream

import language.higherKinds

trait BasicStream { self =>
  type Rep[A]
  type Stream[A]

  implicit class RichStream[A](val st: Stream[A]) {
    def fold[S](s: Rep[S])(f: (Rep[S], Rep[A]) => Rep[S]): Rep[S] = self.fold(f, s)(st)
    def map[B](f: Rep[A] => Rep[B]): Stream[B] = self.map(f)(st)
  }

  // Producers
  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A]
//  def unfold[A, S](f: Rep[S] => Rep[Option[(A, S)]], s: Rep[S]): Stream[A]

  // Consumers
  def fold[A, S](f: (Rep[S], Rep[A]) => Rep[S], s: Rep[S]): Stream[A] => Rep[S]

  // Transformers
  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B]
}

trait BasicStreamCircular extends BasicStream { type Rep[A] = A }

object BasicStreamInterpreter extends BasicStreamCircular {
  import scala.collection.immutable.{Stream => SStream}

  type Stream[A] = SStream[A]

  def ofArray[A](a: IndexedSeq[A]): SStream[A] = Stream(a: _*)
  def unfold[A, S](f: S => Option[(A, S)], s: S): SStream[A] = f(s) match {
    case None => SStream.empty
    case Some((a, s2)) => a #:: unfold(f, s2)
  }

  def fold[A, S](f: (S, A) => S, s: S): SStream[A] => S = _.foldLeft(s)(f)

  def map[A, B](f: A => B): SStream[A] => SStream[B] = _.map(f)
}

trait BasicPullStream extends BasicStream {
  object Stream {
    type Aux[A, R] = Stream[A] { type S = R }
    def apply[A, R](s: Rep[R], f: Rep[R] => Rep[Option[(A, R)]]): Aux[A, R] =
      new Stream[A] {
        type S = R
        val state = s
        val step = f
      }
  }

  abstract class Stream[A] {
    type S
    val state: Rep[S]
    val step: Rep[S] => Rep[Option[(A, S)]]
  }

  def unfold[A, S](f: Rep[S] => Rep[Option[(A, S)]], s: Rep[S]): Stream[A] = Stream(s, f)
}

object BasicPullStreamInterpreter extends BasicPullStream with BasicStreamCircular {
  def ofArray[A](a: IndexedSeq[A]): Stream[A] = {
    type S = (Int, IndexedSeq[A])
    val step: S => Option[(A, S)] = {
      case (i, a) =>
        if (i < a.length)
          Some(a(i), (i + 1, a))
        else
          None
    }
    Stream((0, a), step)
  }

  def fold[A, T](f: (T, A) => T, z: T): Stream[A] => T = stream => {
    def loop(acc: T, s: stream.S): T = {
      stream.step(s) match {
        case None => acc
        case Some((a, t)) => loop(f(acc, a), t)
      }
    }
    loop(z, stream.state)
  }

  def map[A, B](f: A => B): Stream[A] => Stream[B] = stream => new Stream[B] {
    type S = stream.S
    val state = stream.state
    val step = (s: S) => stream.step(s) match {
      case None => None
      case Some((a, t)) => Some((f(a), t))
    }
  }
}

trait BasicStreamString extends BasicStream { type Rep[A] = String }

object BasicPullStreamString extends BasicPullStream with BasicStreamString {
  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A] = {
    type S = (Int, IndexedSeq[A])
    val step: Rep[S] => Rep[Option[(A, S)]] = s => {
      s"$s match {\n" +
      s"  case (i, a) => if (i < a.length) Some(a(i), (i+1, a)) else None\n" +
      s"}\n"
    }
    Stream(s"(0, $a)", step)
  }

  def fold[A, T](f: (Rep[T], Rep[A]) => Rep[T], z: Rep[T]): Stream[A] => Rep[T] = stream => {
    s"def loop(acc, s) = {" +
    s"  ${stream.step("s")} match {\n" +
    s"    case None => acc\n" +
    s"    case Some((a, t)) => loop(${f("acc", "a")}, t)\n" +
    s"  }\n" +
    s"loop($z, ${stream.state})"
  }

  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B] = stream => {
    type S = stream.S
    val step: Rep[S] => Rep[Option[(A, S)]] = s => {
      s"${stream.step(s)} match {\n" +
      s"  case None => None\n" +
      s"  case Some((a, t)) => Some((${f("a")}, t))\n" +
      s"}\n"
    }
    Stream[B, S](stream.state, step)
  }
}

trait BasicStagedStream extends BasicStream {
  type StmtList
  abstract class StStream[A] {
    type S
    def init(k: S => StmtList): StmtList
    def step(s: S, k: A => StmtList): StmtList
    def hasNext(s: S): Rep[Boolean]
  }
  type Stream[A] = StStream[Rep[A]]
}

object BasicStagedStreamInterpreter extends BasicStagedStream with BasicStreamCircular {
  import Imperative.Ref
  type Stmt = Rep[Unit]
  type StmtList = Rep[Unit]
  def ofArray[A](a: IndexedSeq[A]): Stream[A] = new StStream[Rep[A]] {
    case class S(i: Ref[Int], n: Int, a: IndexedSeq[A])
    def init(k: S => Unit): Unit = k(S(Ref(0), a.length, a))
    def step(s: S, k: A => Unit): Unit = {
      val el = a(!s.i)
      s.i := !s.i + 1
      k(el)
    }
    def hasNext(s: S): Boolean = !s.i < s.n
  }

  def unfold[A, St](f: St => Option[(A, St)], z: St): Stream[A] =
    new StStream[Rep[A]] {
      case class S(var s: Option[(A, St)])
      def init(k: S => Rep[Unit]): Rep[Unit] = k(S(f(z)))
      def step(s: S, k: A => Unit): Unit = s.s match {
        case None => assert(false)
        case Some((el, s1)) =>
          s.s = f(s1)
          k(el)
      }
      def hasNext(s: S): Boolean = s.s != None
    }

  def fold[A, T](f: (T, A) => T, z: T): Stream[A] => T = producer => {
    val acc: Ref[T] = Ref(z)
    producer.init(s =>
      while (producer.hasNext(s))
        producer.step(s, a => { acc := f(!acc, a) })
    )
    !acc
  }

  def map[A, B](f: A => B): Stream[A] => Stream[B] = producer => new StStream[Rep[B]] {
    type S = producer.S
    def init(k: S => Unit): Unit = producer.init(k)
    def step(s: S, k: B => Unit): Unit = producer.step(s, el => k(f(el)))
    def hasNext(s: S): Boolean = producer.hasNext(s)
  }
}

trait BasicStreamCode extends BasicStream {
  val code: Imperative.Code
  type Rep[A] = code.Rep[A]
}

trait BasicStagedStreamCode extends BasicStreamCode with BasicStagedStream {
  import Imperative.Ref
  type StmtList = code.StmtList
  import code._
  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A] = new StStream[Rep[A]] {
    case class S(i: Rep[Ref[Int]], n: Rep[Int], a: Rep[IndexedSeq[A]])
    def init(k: S => StmtList): StmtList =
      let(a, (a: Rep[IndexedSeq[A]]) =>
      let(arrayLen(a), (len: Rep[Int]) =>
      letMut(int(0), (i: Rep[Ref[Int]]) =>
        k(S(i, len, a)))))
    def step(s: S, k: Rep[A] => StmtList): StmtList =
      let(arrayRef(s.a, rget(s.i)), (el: Rep[A]) =>
      seq(rset(s.i, add(rget(s.i), int(1))),
        k(el)))
    def hasNext(s: S): Rep[Boolean] = lt(rget(s.i), s.n)
  }

//  def unfold[A, St](f: Rep[St] => Rep[Option[(A, St)]], z: Rep[St]): Stream[A] = new StStream[Rep[A]] {
//    type S = Rep[Ref[Option[(A, St)]]]
//    def init(k: S => StmtList): StmtList = k(let_mut[Option[(A, St)], Ref[Option[(A, St)]]](f(z), (s: S) => s))
//    def step(s: S, k: Rep[A] => StmtList): StmtList =
//      matchOpt(rget(s),
//        fail,
//        p => seq(rset(s, f())))
//    def hasNext(s: S): Rep[Boolean] = ???
//  }

  def fold[A, T](f: (Rep[T], Rep[A]) => Rep[T], z: Rep[T]): Stream[A] => Rep[T] = producer => {
    letMutExpr(z, (acc: Rep[Ref[T]]) =>
    seqExpr(
    producer.init(s =>
    while_(producer.hasNext(s),
      producer.step(s, a =>
      rset(acc, f(rget(acc), a))))),
    rget(acc)))
  }

  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B] = producer => new StStream[Rep[B]] {
    type S = producer.S
    def init(k: S => StmtList): StmtList = producer.init(k)
    def step(s: S, k: Rep[B] => StmtList): StmtList = producer.step(s, el => k(f(el)))
    def hasNext(s: S): Rep[Boolean] = producer.hasNext(s)
  }
}

object BasicStagedStreamCodeString extends BasicStagedStreamCode {
  val code: Imperative.Code = Imperative.CodeString
}
object BasicStagedStreamCodeI extends BasicStagedStreamCode {
  val code: Imperative.Code = Imperative.CodeI
}

object Examples {
  def ex1(semantics: BasicStreamCircular): Int = {
    import semantics._
    ofArray(Array.range(0, 5))
      .map { a => a * a }
      .fold(0) { (x: Int, y: Int) => x + y }
  }

//  def ex1b(semantics: BasicStreamCircular): Int = {
//    import semantics._
//    unfold[Int, Int](i => if (i < 5) Some((i, i+1)) else None, 0)
//      .map { a => a * a }
//      .fold(0) { (x: Int, y: Int) => x + y }
//  }

  def ex1string(semantics: BasicStreamString): String = {
    import semantics._
    ofArray[Int]("Array.range(0, 5)")
      .map { (a: Rep[Int]) => s"($a * $a)" }
      .fold("0") { (x: Rep[Int], y: Rep[Int]) => s"($x + $y)" }
  }

  def ex1code(semantics: BasicStreamCode): semantics.Rep[Int] = {
    import semantics._
    import semantics.code._
    import semantics.code.Rep
    ofArray[Int](array(Range(0, 5).map(i => int(i))))
      .map { a => mul(a, a) }
      .fold(int(0)) { (x: Rep[Int], y: Rep[Int]) => add(x, y) }
  }
}

object BasicStream {
  def main(args: Array[String]): Unit = {
    println(Examples.ex1(BasicStreamInterpreter))
    println(Examples.ex1(BasicPullStreamInterpreter))
    println(Examples.ex1(BasicStagedStreamInterpreter))

//    println(Examples.ex1b(BasicPullStreamInterpreter))
//    println(Examples.ex1b(BasicPullStreamInterpreter2))
//    println(Examples.ex1b(BasicStagedStreamInterpreter))

    println()
    println(Examples.ex1code(BasicStagedStreamCodeString))
    println()
    val interpret = new BasicStagedStreamCode { val code = Imperative.CodeI }
    println(Examples.ex1code(interpret)())
  }
}
