package basic_stream

import Imperative._
import language.higherKinds

abstract class BasicStagedStreamSym { self =>
  val code: Code
  type Rep[A] = code.Rep[A]
  type StagedStream[_]
  type Stream[A] = StagedStream[Rep[A]]
  import code._

  implicit class RichStream[A](val st: Stream[A]) {
    def fold[S](s: Rep[S])(f: (Rep[S], Rep[A]) => Rep[S]): Rep[S] = self.fold(f, s)(st)
    def map[B](f: Rep[A] => Rep[B]): Stream[B] = self.map(f)(st)
  }

  implicit class RichStagedStream[A](val st: StagedStream[A]) {
    def foldRaw(consumer: A => StmtList): StmtList = self.foldRaw(consumer)(st)
    def mapRaw[B](f: A => (B => StmtList) => StmtList): StagedStream[B] = self.mapRaw(f)(st)
  }

  // Producers
  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A]
  def unfold[A, S](f: Rep[S] => Rep[Option[(A, S)]], s: Rep[S]): Stream[A]

  // Consumers
  def foldRaw[A](consumer: A => StmtList): StagedStream[A] => StmtList
  def fold[A, T](f: (Rep[T], Rep[A]) => Rep[T], z: Rep[T]): Stream[A] => Rep[T] = producer => {
    letMutExpr(z, (acc: Rep[Ref[T]]) =>
      seqExpr(foldRaw((a: Rep[A]) => rset(acc, f(rget(acc), a)))(producer), rget(acc)))
  }

  // Transformers
  def mapRaw[A, B](f: A => (B => StmtList) => StmtList): StagedStream[A] => StagedStream[B]
  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B] =
    mapRaw((a: Rep[A]) => (k: Rep[B] => StmtList) =>
      let(f(a), (b: Rep[B]) => k(b)))

  def zipRaw[A, B](p1: StagedStream[A], p2: StagedStream[B]): StagedStream[(A, B)]
  def zipWith[A, B, C](f: (Rep[A], Rep[B]) => Rep[C])(p1: Stream[A], p2: Stream[B]): Stream[C] =
    mapRaw[(Rep[A], Rep[B]), Rep[C]](pair => k => k(f(pair._1, pair._2)))(zipRaw(p1, p2))
}

abstract class BasicStagedStream extends BasicStagedStreamSym {
  type StmtList = code.StmtList
  import code._

  abstract class StagedStream[A] {
    type S
    def init(k: S => StmtList): StmtList
    def step(s: S, k: A => StmtList): StmtList
    def hasNext(s: S): Rep[Boolean]
  }

  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A] = new StagedStream[Rep[A]] {
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

  def unfold[A, St](f: Rep[St] => Rep[Option[(A, St)]], z: Rep[St]): Stream[A] = new StagedStream[Rep[A]] {
    type S = Rep[Ref[Option[(A, St)]]]
    def init(k: S => StmtList): StmtList =
      letMut[Option[(A, St)]](f(z), (s: S) => k(s))
    def step(s: Rep[Ref[Option[(A, St)]]], k: Rep[A] => StmtList): StmtList =
      matchOpt[(A, St)](rget(s),
        fail[(A, St)],
        p => seq(rset(s, f(snd(p))), k(fst(p))))
    def hasNext(s: Rep[Ref[Option[(A, St)]]]): Rep[Boolean] = not(equal(rget(s), none[(A, St)]))
  }

//  def fold[A, T](f: (Rep[T], Rep[A]) => Rep[T], z: Rep[T]): Stream[A] => Rep[T] = producer => {
//    letMutExpr(z, (acc: Rep[Ref[T]]) =>
//    seqExpr(
//    producer.init(s =>
//    while_(producer.hasNext(s),
//      producer.step(s, a =>
//      rset(acc, f(rget(acc), a))))),
//    rget(acc)))
//  }

  def foldRaw[A](consumer: A => StmtList): StagedStream[A] => StmtList = producer =>
    producer.init { s => while_(producer.hasNext(s), producer.step(s, consumer)) }


  //  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B] = producer => new StStream[Rep[B]] {
  //    type S = producer.S
  //    def init(k: S => StmtList): StmtList = producer.init(k)
  //    def step(s: S, k: Rep[B] => StmtList): StmtList = producer.step(s, el => k(f(el)))
  //    def hasNext(s: S): Rep[Boolean] = producer.hasNext(s)
  //  }

  def mapRaw[A, B](f: A => (B => StmtList) => StmtList): StagedStream[A] => StagedStream[B] =
    producer => new StagedStream[B] {
      type S = producer.S
      def init(k: S => StmtList): StmtList = producer.init(k)
      def step(s: S, k: B => StmtList): StmtList = producer.step(s, el => f(el)(k))
      def hasNext(s: S): Rep[Boolean] = producer.hasNext(s)
    }

  def zipRaw[A, B](p1: StagedStream[A], p2: StagedStream[B]): StagedStream[(A, B)] = new StagedStream[(A, B)] {
    type S = (p1.S, p2.S)
    def init(k: S => StmtList): StmtList =
      p1.init { s1 => p2.init { s2 => k(s1, s2) } }
    def step(s: S, k: ((A, B)) => StmtList): StmtList = s match { case (s1, s2) =>
      p1.step(s1, e1 => p2.step(s2, e2 => k((e1, e2))))
    }
    def hasNext(s: S): Rep[Boolean] = s match { case (s1, s2) =>
      and(p1.hasNext(s1), p2.hasNext(s2))
    }
  }

  def moreTermination[A](cond: Rep[Boolean]): StagedStream[A] => StagedStream[A] =
    producer => new StagedStream[A] {
      type S = producer.S
      def init(k: S => StmtList): StmtList = producer.init(k)
      def step(s: S, k: A => StmtList): StmtList = producer.step(s, k)
      def hasNext(s: S): Rep[Boolean] = and(producer.hasNext(s), cond)
    }
}

object BasicStagedStreamString extends BasicStagedStream {
  val code = CodeString
}
object BasicStagedStreamI extends BasicStagedStream {
  val code = CodeI
}

object BasicExamples {
  def ex1(semantics: BasicStagedStreamSym): semantics.Rep[Int] = {
    import semantics._
    import code._
    import code.Rep
    ofArray[Int](array(Range(0, 5).map(i => int(i))))
      .map { a => mul(a, a) }
      .fold(int(0)) { (x: Rep[Int], y: Rep[Int]) => add(x, y) }
  }

  def ex1b(semantics: BasicStagedStreamSym): semantics.Rep[Int] = {
    import semantics._
    import code._
    unfold[Int, Int](i => ifExpr(lt(i, int(5)), some(pair(i, add(i, int(1)))), none), int(0))
      .map { a => mul(a, a) }
      .fold(int(0)) { (x, y) => add(x, y)}
  }

  def ex2(semantics: BasicStagedStreamSym): semantics.Rep[Int] = {
    import semantics._
    import code._
    val p1 = ofArray[Int](array(Range(0, 5).map(i => int(i))))
    val p2 = unfold[Int, Int](i => some(pair(i, add(i, int(1)))), int(0))
    zipWith[Int, Int, Int]((x, y) => add(x, y))(p1, p2)
      .map { a => add(a, int(1)) }
      .fold(int(0)) { (x, y) => add(x, y)}
  }
}

object BasicStream {
  def main(args: Array[String]): Unit = {
    println("ex1")

    println(BasicExamples.ex1(BasicStagedStreamI)())
    println()
    println(BasicExamples.ex1(BasicStagedStreamString))

    println()

    println("ex1b")
    println(BasicExamples.ex1b(BasicStagedStreamI)())
    println()
    println(BasicExamples.ex1b(BasicStagedStreamString))

    println("ex2")
    println(BasicExamples.ex2(BasicStagedStreamI)())
    println()
    println(BasicExamples.ex2(BasicStagedStreamString))
  }
}
