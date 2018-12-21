package basic_stream
import Imperative.Code
import language.higherKinds

abstract class StagedStreamSym extends BasicStagedStreamSym { self =>
  def flatMapRaw[A, B](f: A => StagedStream[B]): StagedStream[A] => StagedStream[B]

  def flatMap[A, B](f: Rep[A] => Stream[B]): Stream[A] => Stream[B] = flatMapRaw(f)

  def filter[A](f: Rep[A] => Rep[Boolean]): Stream[A] => Stream[A]
}

abstract class NestedStagedStreamCode extends StagedStreamSym { self =>
  val B: BasicStagedStream
  val code: B.code.type = B.code

  trait Cardinality
  case object AtMost1 extends Cardinality
  case object Many extends Cardinality

  trait StagedStream[A]
  case class Linear[A](producer: B.StagedStream[A], card: Cardinality) extends StagedStream[A]
  case class Nested[A, B](producer: B.StagedStream[A], nestedf: A => StagedStream[B]) extends StagedStream[B]

  type Id[A] = A

  import Imperative.Ref
  import code._

  implicit class RichStagedStreamP[A](val st: StagedStream[A]) {
    def foldRaw(consumer: A => StmtList): StmtList = self.foldRaw(consumer)(st)
    def mapRaw[B](f: A => (B => StmtList) => StmtList): StagedStream[B] = self.mapRaw(f)(st)
    def moreTermination(cond: Rep[Boolean]): StagedStream[A] = self.moreTermination(cond)(st)
  }

  def ofArray[A](a: Rep[IndexedSeq[A]]): StagedStream[Rep[A]] =
    Linear(B.ofArray(a), Many)

  def unfold[A, S](f: Rep[S] => Rep[Option[(A, S)]], s: Rep[S]): StagedStream[Rep[A]] =
    Linear(B.unfold(f, s), Many)

  def foldRaw[A](consumer: A => StmtList): StagedStream[A] => StmtList = {
    case Linear(producer, card) => card match {
      case Many => B.foldRaw(consumer)(producer)
      case AtMost1 =>
        producer.init { s => if_(producer.hasNext(s), producer.step(s, consumer)) }
    }
    case Nested(producer, nestedf) =>
      B.foldRaw((a: Id[_]) => foldRaw(consumer)(nestedf(a)))(producer)
  }

  def mapRaw[A, B](f: A => (B => StmtList) => StmtList): StagedStream[A] => StagedStream[B] = {
    case Linear(producer, card) => Linear(B.mapRaw(f)(producer), card)
    case Nested(producer, nestedf) => Nested(producer, (a: Id[_]) => mapRaw(f)(nestedf(a)))
  }

  def flatMapRaw[A, B](f: A => StagedStream[B]): StagedStream[A] => StagedStream[B] = {
    case Linear(producer, _) => Nested(producer, f)
    case Nested(producer, nestedf) => Nested(producer, (a: Id[_]) => flatMapRaw(f)(nestedf(a)))
  }

  def subSingleton[A](a: Rep[A], f: Rep[A] => Rep[Boolean]): Stream[A] = {
    val st = new B.StagedStream[Rep[A]] {
      type S = Rep[A]
      def init(k: S => StmtList): StmtList = k(a)
      def step(s: S, k: Rep[A] => StmtList): StmtList = k(s)
      def hasNext(s: S): Rep[Boolean] = f(s)
    }
    Linear(st, AtMost1)
  }

  def filter[A](f: Rep[A] => Rep[Boolean]): Stream[A] => Stream[A] =
    flatMap(a => subSingleton(a, f))

  def moreTermination[A](cond: Rep[Boolean]): StagedStream[A] => StagedStream[A] = {
    case s@Linear(producer, card) => card match {
      case Many => Linear(B.moreTermination(cond)(producer), Many)
      case AtMost1 => s
    }
    case Nested(producer, nestedf) =>
      Nested(B.moreTermination(cond)(producer), (a: Id[_]) => moreTermination(cond)(nestedf(a)))
  }

  def pushLinear[A, B, C](
    producer: B.StagedStream[A],
    nestedProducer: B.StagedStream[B],
    nestedf: B => StagedStream[C]
  ): StagedStream[(A, C)] = {
    // 'newProducer' will replace 'nestedProducer', while also initializing
    // 'producer', and exposing the state and 'hasNext' flag of 'producer'.
    // Exposing the state allows the top level to advance 'producer' once it
    // has a new 'C' value, and making the flag mutable allows the top level to
    // communicate the termination condition of 'producer' to lower levels.
    type B2 = (Rep[Ref[Boolean]], producer.S, B)
    val newProducer = new B.StagedStream[B2] {
        type S = (Rep[Ref[Boolean]], producer.S, nestedProducer.S)
        val card = Many
        def init(k: S => StmtList): StmtList =
          producer.init { s1 =>
          nestedProducer.init { s2 =>
          letMut(producer.hasNext(s1), (flag: Rep[Ref[Boolean]]) =>
            k((flag, s1, s2))) } }
        def step(s: S, k: B2 => StmtList): StmtList = s match {
          case (flag, s1, s2) => nestedProducer.step(s2, b => k((flag, s1, b)))
        }
        def hasNext(s: S): Rep[Boolean] = s match {
          case (flag, _, s2) => and(rget(flag), nestedProducer.hasNext(s2))
        }
      }

    Nested(newProducer, (t: B2) => t match {
      case (flag: Rep[Ref[Boolean]], s1: producer.S, b: B) =>
        nestedf(b).moreTermination(rget(flag)).mapRaw[(A, C)] { c => k => {
          seq(producer.step(s1, a => k((a, c))),
              rset(flag, producer.hasNext(s1)))
        }}
    })
  }

  def zipRaw[A, B](stream1: StagedStream[A], stream2: StagedStream[B]): StagedStream[(A, B)] =
    (stream1, stream2) match {
      case (Linear(producer1, _), Linear(producer2, _)) =>
        Linear(B.zipRaw(producer1, producer2), Many)
      case (Linear(producer1, _), Nested(producer2, nestf2)) =>
        pushLinear(producer1, producer2, nestf2)
      case (Nested(producer1, nestf1), Linear(producer2, _)) =>
        pushLinear(producer2, producer1, nestf1)
        .mapRaw[(A, B)](t => k => k((t._2, t._1)))
      case (stream1, stream2) => ???
    }
}

object Examples {
  def ex1(semantics: StagedStreamSym): semantics.Rep[Int] = {
    import semantics._
    import code._
    import code.Rep
    ofArray[Int](array(Range(0, 5).map(i => int(i))))
      .map { a => mul(a, a) }
      .fold(int(0)) { (x: Rep[Int], y: Rep[Int]) => add(x, y) }
  }
}

object StagedStream {
  def main(args: Array[String]): Unit = {
  }
}
