package basic_stream
import Imperative.Code
import language.higherKinds

abstract class StagedStreamSym { self =>
  type StagedStream[_]

  val code: Code
  type Rep[A] = code.Rep[A]
  type Stream[A] = StagedStream[Rep[A]]
  type StmtList = code.StmtList
  import Imperative.Ref
  import code._

  implicit class RichStagedStream[A](val st: StagedStream[A]) {
    def foldRaw(consumer: A => StmtList): StmtList = self.foldRaw(consumer)(st)
    def mapRaw[B](f: A => (B => StmtList) => StmtList): StagedStream[B] = self.mapRaw(f)(st)
  }

  implicit class RichStream[A](val st: Stream[A]) {
    def fold[S](s: Rep[S])(f: (Rep[S], Rep[A]) => Rep[S]): Rep[S] = self.fold(f, s)(st)
    def map[B](f: Rep[A] => Rep[B]): Stream[B] = self.map(f)(st)
  }

  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A]
  def unfold[A, S](f: Rep[S] => Rep[Option[(A, S)]], s: Rep[S]): Stream[A]

  def foldRaw[A](consumer: A => StmtList): StagedStream[A] => StmtList

  def fold[A, T](f: (Rep[T], Rep[A]) => Rep[T], z: Rep[T]): Stream[A] => Rep[T] = producer => {
    letMutExpr(z, (acc: Rep[Ref[T]]) =>
      seqExpr(foldRaw((a: Rep[A]) => rset(acc, f(rget(acc), a)))(producer), rget(acc)))
  }

  def mapRaw[A, B](f: A => (B => StmtList) => StmtList): StagedStream[A] => StagedStream[B]

  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B] =
    mapRaw((a: Rep[A]) => (k: Rep[B] => StmtList) =>
      let(f(a), (b: Rep[B]) => k(b)))

  def flatMapRaw[A, B](f: A => StagedStream[B]): StagedStream[A] => StagedStream[B]

  def flatMap[A, B](f: Rep[A] => Stream[B]): Stream[A] => Stream[B] = flatMapRaw(f)

  def filter[A](f: Rep[A] => Rep[Boolean]): Stream[A] => Stream[A]

  def zipRaw[A, B](stream1: StagedStream[A], stream2: StagedStream[B]): StagedStream[(A, B)]

  def zipWith[A, B, C](f: (Rep[A], Rep[B]) => Rep[C])(p1: Stream[A], p2: Stream[B]): Stream[C] =
    mapRaw[(Rep[A], Rep[B]), Rep[C]](pair => k => k(f(pair._1, pair._2)))(zipRaw(p1, p2))
}

abstract class NestedStagedStreamCode extends StagedStreamSym { self =>
  val B: BasicStagedStream {
    type Rep[A] = self.Rep[A]
    type StmtList = self.StmtList
  }
  val code: Code = B.code

  trait Cardinality
  case object AtMost1 extends Cardinality
  case object Many extends Cardinality

  trait StagedStream[A]
  case class Linear[A](producer: B.LinStStream[A], card: Cardinality) extends StagedStream[A]
  case class Nested[A, B](producer: B.LinStStream[A], nestedf: A => StagedStream[B]) extends StagedStream[B]

  type Id[A] = A

  import Imperative.Ref
  import code._

  implicit class RichStagedStream[A](val st: StagedStream[A]) {
    def moreTermination(cond: Rep[Boolean]): StagedStream[A] = self.moreTermination(cond)(st)
  }

  def ofArray[A](a: Rep[IndexedSeq[A]]): StagedStream[Rep[A]] =
    Linear(B.LinOfArray(a), Many)

  def unfold[A, S](f: Rep[S] => Rep[Option[(A, S)]], s: Rep[S]): StagedStream[Rep[A]] =
    Linear(B.LinUnfold(f, s), Many)

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
    val st = new B.LinStStream[Rep[A]] {
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
    producer: B.LinStStream[A],
    nestedProducer: B.LinStStream[B],
    nestedf: B => StagedStream[C]
  ): StagedStream[(A, C)] = {
    // 'newProducer' will replace 'nestedProducer', while also initializing
    // 'producer', and exposing the state and 'hasNext' flag of 'producer'.
    // Exposing the state allows the top level to advance 'producer' once it
    // has a new 'C' value, and making the flag mutable allows the top level to
    // communicate the termination condition of 'producer' to lower levels.
    type B2 = (Rep[Ref[Boolean]], producer.S, B)
    val newProducer = new B.LinStStream[B2] {
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
