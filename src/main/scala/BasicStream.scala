import language.higherKinds

trait BasicStream {
  type Rep[A]
  type Stream[A]

  // Producers
  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A]
  def unfold[A, S](f: Rep[S] => Rep[Option[(A, S)]], s: Rep[S]): Stream[A]

  // Consumers
  def fold[A, S](f: (Rep[S], Rep[A]) => Rep[S], s: Rep[S]): Stream[A] => Rep[S]

  // Transformers
  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B]
}

object BasicStreamInterpreter extends BasicStream {
  import scala.collection.immutable.{Stream => SStream}

  type Rep[A] = A
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
}

object BasicStreamInterpreter2 extends BasicPullStream {
  type Rep[A] = A

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

  def unfold[A, S](f: S => Option[(A, S)], s: S): Stream[A] = Stream(s, f)

  // Consumers
  def fold[A, T](f: (T, A) => T, z: T): Stream[A] => T = stream => {
    def loop(acc: T, s: stream.S): T = {
      stream.step(s) match {
        case None => acc
        case Some((a, t)) => loop(f(acc, a), t)
      }
    }
    loop(z, stream.state)
  }

  // Transformers
  def map[A, B](f: Rep[A] => Rep[B]): Stream[A] => Stream[B] = stream => new Stream[B] {
    type S = stream.S
    val state = stream.state
    val step = (s: S) => stream.step(s) match {
      case None => None
      case Some((a, t)) => Some((f(a), t))
    }
  }
}

object Ex1 extends BasicPullStream {
  val res = ( andThen ) ofArray(Array.range(0, 4))
}

//object BasicStreamCode0 extends BasicStream {
//  type Rep[A] = String
//  def ofArray[A](a: Rep[IndexedSeq[A]]): Stream[A] = ???
//}
