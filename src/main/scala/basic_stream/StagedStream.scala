package basic_stream

trait StagedStreamCode extends BasicStagedStreamCode {
  import code._

  def zipRaw[A, B](p1: StStream[A], p2: StStream[B]): StStream[(A, B)] = new StStream[(A, B)] {
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

  def zipWith[A, B, C](f: (Rep[A], Rep[B]) => Rep[C])(p1: Stream[A], p2: Stream[B]): Stream[C] =
    mapRaw[(Rep[A], Rep[B]), Rep[C]](pair => k => k(f(pair._1, pair._2)))(zipRaw(p1, p2))
}

object StagedStreamCodeString extends StagedStreamCode { val code = Imperative.CodeString }
