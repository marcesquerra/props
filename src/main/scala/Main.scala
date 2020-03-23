sealed trait Stream[T] { self =>

  def head: Option[(T, Stream[T])]

  def map[U](f: T => U): Stream[U]

  def +:(t: T): Stream[T]

  def take(n: Int): FiniteStream[T]

  def combine[U, R](that: Stream[U], c: (T, U) => R): Stream[R]

  def zip[U](that: Stream[U]): Stream[(T, U)]

  // def flatten[U](implicit ev: T =:= FiniteStream[U]): Stream[U]

  // def flatMap[U](f: T => FiniteStream[U]): Stream[U]

  def as[U >: T]: Stream[U]

  final def foreach[R](f: T => R): Unit = {
    var hOpt = head
    while (hOpt.isDefined) {
      val (h, tail) = hOpt.get
      f(h)
      hOpt = tail.head
    }
  }
}

trait InfiniteStream[T] extends Stream[T] {

  def directHead: (T, () => InfiniteStream[T])

  def item: T
  def tail: () => InfiniteStream[T]

  override final def head: Option[(T, InfiniteStream[T])] = {
    val (item, tail) = directHead
    Some((item, tail()))
  }

  override def map[U](f: T => U): InfiniteStream[U]

  override final def combine[U, R](that: Stream[U], c: (T, U) => R): Stream[R] =
    that match {
      case is: InfiniteStream[U] => combine(is, c)
      case fs: FiniteStream[U]   => combine(fs, c)
    }

  final def combine[U, R](
      that: FiniteStream[U],
      c: (T, U) => R
  ): FiniteStream[R] =
    StreamMerger(that, this, (f, i) => c(i, f))

  final def combine[U, R](
      that: InfiniteStream[U],
      c: (T, U) => R
  ): InfiniteStream[R] =
    ComputedInfiniteStream(
      c(this.item, that.item),
      this.tail().combine(that.tail(), c)
    )

  override final def zip[U](that: Stream[U]): Stream[(T, U)] =
    combine[U, (T, U)](that, _ -> _)

  final def zip[U](that: InfiniteStream[U]): InfiniteStream[(T, U)] =
    combine[U, (T, U)](that, _ -> _)

  final def zip[U](that: FiniteStream[U]): FiniteStream[(T, U)] =
    combine[U, (T, U)](that, _ -> _)

  override def +:(t: T): InfiniteStream[T] =
    Stream.pure(t) ++ this

  override final def take(n: Int): FiniteStream[T] =
    StreamSlice(this, n)

  override final def as[U >: T]: InfiniteStream[U] =
    this.asInstanceOf[InfiniteStream[U]]

// //   def inits: InfiniteStream[FiniteStream[T]] =
// //     InfiniteStream { () =>
// //       val (t, ts) = impl()
// //       (FiniteStream(List(t)), ts.inits.map(t +: _))
// //     }

// //   def reverseInits: InfiniteStream[FiniteStream[T]] =
// //     InfiniteStream { () =>
// //       val (t, ts) = impl()
// //       (FiniteStream(List(t)), ts.inits.map(_ :+ t))
// //     }

// //   override final def flatten[U](
// //       implicit ev: T =:= FiniteStream[U]
// //   ): InfiniteStream[U] = InfiniteStream { () =>
// //     val (t, ts) = impl()
// //     t.items match {
// //       case Nil =>
// //         ts.flatten.impl()
// //       case h :: tl =>
// //         val cont: T = ev.flip(FiniteStream[U](tl))
// //         (h, (cont +: ts).flatten)
// //     }
// //   }

// //   def *[U](that: InfiniteStream[U]): InfiniteStream[(T, U)] =
// //     (this.inits zip that.reverseInits).map { case (a, b) => a zip b }.flatten

// //   override final def flatMap[U](f: T => FiniteStream[U]): InfiniteStream[U] =
// //     map(f).flatten
}

object ComputedInfiniteStream {
  def apply[T](head: T, tail: => InfiniteStream[T]): ComputedInfiniteStream[T] =
    new ComputedInfiniteStream(head, () => tail)
}

final class ComputedInfiniteStream[T](
    override val item: T,
    override val tail: () => InfiniteStream[T]
) extends InfiniteStream[T] { self =>

  override final val directHead: (T, () => InfiniteStream[T]) =
    (item, tail)

  override final def map[U](f: T => U): InfiniteStream[U] =
    ComputedInfiniteStream(f(item), tail().map(f))

  override final def +:(t: T): InfiniteStream[T] =
    ComputedInfiniteStream(t, this)

}

case class InfiniteStreamConcat[T](a: FiniteStream[T], b: InfiniteStream[T])
    extends InfiniteStream[T] {

  override def directHead: (T, () => InfiniteStream[T]) =
    a.head
      .map {
        case (t, fs) =>
          (t, () => InfiniteStreamConcat(fs, b))
      }
      .getOrElse(b.directHead)

  override def map[U](f: T => U): InfiniteStream[U] =
    InfiniteStreamConcat(a.map(f), b.map(f))

  override def item: T = directHead._1
  override def tail: () => InfiniteStream[T] = directHead._2

}

trait FiniteStream[T] extends Stream[T] {

  override def head: Option[(T, FiniteStream[T])]
  override def map[U](f: T => U): FiniteStream[U]

  def length: Int
  def ++(that: Stream[T]): Stream[T]
  def ++(that: InfiniteStream[T]): InfiniteStream[T]
  def ++(that: FiniteStream[T]): FiniteStream[T]
  def :+(t: T): FiniteStream[T]

  def combine[U, R](that: Stream[U], c: (T, U) => R): FiniteStream[R]
  def combine[U, R](that: FiniteStream[U], c: (T, U) => R): FiniteStream[R]
  def combine[U, R](that: InfiniteStream[U], c: (T, U) => R): FiniteStream[R]

  override final def zip[U](that: Stream[U]): FiniteStream[(T, U)] =
    combine[U, (T, U)](that, _ -> _)

  final def zip[U](that: InfiniteStream[U]): FiniteStream[(T, U)] =
    combine[U, (T, U)](that, _ -> _)

  final def zip[U](that: FiniteStream[U]): FiniteStream[(T, U)] =
    combine[U, (T, U)](that, _ -> _)

  final def isEmpty: Boolean = length <= 0
}

// object StreamSlice {
//   def apply[T](headPointer: InfiniteStream[T], length: Int): StreamSlice[T] =
//     ???
// }

case class StreamSlice[T](
    headPointer: InfiniteStream[T],
    override val length: Int
) extends FiniteStream[T] {

  def head: Option[(T, FiniteStream[T])] =
    if (isEmpty) None
    else
      Some {
        val (t, ts) = headPointer.directHead
        (t, StreamSlice(ts(), length - 1))
      }

  def :+(t: T): FiniteStream[T] = this ++ Stream.pure(t)

  def ++(that: FiniteStream[T]): FiniteStream[T] =
    FiniteStreamConcat(this, that)

  def ++(that: InfiniteStream[T]): InfiniteStream[T] = ???
  def ++(that: Stream[T]): Stream[T] = ???

  def combine[U, R](that: InfiniteStream[U], c: (T, U) => R): FiniteStream[R] =
    ???
  def combine[U, R](that: FiniteStream[U], c: (T, U) => R): FiniteStream[R] =
    ???
  def combine[U, R](that: Stream[U], c: (T, U) => R): FiniteStream[R] = ???
  def +:(t: T): Stream[T] = ???
  def as[U >: T]: Stream[U] = ???
  def map[U](f: T => U): FiniteStream[U] = ???
  def take(n: Int): FiniteStream[T] = ???

}

case class FiniteStreamConcat[T](
    a: FiniteStream[T],
    b: FiniteStream[T]
) extends FiniteStream[T] {

  override lazy val length: Int = a.length + b.length

  def head: Option[(T, FiniteStream[T])] = ???

  def :+(t: T): FiniteStream[T] = ???

  def ++(that: FiniteStream[T]): FiniteStream[T] = ???
  def ++(that: InfiniteStream[T]): InfiniteStream[T] = ???
  def ++(that: Stream[T]): Stream[T] = ???

  def combine[U, R](that: InfiniteStream[U], c: (T, U) => R): FiniteStream[R] =
    ???
  def combine[U, R](that: FiniteStream[U], c: (T, U) => R): FiniteStream[R] =
    ???
  def combine[U, R](that: Stream[U], c: (T, U) => R): FiniteStream[R] = ???
  def +:(t: T): Stream[T] = ???
  def as[U >: T]: Stream[U] = ???
  def map[U](f: T => U): FiniteStream[U] = ???
  def take(n: Int): FiniteStream[T] = ???

}

object StreamMerger {
  def apply[F, I, R](
      finite: FiniteStream[F],
      infinite: InfiniteStream[I],
      combine: (F, I) => R
  ): StreamMerger[F, I, R] = ???
}

abstract class StreamMerger[F, I, R](
    finite: FiniteStream[F],
    infinite: InfiniteStream[I],
    combine: (F, I) => R
) extends FiniteStream[R] {

  def head: Option[(R, FiniteStream[R])] =
    if (isEmpty) None
    else
      (finite.head zip infinite.head).map {
        case ((f, fs), (i, is)) =>
          (combine(f, i), StreamMerger(fs, is, combine))
      }
}

object MaterializedStream {

  def apply[T](items: List[T]): MaterializedStream[T] = ???

}

abstract class MaterializedStream[T](items: List[T]) extends FiniteStream[T] {

//   override final def next: Option[(T, FiniteStream[T])] =
//     items.headOption.map(t => (t, FiniteStream(items.tail)))

//   override final def map[U](f: T => U): FiniteStream[U] =
//     FiniteStream(items.map(f))

//   override final def ++(that: Stream[T]): Stream[T] =
//     that match {
//       case f: FiniteStream[T]   => this ++ f
//       case i: InfiniteStream[T] => this ++ i
//     }

//   final def ++(that: InfiniteStream[T]): InfiniteStream[T] =
//     items match {
//       case Nil => that
//       case h :: t =>
//         h +: (FiniteStream(t) ++ that)
//     }

//   final def ++(that: FiniteStream[T]): FiniteStream[T] =
//     FiniteStream(this.items ++ that.items)

//   override final def +:(t: T): FiniteStream[T] =
//     FiniteStream(t +: items)

//   override final def :+(t: T): FiniteStream[T] =
//     FiniteStream(items :+ t)

//   override final def take(n: Int): FiniteStream[T] =
//     FiniteStream(items.take(n))

//   override final def zip[U](that: Stream[U]): FiniteStream[(T, U)] =
//     that match {
//       case f: FiniteStream[U]   => this zip f
//       case i: InfiniteStream[U] => this zip i
//     }

//   def zip[U](that: InfiniteStream[U]): FiniteStream[(T, U)] =
//     items match {
//       case Nil => FiniteStream(Nil)
//       case t :: ts =>
//         val (u, us) = that.impl()
//         (t, u) +: (FiniteStream(ts) zip us)
//     }

//   def zip[U](that: FiniteStream[U]): FiniteStream[(T, U)] =
//     FiniteStream(this.items zip that.items)

//   def reverse: FiniteStream[T] =
//     FiniteStream(items.reverse)

//   override final def flatten[U](
//       implicit ev: T =:= FiniteStream[U]
//   ): FiniteStream[U] =
//     FiniteStream(items.flatMap(_.items))

//   override final def flatMap[U](f: T => FiniteStream[U]): FiniteStream[U] =
//     map(f).flatten

//   override final def as[U >: T]: FiniteStream[U] =
//     this.asInstanceOf[FiniteStream[U]]
}

object Stream {

  def pure[T](t: T): FiniteStream[T] =
    MaterializedStream(List(t))

  def intsFrom(i: Int): InfiniteStream[Int] =
    ComputedInfiniteStream(i, intsFrom(i + 1))

  val UnsignedInts: InfiniteStream[Int] =
    intsFrom(0)

}

object Main extends App {
  // val a = Stream.UnsignedInts.map(_ + 1)
  // val b = Stream.UnsignedInts.map('a' + _).map(_.toChar)
  // val c = a * b
  // for {
  //   t <- a.reverseInits.take(20)
  // } println(t)

  // println("Done")
}
