import scala.collection.mutable.StringBuilder

sealed trait Stream[A] { self =>

  def head: Option[(A, Stream[A])]

  def map[B](f: A => B): Stream[B]

//   def +:(t: A): Stream[A]

  def take(n: Int): FiniteStream[A]

//   def combine[B, R](that: Stream[B], c: (A, B) => R): Stream[R]

//   def zip[B](that: Stream[B]): Stream[(A, B)]

//   // def flatten[B](implicit ev: A =:= FiniteStream[B]): Stream[B]

//   // def flatMap[B](f: A => FiniteStream[B]): Stream[B]

//   def as[B >: A]: Stream[B]

  final def foreach[R](f: A => R): Unit = {
    var hOpt = head
    while (hOpt.isDefined) {
      val (h, tail) = hOpt.get
      f(h)
      hOpt = tail.head
    }
  }

  override def toString(): String =
    take(5).mkString("[", ", ", "...")

}

trait InfiniteStream[A] extends Stream[A] {

  def directHead: (A, () => InfiniteStream[A])

  def item: A = directHead._1
  def tail: () => InfiniteStream[A] = directHead._2

  override final def head: Option[(A, InfiniteStream[A])] = {
    val (item, tail) = directHead
    Some((item, tail()))
  }

  override def map[B](f: A => B): InfiniteStream[B]

//   override final def combine[B, R](that: Stream[B], c: (A, B) => R): Stream[R] =
//     that match {
//       case is: InfiniteStream[B] => combine(is, c)
//       case fs: FiniteStream[B]   => combine(fs, c)
//     }

//   final def combine[B, R](
//       that: FiniteStream[B],
//       c: (A, B) => R
//   ): FiniteStream[R] =
//     StreamMerger(that, this, (f: B, i: A) => c(i, f))

//   final def combine[B, R](
//       that: InfiniteStream[B],
//       c: (A, B) => R
//   ): InfiniteStream[R] =
//     ComputedInfiniteStream(
//       c(this.item, that.item),
//       this.tail().combine(that.tail(), c)
//     )

//   override final def zip[B](that: Stream[B]): Stream[(A, B)] =
//     combine[B, (A, B)](that, tup _)

//   final def zip[B](that: InfiniteStream[B]): InfiniteStream[(A, B)] =
//     combine[B, (A, B)](that, tup _)

//   final def zip[B](that: FiniteStream[B]): FiniteStream[(A, B)] =
//     combine[B, (A, B)](that, tup _)

//   override def +:(t: A): InfiniteStream[A] =
//     Stream.pure(t) ++ this

  override final def take(n: Int): FiniteStream[A] =
    StreamSlice(this, n)

//   override final def as[B >: A]: InfiniteStream[B] =
//     this.asInstanceOf[InfiniteStream[B]]

//   private def tup[A, B](a: A, b: B): (A, B) = (a, b)

// // //   def inits: InfiniteStream[FiniteStream[A]] =
// // //     InfiniteStream { () =>
// // //       val (t, ts) = impl()
// // //       (FiniteStream(List(t)), ts.inits.map(t +: _))
// // //     }

// // //   def reverseInits: InfiniteStream[FiniteStream[A]] =
// // //     InfiniteStream { () =>
// // //       val (t, ts) = impl()
// // //       (FiniteStream(List(t)), ts.inits.map(_ :+ t))
// // //     }

// // //   override final def flatten[B](
// // //       implicit ev: A =:= FiniteStream[B]
// // //   ): InfiniteStream[B] = InfiniteStream { () =>
// // //     val (t, ts) = impl()
// // //     t.items match {
// // //       case Nil =>
// // //         ts.flatten.impl()
// // //       case h :: tl =>
// // //         val cont: A = ev.flip(FiniteStream[B](tl))
// // //         (h, (cont +: ts).flatten)
// // //     }
// // //   }

// // //   def *[B](that: InfiniteStream[B]): InfiniteStream[(A, B)] =
// // //     (this.inits zip that.reverseInits).map { case (a, b) => a zip b }.flatten

// // //   override final def flatMap[B](f: A => FiniteStream[B]): InfiniteStream[B] =
// // //     map(f).flatten
}

object ComputedInfiniteStream {
  def apply[A](head: A, tail: => InfiniteStream[A]): ComputedInfiniteStream[A] =
    new ComputedInfiniteStream(head, () => tail)
}

final class ComputedInfiniteStream[A](
    override val item: A,
    override val tail: () => InfiniteStream[A]
) extends InfiniteStream[A] { self =>

  override final val directHead: (A, () => InfiniteStream[A]) =
    (item, tail)

  override final def map[B](f: A => B): InfiniteStream[B] =
    ComputedInfiniteStream(f(item), tail().map(f))

//   override final def +:(t: A): InfiniteStream[A] =
//     ComputedInfiniteStream(t, this)

}

// case class InfiniteStreamConcat[A](a: FiniteStream[A], b: InfiniteStream[A])
//     extends InfiniteStream[A] {

//   override def directHead: (A, () => InfiniteStream[A]) =
//     a.head
//       .map {
//         case (t, fs) =>
//           (t, () => InfiniteStreamConcat(fs, b))
//       }
//       .getOrElse(b.directHead)

//   override def map[B](f: A => B): InfiniteStream[B] =
//     InfiniteStreamConcat(a.map(f), b.map(f))

//   override def item: A = directHead._1
//   override def tail: () => InfiniteStream[A] = directHead._2

// }

trait FiniteStream[A] extends Stream[A] {

  override def head: Option[(A, FiniteStream[A])]
  override def map[B](f: A => B): FiniteStream[B]

  def length: Int
//   def ++(that: Stream[A]): Stream[A]
//   def ++(that: InfiniteStream[A]): InfiniteStream[A]
//   def ++(that: FiniteStream[A]): FiniteStream[A]
//   def :+(t: A): FiniteStream[A]

//   def combine[B, R](that: Stream[B], c: (A, B) => R): FiniteStream[R]
//   def combine[B, R](that: FiniteStream[B], c: (A, B) => R): FiniteStream[R]
//   def combine[B, R](that: InfiniteStream[B], c: (A, B) => R): FiniteStream[R]

//   override final def zip[B](that: Stream[B]): FiniteStream[(A, B)] =
//     combine[B, (A, B)](that, _ -> _)

//   final def zip[B](that: InfiniteStream[B]): FiniteStream[(A, B)] =
//     combine[B, (A, B)](that, _ -> _)

//   final def zip[B](that: FiniteStream[B]): FiniteStream[(A, B)] =
//     combine[B, (A, B)](that, _ -> _)

  final def isEmpty: Boolean = length <= 0

  def fold[Z](z: Z)(op: (Z, A) => Z): Z = {
    var acc = z
    var hOpt = head
    while (hOpt.isDefined) {
      val (h, tail) = hOpt.get
      acc = op(acc, h)
      hOpt = tail.head
    }
    acc
  }

  @inline def addString(
      b: StringBuilder,
      start: String,
      sep: String,
      end: String
  ): StringBuilder = {
    val r = b.append(start)
    head match {
      case Some((h, cont)) => fold(r.append(h))(_.append(sep).append(_))
      case None            =>
    }
    r.append(end)
    r
  }

  @inline final def addString(b: StringBuilder, sep: String): StringBuilder =
    addString(b, "", sep, "")

  @inline final def addString(b: StringBuilder): StringBuilder =
    addString(b, "")

  @inline final def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).result()

  override final def toString(): String =
    if (length <= 5)
      take(5).mkString("[", ", ", "]")
    else
      take(5).mkString("[", ", ", "...")

}

case class StreamSlice[A](
    headPointer: InfiniteStream[A],
    override val length: Int
) extends FiniteStream[A] {

  def head: Option[(A, FiniteStream[A])] =
    if (isEmpty) None
    else
      Some {
        val (t, ts) = headPointer.directHead
        (t, StreamSlice(ts(), length - 1))
      }

//   def :+(t: A): FiniteStream[A] = this ++ Stream.pure(t)

//   def ++(that: FiniteStream[A]): FiniteStream[A] =
//     FiniteStreamConcat(this, that)

//   def ++(that: InfiniteStream[A]): InfiniteStream[A] = ???
//   def ++(that: Stream[A]): Stream[A] = ???

//   def combine[B, R](that: InfiniteStream[B], c: (A, B) => R): FiniteStream[R] =
//     ???
//   def combine[B, R](that: FiniteStream[B], c: (A, B) => R): FiniteStream[R] =
//     ???
//   def combine[B, R](that: Stream[B], c: (A, B) => R): FiniteStream[R] = ???
//   def +:(t: A): Stream[A] = ???
//   def as[B >: A]: Stream[B] = ???
  def map[B](f: A => B): FiniteStream[B] =
    StreamSlice(headPointer.map(f), length)

  def take(n: Int): FiniteStream[A] =
    if (n < length) StreamSlice(headPointer, n)
    else this

}

// case class FiniteStreamConcat[A](
//     a: FiniteStream[A],
//     b: FiniteStream[A]
// ) extends FiniteStream[A] {

//   override lazy val length: Int = a.length + b.length

//   def head: Option[(A, FiniteStream[A])] = ???

//   def :+(t: A): FiniteStream[A] = ???

//   def ++(that: FiniteStream[A]): FiniteStream[A] = ???
//   def ++(that: InfiniteStream[A]): InfiniteStream[A] = ???
//   def ++(that: Stream[A]): Stream[A] = ???

//   def combine[B, R](that: InfiniteStream[B], c: (A, B) => R): FiniteStream[R] =
//     ???
//   def combine[B, R](that: FiniteStream[B], c: (A, B) => R): FiniteStream[R] =
//     ???
//   def combine[B, R](that: Stream[B], c: (A, B) => R): FiniteStream[R] = ???
//   def +:(t: A): Stream[A] = ???
//   def as[B >: A]: Stream[B] = ???
//   def map[B](f: A => B): FiniteStream[B] = ???
//   def take(n: Int): FiniteStream[A] = ???

// }

// object StreamMerger {
//   def apply[F, I, R](
//       finite: FiniteStream[F],
//       infinite: InfiniteStream[I],
//       combine: (F, I) => R
//   ): StreamMerger[F, I, R] = ???
// }

// abstract class StreamMerger[F, I, R](
//     finite: FiniteStream[F],
//     infinite: InfiniteStream[I],
//     combine: (F, I) => R
// ) extends FiniteStream[R] {

//   def head: Option[(R, FiniteStream[R])] =
//     if (isEmpty) None
//     else
//       (finite.head zip infinite.head).map {
//         case ((f, fs), (i, is)) =>
//           (combine(f, i), StreamMerger(fs, is, combine))
//       }
// }

// object MaterializedStream {

//   def apply[A](items: List[A]): MaterializedStream[A] = ???

// }

case class MaterializedStream[A](items: List[A]) extends FiniteStream[A] {

  override final def head: Option[(A, FiniteStream[A])] =
    items.headOption.map(t => (t, MaterializedStream(items.tail)))

  @inline override final def length: Int =
    items.length

  override final def map[B](f: A => B): FiniteStream[B] =
    MaterializedStream(items.map(f))

// //   override final def ++(that: Stream[A]): Stream[A] =
// //     that match {
// //       case f: FiniteStream[A]   => this ++ f
// //       case i: InfiniteStream[A] => this ++ i
// //     }

// //   final def ++(that: InfiniteStream[A]): InfiniteStream[A] =
// //     items match {
// //       case Nil => that
// //       case h :: t =>
// //         h +: (FiniteStream(t) ++ that)
// //     }

// //   final def ++(that: FiniteStream[A]): FiniteStream[A] =
// //     FiniteStream(this.items ++ that.items)

// //   override final def +:(t: A): FiniteStream[A] =
// //     FiniteStream(t +: items)

// //   override final def :+(t: A): FiniteStream[A] =
// //     FiniteStream(items :+ t)

  override final def take(n: Int): FiniteStream[A] =
    MaterializedStream(items.take(n))

// //   override final def zip[B](that: Stream[B]): FiniteStream[(A, B)] =
// //     that match {
// //       case f: FiniteStream[B]   => this zip f
// //       case i: InfiniteStream[B] => this zip i
// //     }

// //   def zip[B](that: InfiniteStream[B]): FiniteStream[(A, B)] =
// //     items match {
// //       case Nil => FiniteStream(Nil)
// //       case t :: ts =>
// //         val (u, us) = that.impl()
// //         (t, u) +: (FiniteStream(ts) zip us)
// //     }

// //   def zip[B](that: FiniteStream[B]): FiniteStream[(A, B)] =
// //     FiniteStream(this.items zip that.items)

// //   def reverse: FiniteStream[A] =
// //     FiniteStream(items.reverse)

// //   override final def flatten[B](
// //       implicit ev: A =:= FiniteStream[B]
// //   ): FiniteStream[B] =
// //     FiniteStream(items.flatMap(_.items))

// //   override final def flatMap[B](f: A => FiniteStream[B]): FiniteStream[B] =
// //     map(f).flatten

// //   override final def as[B >: A]: FiniteStream[B] =
// //     this.asInstanceOf[FiniteStream[B]]

}

object Stream {

  def pure[A](t: A): FiniteStream[A] =
    MaterializedStream(List(t))

  def intsFrom(i: Int): InfiniteStream[Int] =
    ComputedInfiniteStream(i, intsFrom(i + 1))

  val UnsignedInts: InfiniteStream[Int] =
    intsFrom(0)

}

object Main extends App {
  println(Stream.UnsignedInts)
  println(Stream.UnsignedInts.take(3))

  println(("\n" * 4) + "Cross Project example")
}
