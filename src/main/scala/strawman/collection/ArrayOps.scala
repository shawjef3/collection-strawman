package strawman
package collection

import scala.{AnyVal, Array, ArrayIndexOutOfBoundsException, Char, Int, throws, AnyRef, Double, Boolean, Long, Float, Serializable, Byte, Short, Unit, Any, `inline`, specialized}
import scala.Predef.???
import scala.util.hashing.MurmurHash3
import mutable.{ArrayBuffer, GrowableBuilder}
import java.util.Arrays
import scala.reflect.ClassTag

// We need ArrayOpsParent because ArrayOps (as a value class) may not contain a nested class (ArrayWithFilter)
trait ArrayOpsParent[A] extends Any
  with IterableOnce[A]
  with IndexedSeqOps[A, immutable.IndexedSeq, Array[A]]
  with StrictOptimizedIterableOps[A, Seq, Array[A]]
  with ArrayLike[A] {

  protected[this] def fromTaggedIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]

  override def withFilter(p: A => Boolean): ArrayWithFilter = new ArrayWithFilter(p)

  class ArrayWithFilter(p: A => Boolean) extends WithFilter(p) {
    def map[B: ClassTag](f: A => B): Array[B] = fromTaggedIterable(View.Map(filtered, f))
    def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromTaggedIterable(View.FlatMap(filtered, f))
    override def withFilter(q: A => Boolean): ArrayWithFilter = new ArrayWithFilter(a => p(a) && q(a))
  }
}

class ArrayOps[A](val xs: Array[A]) extends AnyVal with ArrayOpsParent[A] {

  protected[this] def coll = ArrayView(xs)
  protected[this] def c: Array[A] = xs
  protected[this] def seq: Seq[A] = iterableFactory.fromIterable(coll)

  def length = xs.length
  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int) = xs.apply(i)

  override def view = ArrayView(xs)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  def iterableFactory = immutable.IndexedSeq

  protected[this] def fromSpecificIterable(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  protected[this] def newSpecificBuilder() = new GrowableBuilder(ArrayBuffer.empty[A]).mapResult(_.toArray(elemTag))

  override def knownSize = xs.length

  override def className = "Array"

  def iterator(): Iterator[A] = coll.iterator()
  def map[B: ClassTag](f: A => B): Array[B] = fromTaggedIterable(View.Map(coll, f))
  def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromTaggedIterable(View.FlatMap(coll, f))
  def ++[B >: A : ClassTag](xs: IterableOnce[B]): Array[B] = fromTaggedIterable(View.Concat(coll, xs))
  def zip[B: ClassTag](xs: IterableOnce[B]): Array[(A, B)] = fromTaggedIterable(View.Zip(coll, xs))
}

abstract class ArrayView[A] extends IndexedView[A] {
  //def length = xs.length
  //@throws[ArrayIndexOutOfBoundsException]
  //def apply(n: Int) = xs(n)
  override def className = "ArrayView"
}

object ArrayView {
  // This is reused for all calls to empty.
  private val EmptyArrayView  = new ofRef[AnyRef](new Array[AnyRef](0))
  def empty[T <: AnyRef]: ArrayView[T] = EmptyArrayView.asInstanceOf[ArrayView[T]]

  @`inline` def apply[T](x: Array[T]): ArrayView[T] = make(x)

  // If make is called explicitly we use whatever we're given, even if it's
  // empty.  This may be unnecessary (if ArrayView is to honor the collections
  // contract all empty ones must be equal, so discriminating based on the reference
  // equality of an empty array should not come up) but we may as well be
  // conservative since wrapRefArray contributes most of the unnecessary allocations.
  def make[T](x: AnyRef): ArrayView[T] = (x match {
    case null              => null
    case x: Array[AnyRef]  => new ofRef[AnyRef](x)
    case x: Array[Int]     => new ofInt(x)
    case x: Array[Double]  => new ofDouble(x)
    case x: Array[Long]    => new ofLong(x)
    case x: Array[Float]   => new ofFloat(x)
    case x: Array[Char]    => new ofChar(x)
    case x: Array[Byte]    => new ofByte(x)
    case x: Array[Short]   => new ofShort(x)
    case x: Array[Boolean] => new ofBoolean(x)
    case x: Array[Unit]    => new ofUnit(x)
  }).asInstanceOf[ArrayView[T]]

  final class ofRef[T <: AnyRef](val array: Array[T]) extends ArrayView[T] with Serializable {
    lazy val elemTag = ClassTag[T](array.getClass.getComponentType)
    def length: Int = array.length
    def apply(index: Int): T = array(index).asInstanceOf[T]
    def update(index: Int, elem: T) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofRef[_] => Arrays.equals(array.asInstanceOf[Array[AnyRef]], that.array.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
  }

  final class ofByte(val array: Array[Byte]) extends ArrayView[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte) { array(index) = elem }
    override def hashCode = wrappedBytesHash(array)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofShort(val array: Array[Short]) extends ArrayView[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    def update(index: Int, elem: Short) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofChar(val array: Array[Char]) extends ArrayView[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    def update(index: Int, elem: Char) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofInt(val array: Array[Int]) extends ArrayView[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    def update(index: Int, elem: Int) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofLong(val array: Array[Long]) extends ArrayView[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    def update(index: Int, elem: Long) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofFloat(val array: Array[Float]) extends ArrayView[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    def update(index: Int, elem: Float) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofDouble(val array: Array[Double]) extends ArrayView[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    def update(index: Int, elem: Double) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofBoolean(val array: Array[Boolean]) extends ArrayView[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    def update(index: Int, elem: Boolean) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofUnit(val array: Array[Unit]) extends ArrayView[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    def update(index: Int, elem: Unit) { array(index) = elem }
    override def hashCode = arrayViewHash(array)
    override def equals(that: Any) = that match {
      case that: ofUnit => array.length == that.array.length
      case _ => super.equals(that)
    }
  }

  //TODO This methods needs to be moved into MurmurHash3
  private def arrayViewHash[@specialized T](a: Array[T]): Int = MurmurHash3.arrayHash(a, MurmurHash3.seqSeed)

  //TODO Call the corresponding method (which is private[scala]) in MurmurHash3 instead
  private def wrappedBytesHash(data: Array[Byte]): Int = MurmurHash3.bytesHash(data, MurmurHash3.seqSeed)
}