package strawman
package collection
package test

import scala.language.implicitConversions
import scala.{Unit, Int, Any}
import scala.Predef.{implicitly, $conforms}
import strawman.collection.generic.IsIterableLike

import org.junit.Test
import org.junit.Assert._

class IterableLikeTest {
  class ExtensionMethods[A, Repr](coll: IterableOps[A, Any, Repr]) {
    def mapReduce[B](mapper: A => B)(reducer: (B, B) => B): B = {
      val iter = coll.iterator
      var res = mapper(iter.next())
      while (iter.hasNext)
        res = reducer(res, mapper(iter.next()))
      res
    }
  }

  implicit def withExtensions[Repr] (coll: Repr) (implicit Iterable: IsIterableLike[Repr] ) =
    new ExtensionMethods (Iterable.conversion (coll) )

  @Test def testIterableLike: Unit = {
    implicitly[scala.collection.immutable.List[Int] => scala.collection.Iterable[Int]]
    implicitly[immutable.List[Int] => Iterable[Int]]
    assertEquals(12, immutable.List(1, 2, 3).mapReduce(_* 2)(_+ _))
    assertEquals(59, "Yeah, well, you know, that's just, like, your opinion, man.".mapReduce(x => 1)(_+ _))
  }
}
