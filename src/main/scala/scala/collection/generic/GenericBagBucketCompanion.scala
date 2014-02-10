package scala.collection
package generic

import scala.language.higherKinds


trait GenericBagBucketCompanion[+CC[X] <: collection.BagBucket[X]]
  extends GenericCompanion[CC] {

  def newBuilder[A]: mutable.BagBucketBuilder[A, CC[A]]

  def from[A](elemCounts: (A, Int)*): CC[A] = {
    if (elemCounts.isEmpty) empty[A]
    else {
      val b = newBuilder[A]
      for ((elem, count) <- elemCounts) {
        b add(elem, count)
      }
      b.result()
    }
  }

}
