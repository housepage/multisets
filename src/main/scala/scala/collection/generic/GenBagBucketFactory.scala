package scala
package collection
package generic

import scala.language.higherKinds
import scala.collection.mutable

abstract class GenBagBucketFactory[CC[X] <: BagBucket[X] with GenBagBucketLike[X, CC[X]]]
  extends GenericBagBucketCompanion[CC] {

  def newBuilder[A]: mutable.BagBucketBuilder[A, CC[A]]

  def bagBucketCanBuildFrom[A] = new CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = newBuilder[A]

    def apply() = newBuilder[A]
  }
}
