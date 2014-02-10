package scala.collection.generic

import scala.collection.{mutable, immutable}
import scala.language.higherKinds


abstract class ImmutableBagBucketBuilder[CC[X] <: immutable.BagBucket[X] with immutable.BagBucketLike[X, CC[X]]]
  extends BagBucketFactory[CC] {

  def newBuilder[A]: mutable.BagBucketBuilder[A, CC[A]] = mutable.BagBucketBuilder(empty[A])

}
