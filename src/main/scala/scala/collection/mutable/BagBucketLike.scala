package scala.collection.mutable

import scala.collection.mutable

trait BagBucketLike[A, +This <: mutable.BagBucketLike[A, This] with mutable.BagBucket[A]]
  extends collection.BagBucketLike[A, This] {
  self =>


}
