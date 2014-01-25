package scala.collection.mutable

trait BagBucketLike[A, +This <: BagBucketLike[A, This] with BagBucket[A]]
  extends collection.BagBucketLike[A, This] {
  self =>


}
