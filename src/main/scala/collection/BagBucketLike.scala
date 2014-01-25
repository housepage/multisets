package scala.collection

import scala.collection.generic.Subtractable


trait BagBucketLike[A, +This <: BagBucketLike[A, This] with BagBucket[A]]
  extends IterableLike[A, This]
  with Subtractable[A, This] {
  self =>

  /**
   *
   * @param that
   * @return
   */
  def intersect(that: collection.BagBucket[A]): This

  /**
   *
   * @param that
   * @return
   */
  def diff(that: collection.BagBucket[A]): This

  /**
   *
   * @param elem
   * @return
   */
  def +(elem: A): This = added(elem, 1)

  /**
   *
   * @param elemCount
   * @return
   */
  def +(elemCount: (A, Int)): This = added(elemCount._1, elemCount._2)

  /**
   *
   * @param elem
   * @param count
   * @return
   */
  def added(elem: A, count: Int): This

  /**
   *
   * @param bucket
   * @return
   */
  def addedBucket(bucket: collection.BagBucket[A]): This

  /**
   *
   * @param elem
   * @return
   */
  def -(elem: A): This = removed(elem, 1)

  /**
   *
   * @param elem
   * @param count
   * @return
   */
  def removed(elem: A, count: Int): This

  /**
   *
   * @return
   */
  def distinct: This
}
