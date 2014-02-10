package scala.collection.mutable

import scala.collection._

trait BagBucketBuilder[A, +BagBucket <: GenBagBucket[A]] extends mutable.Builder[A, BagBucket] {

  def add(elem: A, count: Int): this.type

  def addBucket(bucket: GenBagBucket[A]): this.type
}

object BagBucketBuilder {

  def apply[A, BB <: immutable.BagBucket[A] with immutable.BagBucketLike[A, BB]](empty: BB): mutable.BagBucketBuilder[A, BB] = new BagBucketBuilderImpl[A, BB](empty)

  private class BagBucketBuilderImpl[A, BB <: immutable.BagBucket[A] with immutable.BagBucketLike[A, BB]](empty: BB) extends mutable.BagBucketBuilder[A, BB] {
    protected var elems: BB = empty

    def +=(elem: A): this.type = {
      elems = elems + elem
      this
    }

    def clear(): Unit = elems = empty

    def result(): BB = elems

    def add(elem: A, count: Int): this.type = {
      elems = elems.added(elem, count)
      this
    }

    def addBucket(bucket: GenBagBucket[A]): this.type = {
      elems = elems addedBucket bucket
      this
    }
  }

}

class GrowingBagBucketBuilder[A, BB <: mutable.BagBucket[A]](empty: BB)
  extends mutable.GrowingBuilder[A, BB](empty)
  with mutable.BagBucketBuilder[A, BB] {

  def add(elem: A, count: Int): this.type = {
    elems.add(elem, count)
    this
  }

  def addBucket(bucket: GenBagBucket[A]): this.type = {
    elems addBucket bucket
    this
  }
}


