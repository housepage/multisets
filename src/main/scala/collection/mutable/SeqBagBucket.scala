package scala.collection.mutable

import scala.collection.mutable


class SeqBagBucket[A](val sentinel: A, var sequence: mutable.Seq[A])
  extends scala.collection.SeqBagBucket[A]
  with mutable.BagBucket[A] {


  def +=(elem: A): this.type = {
    sequence = sequence :+ elem
    this
  }

  def -=(elem: A): this.type = {
    sequence = sequence.tail
    this
  }

}


object SeqBagBucketFactory {
  def of[A] = new mutable.SeqBagBucketFactory[A]
}

class SeqBagBucketFactory[A] extends scala.collection.BagBucketFactory[A, mutable.SeqBagBucket[A]] {

  def empty(sentinel: A): mutable.SeqBagBucket[A] = new mutable.SeqBagBucket(sentinel, mutable.Seq.empty[A])

  def apply(elem: A): mutable.SeqBagBucket[A] = new mutable.SeqBagBucket(elem, mutable.Seq(elem))

  def apply(elem: A, multiplicity: Int): mutable.SeqBagBucket[A] = new mutable.SeqBagBucket(elem, mutable.Seq.fill(multiplicity)(elem))

}