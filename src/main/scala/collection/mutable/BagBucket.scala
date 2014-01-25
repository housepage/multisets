package scala.collection.mutable

import scala.collection.generic.GrowableBag
import scala.collection
import scala.collection.mutable
import scala.collection.immutable.{List, Nil}

trait BagBucket[A]
  extends collection.BagBucket[A]
  with BagBucketLike[A, BagBucket[A]]
  with GrowableBag[A] {

  def -=(elem: A): this.type = remove(elem, 1)

  def remove(elem: A, count: Int): this.type

  def removeAll(elem: A): this.type = remove(elem, multiplicity(elem))
}


final class MultiplicityBagBucket[A](val sentinel: A, var multiplicity: Int)
  extends collection.MultiplicityBagBucket[A]
  with BagBucket[A]
  with BagBucketLike[A, MultiplicityBagBucket[A]] {

  def update(elem: A, multiplicity: Int): this.type = {
    if (elem == sentinel) this.multiplicity = multiplicity
    this
  }

  def clear(): Unit = multiplicity = 0

  override def +=(elem: A): this.type = {
    multiplicity += 1
    this
  }

  override def -=(elem: A): this.type = {
    if (multiplicity > 0)
      multiplicity = multiplicity - 1
    this
  }

  def added(elem: A, count: Int): MultiplicityBagBucket[A] = {
    new mutable.MultiplicityBagBucket[A](sentinel, multiplicity + Math.max(count, 0))
  }


  def add(elem: A, count: Int): this.type = {
    this.multiplicity += Math.max(count, 0)
    this
  }


  def remove(elem: A, count: Int): this.type = {
    if (multiplicity > count)
      multiplicity = multiplicity - count
    else
      multiplicity = 0
    this
  }


  override def removeAll(elem: A): this.type = {
    multiplicity = 0
    this
  }

  def addBucket(bucket: collection.BagBucket[A]): this.type = {
    this.multiplicity += bucket.multiplicity(sentinel)
    this
  }

  def addedBucket(bucket: collection.BagBucket[A]): MultiplicityBagBucket[A] = {
    new mutable.MultiplicityBagBucket[A](sentinel, this.multiplicity + bucket.multiplicity(sentinel))
  }

  override def -(elem: A): MultiplicityBagBucket[A] = {
    new mutable.MultiplicityBagBucket[A](sentinel, Math.max(0, multiplicity - 1))
  }

  def intersect(that: collection.BagBucket[A]): MultiplicityBagBucket[A] = new mutable.MultiplicityBagBucket(sentinel, Math.min(this.multiplicity, that.multiplicity(sentinel)))

  def diff(that: collection.BagBucket[A]): MultiplicityBagBucket[A] = new mutable.MultiplicityBagBucket(sentinel, Math.max(this.multiplicity - that.multiplicity(sentinel), 0))

  def removed(elem: A, count: Int): MultiplicityBagBucket[A] = new mutable.MultiplicityBagBucket(sentinel, Math.max(0, multiplicity - count))

  def distinct: MultiplicityBagBucket[A] = {
    if (multiplicity >= 1) new mutable.MultiplicityBagBucket(sentinel, 1)
    else new mutable.MultiplicityBagBucket(sentinel, 0)
  }
}

final class BagOfMultiplicitiesBagBucket[A](val sentinel: A, val bag: mutable.Bag[A])
  extends scala.collection.BagOfMultiplicitiesBagBucket[A]
  with mutable.BagBucket[A]
  with BagBucketLike[A, BagOfMultiplicitiesBagBucket[A]] {


  def clear(): Unit = bag.clear()

  def addBucket(bucket: collection.BagBucket[A]): this.type = {
    bag.addBucket(bucket)
    this
  }

  def add(elem: A, count: Int): this.type = {
    bag.add(elem, count)
    this
  }

  def remove(elem: A, count: Int): this.type = {
    bag.remove(elem, count)
    this
  }


  override def removeAll(elem: A): this.type = {
    bag.removeAll(elem)
    this
  }

  def removed(elem: A, count: Int): BagOfMultiplicitiesBagBucket[A] = new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag.removed(elem, count))

  override def -(elem: A): BagOfMultiplicitiesBagBucket[A] = new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag - elem)

  def addedBucket(bucket: collection.BagBucket[A]): BagOfMultiplicitiesBagBucket[A] = new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag addedBucket bucket)

  def added(elem: A, count: Int): BagOfMultiplicitiesBagBucket[A] = new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag.added(elem, count))

  def diff(that: collection.BagBucket[A]): BagOfMultiplicitiesBagBucket[A] = that match {
    case bagBucketBag: collection.BagOfMultiplicitiesBagBucket[A] => new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag diff bagBucketBag.bag)
    case _ => new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag.diff(bag.empty ++ that))
  }

  def intersect(that: collection.BagBucket[A]): BagOfMultiplicitiesBagBucket[A] = that match {
    case bagBucketBag: collection.BagOfMultiplicitiesBagBucket[A] => new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag intersect bagBucketBag.bag)
    case _ => new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag.intersect(bag.empty ++ that))
  }

  def distinct: BagOfMultiplicitiesBagBucket[A] = new mutable.BagOfMultiplicitiesBagBucket(sentinel, bag.distinct)
}


final class ListBagBucket[A](val sentinel: A, initialList: List[A])
  extends scala.collection.ListBagBucket[A]
  with mutable.BagBucket[A]
  with BagBucketLike[A, ListBagBucket[A]] {

  var list: List[A] = initialList

  override def toList: List[A] = list

  def clear(): Unit = list = Nil

  def result(): ListBagBucket[A] = new mutable.ListBagBucket[A](sentinel, list)

  override def +=(elem: A) = {
    list = elem :: list
    this
  }

  def remove(elem: A, count: Int): this.type = {
    list = removedFromList(elem, list, count)
    this
  }

  def added(elem: A, count: Int): ListBagBucket[A] = {
    new mutable.ListBagBucket[A](sentinel, Iterator.fill(count)(elem) ++: list)
  }


  def add(elem: A, count: Int) = {
    list = Iterator.fill(count)(elem) ++: list
    this
  }

  def addBucket(bucket: collection.BagBucket[A]) = {
    list = bucket ++: list
    this
  }

  def addedBucket(bucket: collection.BagBucket[A]): ListBagBucket[A] = {
    new mutable.ListBagBucket[A](sentinel, bucket ++: list)
  }

  def intersect(that: collection.BagBucket[A]): ListBagBucket[A] = new mutable.ListBagBucket[A](sentinel, list.intersect(that.toList))

  def diff(that: collection.BagBucket[A]): ListBagBucket[A] = new mutable.ListBagBucket[A](sentinel, list.diff(that.toList))

  def removed(elem: A, count: Int): ListBagBucket[A] = new mutable.ListBagBucket[A](sentinel, removedFromList(elem, list, count))

  override def removeAll(elem: A): this.type = {
    list = removedFromList(elem, list, list.length)
    this
  }

  def distinct: ListBagBucket[A] = new mutable.ListBagBucket[A](sentinel, list.distinct)
}


