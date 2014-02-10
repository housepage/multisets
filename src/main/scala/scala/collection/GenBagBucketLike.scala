
package scala
package collection


trait GenBagBucketLike[A, +Repr]
  extends GenIterableLike[A, Repr]
  with Equals {

  def iterator: Iterator[A]

  def contains(elem: A): Boolean = multiplicity(elem) > 0

  def multiplicity(elem: A): Int

  def maxMultiplicity: Int = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxMultiplicity")

    distinctIterator.map(elem => multiplicity(elem)).max
  }

  def minMultiplicity: Int = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxMultiplicity")

    distinctIterator.map(elem => multiplicity(elem)).min
  }


  def distinctIterator: Iterator[A]

  def +(elem: A): Repr

  def -(elem: A): Repr


  def subsetOf(that: GenBagBucket[A]): Boolean = {
    this.distinctIterator.forall(elem => this.multiplicity(elem) <= that.multiplicity(elem))
  }

  override def equals(that: Any): Boolean = that match {
    case that: GenBagBucket[_] =>
      (this eq that) ||
        (that canEqual this) &&
          (this.size == that.size) &&
          (try this subsetOf that.asInstanceOf[GenBagBucket[A]]
          catch {
            case ex: ClassCastException => false
          })
    case _ =>
      false
  }

  //TODO: implement better hashCode
  override def hashCode() = scala.util.hashing.MurmurHash3.setHash(Set.empty[A] ++ this)
}