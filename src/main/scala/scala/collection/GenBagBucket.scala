
package scala
package collection


trait GenBagBucket[A]
  extends GenBagBucketLike[A, GenBagBucket[A]]
  with GenIterable[A] {

  def sentinel: A

}


object GenBagBucket {

}