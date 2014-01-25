package scala
package collection
package generic

import scala.language.higherKinds

abstract class BagBucketFactory[CC[X] <: BagBucket[X] with BagBucketLike[X, CC[X]]]
  extends GenBagBucketFactory[CC] with GenericSeqCompanion[CC]