package org.shl.scalapath.collection

import java.lang.{Integer => JInt, Float => JFloat, Long => JLong, Double => JDouble, Boolean => JBoolean, String => JString}
import java.util.{Map => JMap, List => JList}
import scala.language.dynamics
import scala.reflect.ClassTag
import scala.collection.JavaConverters._

package exceptions {
  abstract class SemiStructureException(
    val message:String,
    val offender:SemiStructure,
    val cause:Exception = null
  ) extends RuntimeException(message, cause)

  case class CannotTraverseByMapException(override val offender: SemiStructure)
    extends SemiStructureException("Cannot map traverse from this node", offender)

  case class CannotTraverseBySeqException(override val offender: SemiStructure)
    extends SemiStructureException("Cannot sequentially traverse from this node", offender)


}
