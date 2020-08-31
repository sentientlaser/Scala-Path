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

  package atoms {
    case class CannotTraverseByMapException(override val offender: SemiStructure)
      extends SemiStructureException("Cannot map traverse from an atomic node", offender)

    case class CannotTraverseBySeqException(override val offender: SemiStructure)
      extends SemiStructureException("Cannot sequentially traverse from an atomic node", offender)
  }

  package aggregates {
    case class CannotTerminateException(override val offender: SemiStructure)
      extends SemiStructureException("Cannot terminate traversal at an aggregate node", offender)
  }

  package maps {
    case class NoSuchElementException(val member:String, override val offender: SemiStructure)
      extends SemiStructureException("Cannot traverse path to non-existant member '%s' ".format(member), offender)

    case class CannotTraverseBySeqException(override val offender: SemiStructure)
      extends SemiStructureException("Cannot sequentially traverse from a map node", offender)

  }

  package sequences {
    case class IndexOutOfBoundsException(root:Exception, override val offender: SemiStructure)
      extends SemiStructureException("Index out of bounds at sequential node", offender, root)

    case class CannotTraverseByMapException(override val offender: SemiStructure)
      extends SemiStructureException("Cannot map traverse from a sequence node", offender)
  }

}
