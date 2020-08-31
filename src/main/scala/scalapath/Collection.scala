package org.shl.scalapath

import java.lang.{Integer => JInt, Float => JFloat, Long => JLong, Double => JDouble, Boolean => JBoolean, String => JString}
import java.util.{Map => JMap, List => JList}
import scala.language.dynamics
import scala.reflect.ClassTag
import scala.collection.JavaConverters._


import path._

package object collection {

  object implicits {
    implicit def implictlyConvertToSemiStructure1[T <: Map[String, _] :ClassTag](a:T):SemiStructure = convertToSemiStructure(a)
    implicit def implictlyConvertToSemiStructure2[T <: List[_] :ClassTag](a:T):SemiStructure = convertToSemiStructure(a)
  }

  private def convertToSemiStructure[T <: Any :ClassTag](a:T):SemiStructure = { // this handles 'any' so it's being hidden behind functions that understand types

    case class ForJMap(value: JMap[String, Any])
    case class ForJList(value: JList[Any])

    def recurseMap(x:Map[String, Any]):SemiStructure = {
       MapNode(x.map { case (k, v) => (k, convertToSemiStructure(v))})
    }

    def recurseSeq(x:Seq[Any]): SemiStructure = {
      SeqNode(x.map{ e => convertToSemiStructure(e)})
    }

    a match {
      case x:SemiStructure => x // idempotent case

      // happy happy scala land
      case x:String => StringNode(x)
      case x:Int => IntNode(x)
      case x:Long => LongNode(x)
      case x:Float => FloatNode(x)
      case x:Double => DoubleNode(x)
      case x:Boolean => BooleanNode(x)
      case x:Map[String, Any] @unchecked => recurseMap(x) // fixme: issue #6
      case x:Seq[Any] => recurseSeq(x)

      // icky icky java land
      case x:JInt => IntNode(x.toInt)
      case x:JLong => LongNode(x.toLong)
      case x:JFloat => FloatNode(x.toFloat)
      case x:JDouble => DoubleNode(x.toDouble)
      case x:JBoolean => BooleanNode(x.booleanValue)
      case ForJMap(x) => recurseMap(x.asScala.toMap)
      case ForJList(x) => recurseSeq(x.asScala.toList)

      // everything else is an exception
      case x:Any => throw new Exception("Cannot convert type " + x.getClass + " to scala path")
    }
  }

  // implicit def convertAggregateNodeToMonad(n:AgregateNode):MonadOps

  type Metadata = Option[Set[Any]]
  type SemiStructure = SemiStructureNode[_]
  type Bush = SemiStructure

  object SemiStructureSelectionPath {
   val defaultSelector:Selector[Any] = {
     (i: Any, entries: List[PathEntry]) => i match {
       case i:String => entries :+ StringSelectorNode(i)
       case i:Symbol => entries :+ StringSelectorNode(i.name)
       case i:Int    => entries :+ IntSelectorNode(i)
     }
   }
   val defaultResolver:Resolver[SemiStructure] = { (entries:List[PathEntry]) =>
     val head = entries.head.asInstanceOf[PathRootNode[SemiStructure]]
     val root = head.root.asInstanceOf[SemiStructureNode[Any]]
     entries.tail.foldLeft (root) { (node, selector) =>
       val ret = selector match {
         case StringSelectorNode(sel) => node(sel)
         case IntSelectorNode(sel) => node(sel)
       }
       ret.asInstanceOf[SemiStructureNode[Any]]
     }
   }
  }

  implicit class SemiStructureSelectionPath
  (val root: SemiStructure)
  (implicit
    val selector: Selector[Any] = SemiStructureSelectionPath.defaultSelector,
    val resolver: Resolver[SemiStructure] = SemiStructureSelectionPath.defaultResolver
    )
  extends PathHeadOps[Any, SemiStructure]{
    override def path = Path(List(PathRootNode(root)))(selector, resolver)
  }
}

package collection {


  trait SemiStructureSelectors[T] {
    def apply(i:String):SemiStructure
    def apply(i:Int):SemiStructure
    def apply():T
  }

  abstract class SemiStructureNode[T](
    val rawvalue:T,
    val metadata:Metadata=None
  ) extends SemiStructureSelectors[T] {
    override final def apply() = rawvalue
  }


  import exceptions._

  // atomic nodes
  abstract class AtomicNode[T](
    override val rawvalue:T,
    override val metadata:Metadata=None
  ) extends SemiStructureNode[T](rawvalue, metadata) {
    override def apply(i:String) = throw exceptions.CannotTraverseByMapException(this)
    override def apply(i:Int) = throw  exceptions.CannotTraverseBySeqException(this)
  }

  case class StringNode(
    override val rawvalue:String,
    override val metadata:Metadata=None
  ) extends AtomicNode[String](rawvalue, metadata)

  case class IntNode(
    override val rawvalue:Int,
    override val metadata:Metadata=None
  ) extends AtomicNode[Int](rawvalue, metadata)

  case class LongNode(
    override val rawvalue:Long,
    override val metadata:Metadata=None
  ) extends AtomicNode[Long](rawvalue, metadata)

  case class FloatNode(
    override val rawvalue:Float,
    override val metadata:Metadata=None
  ) extends AtomicNode[Float](rawvalue, metadata)

  case class DoubleNode(
    override val rawvalue:Double,
    override val metadata:Metadata=None
  ) extends AtomicNode[Double](rawvalue, metadata)

  case class BooleanNode(
    override val rawvalue:Boolean,
    override val metadata:Metadata=None
  ) extends AtomicNode[Boolean](rawvalue, metadata)

  // aggregate nodes

  abstract class AggregateNode[T](
    override val rawvalue:T,
    override val metadata:Metadata=None
  ) extends SemiStructureNode[T](rawvalue, metadata)

  case class MapNode[T <: Map[String, SemiStructure]] (
    override val rawvalue:T,
    override val metadata:Metadata=None
  )
  extends AggregateNode[T](rawvalue, metadata) {

    override def apply(i:String) = rawvalue.get(i)
    override def apply(i:Int) = throw exceptions.CannotTraverseBySeqException(this)
  }

// this is not really extensible, so maybe I should make it more 'functional'
  case class SeqNode[T <: Seq[SemiStructure]](
    override val rawvalue:T,
    override val metadata:Metadata=None
  ) extends AggregateNode[T](rawvalue, metadata) {

    override def apply(i:Int) = rawvalue.apply(i)
    override def apply(i:String) = throw exceptions.CannotTraverseByMapException(this)
  }

}
