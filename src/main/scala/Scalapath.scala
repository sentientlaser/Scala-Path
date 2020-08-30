package org.shl

import java.lang.{Integer => JInt, Float => JFloat, Long => JLong, Double => JDouble, Boolean => JBoolean, String => JString}
import java.util.{Map => JMap, List => JList}
import scala.language.dynamics
import scala.reflect.ClassTag
import scala.collection.JavaConverters._

package object scalapath {

  implicit def convertToScalaPath[T <: Any :ClassTag](a:T):UnboundScalaPath = {
    def recurseMap(x:Map[String, Any]):UnboundScalaPath = {
       MapNode(x.map { case (k, v) => (k, convertToScalaPath(v))})
    }

    def recurseSeq(x:Seq[Any]): UnboundScalaPath = {
      SeqNode(x.map{ e => convertToScalaPath(e)})
    }

    a match {
      case x:UnboundScalaPath => x // idempotent case

      // happy happy scala land
      case x:String => StringNode(x)
      case x:Int => IntNode(x)
      case x:Long => LongNode(x)
      case x:Float => FloatNode(x)
      case x:Double => DoubleNode(x)
      case x:Boolean => BooleanNode(x)
      case x:Map[String, Any] => recurseMap(x)
      case x:Seq[Any] => recurseSeq(x)

      // icky icky java land
      case x:JInt => IntNode(x.toInt)
      case x:JLong => LongNode(x.toLong)
      case x:JFloat => FloatNode(x.toFloat)
      case x:JDouble => DoubleNode(x.toDouble)
      case x:JBoolean => BooleanNode(x.booleanValue)
      case x:JMap[String, Any] => recurseMap(x.asScala.toMap)
      case x:JList[Any] => recurseSeq(x.asScala.toList)

      // everything else is an exception
      case x:Any => throw new Exception("Cannot convert type " + x.getClass + " to scala path")
    }
  }

  type Metadata = Option[Set[Any]]
  type UnboundScalaPath = ScalaPathNode[_]
  type Path = UnboundScalaPath

}

package scalapath {

  trait ScalaPathOps[T] {
    def apply(i:String):UnboundScalaPath
    def apply(i:Int):UnboundScalaPath
    def apply():T
    def assign(o:T)

    final def / (i:String) = this(i)
    final def / (i:Symbol) = this(i.name)
    final def / (i:Int) = this(i)
    final def ! () = this()

    final def := (o:T) = this.assign(o)
  }

  // it's mixed in to everything, but it's experimental, so isolated.
  trait ScalaPathDynamicSelect[T] extends ScalaPathOps[T] with Dynamic {
      def selectDynamic(i:String) = this(i)
  }

  abstract class ScalaPathNode[T](val rawvalue:T, val metadata:Metadata=None) extends ScalaPathOps[T] with ScalaPathDynamicSelect[T]{
    override def apply(i:String):UnboundScalaPath = ???
    override def apply(i:Int):UnboundScalaPath = ???
    override def apply() = rawvalue
    override def assign(o:T) = ???
  }

  // atomic nodes
  abstract class AtomicNode[T](override val rawvalue:T, override val metadata:Metadata=None) extends ScalaPathNode[T](rawvalue, metadata)

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
  ) extends ScalaPathNode[T](rawvalue, metadata)

  case class MapNode[T <: Map[String, UnboundScalaPath]] (
    override val rawvalue:T,
    override val metadata:Metadata=None
  )
  extends ScalaPathNode[T](rawvalue, metadata) {
    override def apply(i:String) = rawvalue.getOrElse(i, throw new Exception(""))
  }

  case class SeqNode[T <: Seq[UnboundScalaPath]](
    override val rawvalue:T,
    override val metadata:Metadata=None
  ) extends ScalaPathNode[T](rawvalue, metadata) {
    override def apply(i:Int) = rawvalue.apply(i)
  }





}
