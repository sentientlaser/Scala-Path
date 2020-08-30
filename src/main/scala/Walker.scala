package org.shl.walker

import java.lang.{Integer => JInt, Float => JFloat, Long => JLong, Double => JDouble, Boolean => JBoolean }
import scala.language.dynamics

object Implicits {
  import Types._
  implicit def walkerize(a:Any):UnboundWalker = {
    // def recurseMap(x:Map[String, Any]) = {
    //   x.map{}
    // }

    a match {
      case x:UnboundWalker => x // idempotent case
      case x:String => StringNode(x)
      case x:Int => IntNode(x)
      case x:JInt => IntNode(x.toInt)
      case x:Long => new LongNode(x)
      case x:JLong => new LongNode(x.toLong)
      case x:Float => new FloatNode(x)
      case x:JFloat => new FloatNode(x.toFloat)
      case x:Double => new DoubleNode(x)
      case x:JDouble => new DoubleNode(x.toDouble)
      case x:Boolean => new BooleanNode(x)
      case x:JBoolean => new BooleanNode(x.booleanValue)
      // case x:Map[String, Any] =>
      // case
    }
  }

}

object Types {

  trait WalkerOps[T] {
    def apply(i:String):UnboundWalker
    def apply(i:Int):UnboundWalker
    def apply():T
    def assign(o:T)

    final def / (i:String) = this(i)
    final def / (i:Symbol) = this(i.name)
    final def / (i:Int) = this(i)
    final def * () = this()

    final def := (o:T) = this.assign(o)
  }

  trait WalkerDynamicSelect[T] extends WalkerOps[T] with Dynamic {
      def selectDynamic(i:String) = this(i)
  }

  type Metadata = Option[Set[Any]]


  abstract class WalkerNode[T](val rawvalue:T, val metadata:Metadata=None) extends WalkerOps[T] with WalkerDynamicSelect[T]{
    def apply(i:String):UnboundWalker = ???
    def apply(i:Int):UnboundWalker = ???
    def apply():T
  }

  type UnboundWalker = WalkerNode[_]

  abstract class AtomicNode[T](override val rawvalue:T, override val metadata:Metadata=None) extends WalkerNode[T](rawvalue, metadata) {
    override def apply() = rawvalue
    override def assign(o:T) = ???
  }

  abstract class CollectionNode ///
  case class MapNode[Map[String, UnboundWalker]] (
    override val rawvalue:Map[String, UnboundWalker],
    override val metadata:Metadata=None
  )
  extends WalkerNode[Map[String, UnboundWalker]](rawvalue, metadata) {
    override def apply(i:String) = {
      println(rawvalue)
      null
    }
  }
  abstract class SeqNode


  case class StringNode(override val rawvalue:String, override val metadata:Metadata=None) extends AtomicNode[String](rawvalue, metadata)
  case class IntNode(override val rawvalue:Int, override val metadata:Metadata=None) extends AtomicNode[Int](rawvalue, metadata)
  case class LongNode(override val rawvalue:Long, override val metadata:Metadata=None) extends AtomicNode[Long](rawvalue, metadata)
  case class FloatNode(override val rawvalue:Float, override val metadata:Metadata=None) extends AtomicNode[Float](rawvalue, metadata)
  case class DoubleNode(override val rawvalue:Double, override val metadata:Metadata=None) extends AtomicNode[Double](rawvalue, metadata)
  case class BooleanNode(override val rawvalue:Boolean, override val metadata:Metadata=None) extends AtomicNode[Boolean](rawvalue, metadata)
}
