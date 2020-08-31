package org.shl.scalapath

import collection._

import scala.reflect.ClassTag

package object path {


  type Selector[T] = Function2[T, List[PathEntry], List[PathEntry]]
  type Resolver[L] = Function1[List[PathEntry], L]
}

package path {
  trait PathHeadOps[R,T] {
    def path: Path[R,T]
    final def ?? () = path
    final def ?? (x:R) = this.path.select(x)
  }


  trait SelectorOps[R,T] {
    def select(x:R):Path[R,T]
    def resolve (): T
    final def / (x:R):Path[R,T] = select(x)
    final def ! ():T = resolve()
  }

  // dynamics would be nice, but present a slight problem because they need to the path type param to be bound to string.
  import scala.language.dynamics
  trait SelectorOpsWithDynamic[T] extends SelectorOps[String,T] with Dynamic {
    final def selectDynamic(s:String) = select(s)
  }

/*
R is the type of the Selector
T is the type of the structure being queried */
  case class Path[R,T](val entries: List[PathEntry])(implicit val selector: Selector[R], val resolver: Resolver[T]) extends SelectorOps[R,T] {
    override def select(x:R) = Path(selector(x,entries))
    override def resolve = resolver(entries)
  }

  trait PathEntry
  case class PathRootNode[R] (val root: R) extends PathEntry {
    override def toString = "PathRootNode(...)"
  }
  abstract class PathSelectorNode[K :ClassTag] (val selector: K) extends PathEntry

  case class StringSelectorNode(override val selector: String) extends PathSelectorNode[String] (selector)
  case class IntSelectorNode(override val selector: Int) extends PathSelectorNode[Int] (selector)
}
