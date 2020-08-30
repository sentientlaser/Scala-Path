package org.shl.scalapath

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class ScalaPathSeqNodeSpec extends AnyFlatSpec {

  var dyn:Path = List(3, 5, 9, List(2,3))

  "A ScalaPath seq node" should "have the correct type by implicit method" in {
    assert(dyn.isInstanceOf[SeqNode[_]])
  }

  it should "fail when a map traversal is attempted" in {
    assertThrows [exceptions.sequences.CannotTraverseByMapException] {
      dyn / "foo"
    }
  }

  it should "fail when an index is out of bounds" in {
    assertThrows [exceptions.sequences.IndexOutOfBoundsException] {
      dyn / -1
    }
  }

  it should "fail when traversal is terminated" in {
    assertThrows [exceptions.aggregates.CannotTerminateException] {
      dyn !
    }
  }

  it should "get the correct element of a path at depth 1" in {
    assert( {dyn / 1 !} == 5)
  }

  it should "get the correct element of a path at depth 2" in {
    assert( {dyn / 3 / 0 !} == 2)
  }



}
