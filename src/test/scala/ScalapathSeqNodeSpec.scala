package org.shl.scalapath

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class ScalaPathSeqSpec extends AnyFlatSpec {

  var dyn:Path = List(3, 5, 9, List(2,3))

  "A ScalaPath seq node" should "have the correct type by inferred type" in {
    assert(dyn.isInstanceOf[SeqNode[_]])
  }

  "A ScalaPath seq node" should "get the correct element of a path at depth 1" in {
    assert( {dyn / 1 !} == 5)
  }

  "A ScalaPath seq node" should "get the correct element of a path at depth 2" in {
    assert( {dyn / 3 / 0 !} == 2)
  }



}
