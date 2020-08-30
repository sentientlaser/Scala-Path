package org.shl.scalapath

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class ScalaPathMapNodeSpec extends AnyFlatSpec {

  var dyn:Path = Map(
    "foo" -> 2,
    "bar" -> Map("baz" -> true),
    "derp" -> Map(
      "herp" -> 12,
      "ferp" -> 3.5
    )
  )

  "A ScalaPath map node" should "have the correct type by implicit method" in {
    assert(dyn.isInstanceOf[MapNode[_]])
  }

  it should "fail when a non-existant member is specified" in {
    assertThrows [MapNode.NoSuchElementException] {
      dyn / "florp"
    }
  }

  it should "fail when a sequential traversal is specified" in {
    assertThrows [MapNode.CannotTraverseBySeqException] {
      dyn / 1
    }
  }


  it should "fail when traversal is terminated" in {
    assertThrows [AggregateNode.CannotTerminateException] {
      dyn !
    }
  }

  it should "get the correct element of a path at depth 1" in {
    assert( {dyn / "foo" !} == 2)
    assert( {dyn / 'foo  !} == 2)
    assert( {dyn.foo !} == 2)
  }

  it should "get the correct element of a path at depth 2" in {
    assert( {dyn / "derp" / "herp" !} == 12)
    assert( {dyn / 'derp / 'herp !} == 12)
    assert( {dyn.derp.herp !} == 12)
    assert( {dyn("derp")("herp")()} == 12)
  }

  it should "return the reference equal object regardless of accessor method" in {
    assert( dyn.foo eq {dyn / "foo" })
    assert( dyn.foo eq {dyn / "foo" })
  }


}
