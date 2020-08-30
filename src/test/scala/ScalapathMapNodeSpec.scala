package org.shl.scalapath

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class ScalaPathMapSpec extends AnyFlatSpec {

  var dyn:Path = Map(
    "foo" -> 2,
    "bar" -> Map("baz" -> true),
    "derp" -> Map(
      "herp" -> 12,
      "ferp" -> 3.5
    )
  )

  "A ScalaPath map node" should "have the correct type by inferred type" in {
    assert(dyn.isInstanceOf[MapNode[_]])
  }

  "A ScalaPath map node" should "get the correct element of a path at depth 1" in {
    assert( {dyn / "foo" !} == 2)
    assert( {dyn / 'foo  !} == 2)
    assert( {dyn.foo !} == 2)
  }

  "A ScalaPath map node" should "get the correct element of a path at depth 2" in {
    assert( {dyn / "derp" / "herp" !} == 12)
    assert( {dyn / 'derp / 'herp !} == 12)
    assert( {dyn.derp.herp !} == 12)
  }

  "A ScalaPath map node" should "return the reference equal object regardless of accessor method" in {
    assert( dyn.foo eq {dyn / "foo" })
    assert( dyn.foo eq {dyn / "foo" })
  }


}
