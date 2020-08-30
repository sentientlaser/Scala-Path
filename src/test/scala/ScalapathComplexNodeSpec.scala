package org.shl.scalapath

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class ScalaPathComplexSpec extends AnyFlatSpec {

  var dyn:Path = Map(
    "thelist" -> List (1, 3, 4),
    "foo" -> 2,
    "bar" -> Map("baz" -> true),
    "derp" -> Map(
      "herp" -> List (
        1, 2, 3, Map (
          "first" -> List( 10, 11, 12),
          "last" -> 4
        )
      ),
      "ferp" -> 3.5
    )
  )


  "A ScalaPath map node" should "get the correct element of a path from map to list" in {
    assert( {dyn / "thelist" / 0 !} == 1)
    assert( {dyn.thelist / 0 !} == 1 )
  }


  "A ScalaPath map node" should "get the correct element of a path from map " in {
    assert( {dyn / "foo" !} == 2)
  }


  "A ScalaPath map node" should "get the correct element of a path from a fairly arbitrary path " in {
    assert( {dyn / "derp" / "herp" / 3 !} == 2)
  }

  // TODO: issue #4

}
