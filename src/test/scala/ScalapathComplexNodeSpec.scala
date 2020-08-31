package org.shl.scalapath

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class SemiStructurePathComplexSpec extends AnyFlatSpec {

  import collection._
  import path._

  object data {
    import collection.implicits._
    val map = Map(
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

    val dyn:SemiStructure = map
  }

  val dyn = data.dyn

  "A ScalaPath map node" should "get the correct element of a path from map to list" in {
    val q = dyn ?? "derp" / "herp" / 3 / "first" / 2
    println({q !}.rawvalue)
  }
  //
  //
  // "A ScalaPath map node" should "get the correct element of a path from map " in {
  //   assert( {dyn / "foo" !} == 2)
  // }
  //
  //
  // "A ScalaPath map node" should "get the correct element of a path from a fairly arbitrary path " in {
  //   assert( {dyn / "derp" / "herp" / 3 !} == 2)
  // }
  //
  // // TODO: issue #4

}
