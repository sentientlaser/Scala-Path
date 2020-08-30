package org.shl.scalapath

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps

class ScalaPathAtomicNodesSpec extends AnyFlatSpec {

  var dyn:Path = null

  "A ScalaPath atomic node" should "fail to traverse when a path is specified" in {
    dyn = 1
    assertThrows [exceptions.atoms.CannotTraverseByMapException] {
      dyn / "foo"
    }
    assertThrows [exceptions.atoms.CannotTraverseBySeqException] {
      dyn / 1
    }
  }

  it should "return the value it was assigned as Int" in {
    dyn = 1
    assert(dyn.isInstanceOf[IntNode], "ScalaPath is an instance of Int")
    assert(dyn() == 1, "ScalaPath apply() function dereferences the atom correctly")

    dyn = new java.lang.Integer(1)
    assert(dyn.isInstanceOf[IntNode], "ScalaPath is an instance of Int")
    assert(dyn() == 1, "ScalaPath apply() function dereferences the atom correctly")
  }

  it should "return the value it was assigned as Long" in {
    dyn = 1l
    assert(dyn.isInstanceOf[LongNode], "ScalaPath is an instance of Long")
    assert(dyn() == 1l, "ScalaPath apply() function dereferences the atom correctly")
    dyn = new java.lang.Long(1l)
    assert(dyn.isInstanceOf[LongNode], "ScalaPath is an instance of Long")
    assert(dyn() == 1l, "ScalaPath apply() function dereferences the atom correctly")
  }

  it should "return the value it was assigned as Float" in {
    dyn = 1f
    assert(dyn.isInstanceOf[FloatNode], "ScalaPath is an instance of Float")
    assert(dyn() == 1f, "ScalaPath apply() function dereferences the atom correctly")
    dyn = new java.lang.Float(1f)
    assert(dyn.isInstanceOf[FloatNode], "ScalaPath is an instance of Float")
    assert(dyn() == 1f, "ScalaPath apply() function dereferences the atom correctly")
  }

  it should "return the value it was assigned as Double" in {
    dyn = 1d
    assert(dyn.isInstanceOf[DoubleNode], "ScalaPath is an instance of Double")
    assert(dyn() == 1d, "ScalaPath apply() function dereferences the atom correctly")
    dyn = new java.lang.Double(1d)
    assert(dyn.isInstanceOf[DoubleNode], "ScalaPath is an instance of Double")
    assert(dyn() == 1d, "ScalaPath apply() function dereferences the atom correctly")
  }

  it should "return the value it was assigned as Boolean" in {
    dyn = true
    assert(dyn.isInstanceOf[BooleanNode], "ScalaPath is an instance of Boolean")
    assert(dyn() == true, "ScalaPath apply() function dereferences the atom correctly")
    dyn = java.lang.Boolean.TRUE
    assert(dyn.isInstanceOf[BooleanNode], "ScalaPath is an instance of Boolean")
    assert(dyn() == true, "ScalaPath apply() function dereferences the atom correctly")
  }

  it should "return the value it was assigned as String" in {
    dyn = "some string"
    assert(dyn.isInstanceOf[StringNode], "ScalaPath is an instance of String")
    assert(dyn() == "some string", "ScalaPath apply() function dereferences the atom correctly")
    dyn = new java.lang.String("some string")
    assert(dyn.isInstanceOf[StringNode], "ScalaPath is an instance of String")
    assert(dyn() == "some string", "ScalaPath apply() function dereferences the atom correctly")
  }


  it should "return the same object if the apply() or *() dereferences the same object" in {
      val dyn:UnboundScalaPath = "some string"
      assert(dyn() == {dyn !}, "atom is value equal regardless of deference method")
      assert(dyn().asInstanceOf[AnyRef] eq {dyn !}.asInstanceOf[AnyRef], "atom is value equal regardless of deference method")
  }
}
