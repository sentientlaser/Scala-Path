package org.shl.walker

import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps
import Implicits._
import Types._

class WalkerMapSpec extends AnyFlatSpec {
  
}

class WalkerAtomicSpec extends AnyFlatSpec {

  var dyn:UnboundWalker = null

  "A walker atomic node" should "return the value it was assigned as Int" in {
    dyn = 1
    assert(dyn.isInstanceOf[IntNode], "walker is an instance of Int")
    assert(dyn() == 1, "walker apply() function dereferences the atom correctly")

    dyn = new java.lang.Integer(1)
    assert(dyn.isInstanceOf[IntNode], "walker is an instance of Int")
    assert(dyn() == 1, "walker apply() function dereferences the atom correctly")
  }

  "A walker atomic node" should "return the value it was assigned as Long" in {
    dyn = 1l
    assert(dyn.isInstanceOf[LongNode], "walker is an instance of Long")
    assert(dyn() == 1l, "walker apply() function dereferences the atom correctly")
    dyn = new java.lang.Long(1l)
    assert(dyn.isInstanceOf[LongNode], "walker is an instance of Long")
    assert(dyn() == 1l, "walker apply() function dereferences the atom correctly")
  }

  "A walker atomic node" should "return the value it was assigned as Float" in {
    dyn = 1f
    assert(dyn.isInstanceOf[FloatNode], "walker is an instance of Float")
    assert(dyn() == 1f, "walker apply() function dereferences the atom correctly")
    dyn = new java.lang.Float(1f)
    assert(dyn.isInstanceOf[FloatNode], "walker is an instance of Float")
    assert(dyn() == 1f, "walker apply() function dereferences the atom correctly")
  }

  "A walker atomic node" should "return the value it was assigned as Double" in {
    dyn = 1d
    assert(dyn.isInstanceOf[DoubleNode], "walker is an instance of Double")
    assert(dyn() == 1d, "walker apply() function dereferences the atom correctly")
    dyn = new java.lang.Double(1d)
    assert(dyn.isInstanceOf[DoubleNode], "walker is an instance of Double")
    assert(dyn() == 1d, "walker apply() function dereferences the atom correctly")
  }

  "A walker atomic node" should "return the value it was assigned as Boolean" in {
    dyn = true
    assert(dyn.isInstanceOf[BooleanNode], "walker is an instance of Boolean")
    assert(dyn() == true, "walker apply() function dereferences the atom correctly")
    dyn = java.lang.Boolean.TRUE
    assert(dyn.isInstanceOf[BooleanNode], "walker is an instance of Boolean")
    assert(dyn() == true, "walker apply() function dereferences the atom correctly")
  }

  "A walker atomic node" should "return the value it was assigned as String" in {
    dyn = "some string"
    assert(dyn.isInstanceOf[StringNode], "walker is an instance of String")
    assert(dyn() == "some string", "walker apply() function dereferences the atom correctly")
    dyn = new java.lang.String("some string")
    assert(dyn.isInstanceOf[StringNode], "walker is an instance of String")
    assert(dyn() == "some string", "walker apply() function dereferences the atom correctly")
  }


  "A walker atomic node" should "return the same object if the apply() or *() dereferences the same object" in {
      val dyn:UnboundWalker = "some string"
      assert(dyn() == {dyn *}, "atom is value equal regardless of deference method")
      assert(dyn().asInstanceOf[AnyRef] eq {dyn *}.asInstanceOf[AnyRef], "atom is value equal regardless of deference method")
      // assert(, "walker apply() function dereferences the atom correctly")
  }
}
