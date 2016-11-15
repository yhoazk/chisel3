// See LICENSE for license details.

package chiselTests

import chisel3._
import scala.collection.immutable.ListMap

class RecordSpec extends ChiselFlatSpec {
  class RecordFromMap(val elements: ListMap[String, Data]) extends Record {
    override def cloneType =
      (new RecordFromMap(elements.map(e => e._1 -> e._2.cloneType))).asInstanceOf[this.type]
  }
  class MyBundle extends Bundle {
    val foo = UInt.width(32)
    val bar = UInt.width(32)
    override def cloneType = (new MyBundle).asInstanceOf[this.type]
  }
  val listMap = ListMap("foo" -> UInt.width(32), "bar" -> UInt.width(32))

  class MyModule(output: => Record, input: => Record) extends Module {
    val io = IO(new Bundle {
      val in = Input(input.cloneType)
      val out = Output(output.cloneType)
    })
    io.out <> io.in
  }

  "Records" should "work similarly to Bundles" in {
    elaborate { new MyModule(new RecordFromMap(listMap), new RecordFromMap(listMap)) }
  }

  "Records" should "interoperate with Bundles" in {
    elaborate { new MyModule(new MyBundle, new RecordFromMap(listMap)) }
  }

  "Bulk connect on Record" should "check that the fields match" in {
    (the [ChiselException] thrownBy {
      elaborate { new MyModule(new RecordFromMap(listMap), new RecordFromMap(listMap - "foo")) }
    }).getMessage should include ("Right Record missing field")

    (the [ChiselException] thrownBy {
      elaborate { new MyModule(new RecordFromMap(listMap - "bar"), new RecordFromMap(listMap)) }
    }).getMessage should include ("Left Record missing field")
  }
}
