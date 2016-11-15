// See LICENSE for license details.

package chiselTests

import chisel3._

class BundleSpec extends ChiselFlatSpec {
  class BundleFooBar extends Bundle {
    val foo = UInt.width(32)
    val bar = UInt.width(32)
    override def cloneType = (new BundleFooBar).asInstanceOf[this.type]
  }
  class BundleBarFoo extends Bundle {
    val bar = UInt.width(32)
    val foo = UInt.width(32)
    override def cloneType = (new BundleBarFoo).asInstanceOf[this.type]
  }
  class BundleFoo extends Bundle {
    val foo = UInt.width(32)
    override def cloneType = (new BundleFoo).asInstanceOf[this.type]
  }
  class BundleBar extends Bundle {
    val bar = UInt.width(32)
    override def cloneType = (new BundleBar).asInstanceOf[this.type]
  }

  def progBundleFooBar = Bundle("foo" -> UInt.width(32), "bar" -> UInt.width(32))

  class MyModule(output: Bundle, input: Bundle) extends Module {
    val io = IO(new Bundle {
      val in = Input(input.cloneType)
      val out = Output(output.cloneType)
    })
    io.out <> io.in
  }

  "Bundles with the same fields but in different orders" should "work" in {
    elaborate { new MyModule(new BundleFooBar, new BundleBarFoo) }
  }

  "Bundles with the same fields as programmatic Bundle" should "work" in {
    elaborate { new MyModule(new BundleFooBar, progBundleFooBar) }
  }

  "Bulk connect on Bundles" should "check that the fields match" in {
    (the [ChiselException] thrownBy {
      elaborate { new MyModule(new BundleFooBar, new BundleFoo) }
    }).getMessage should include ("Right Bundle missing field")

    (the [ChiselException] thrownBy {
      elaborate { new MyModule(new BundleFoo, new BundleFooBar) }
    }).getMessage should include ("Left Bundle missing field")
  }
}
