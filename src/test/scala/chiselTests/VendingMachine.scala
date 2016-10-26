// See LICENSE for license details.

package chiselTests

import chisel3._
import chisel3.util._
import chisel3.testers.BasicTester

abstract class VendingMachine extends Module {
  val io = IO(new Bundle {
    val nickel = Input(Bool())
    val dime   = Input(Bool())
    val valid  = Output(Bool())
  })
}

class VendingMachineFSM extends VendingMachine {
  val sIdle :: s5 :: s10 :: s15 :: sOk :: Nil = Enum(UInt(), 5)
  val state = Reg(init = sIdle)

  when (state === sIdle) {
    when (io.nickel) { state := s5 }
    when (io.dime)   { state := s10 }
  }
  when (state === s5) {
    when (io.nickel) { state := s10 }
    when (io.dime)   { state := s15 }
  }
  when (state === s10) {
    when (io.nickel) { state := s15 }
    when (io.dime)   { state := sOk }
  }
  when (state === s15) {
    when (io.nickel) { state := sOk }
    when (io.dime)   { state := sOk }
  }
  when (state === sOk) {
    state := sIdle
  }
  io.valid := (state === sOk)
}

class VendingMachineNoFSM extends VendingMachine {
  val MaxValue = 25 // Nickel + Dime + Dime
  val SodaCost = 20

  val value = Reg(init = UInt(0, log2Up(MaxValue)))
  val incValue = Wire(init = UInt(0, log2Up(MaxValue)))
  val dispense = value >= SodaCost.U

  when (dispense) {
    value := 0.U // No change
  } .otherwise {
    value := value + incValue
  }

  when (io.nickel) { incValue := 5.U }
  when (io.dime) { incValue := 10.U }
  io.valid := dispense
}

object VendingMachineUtils {
  abstract class Coin(val value: Int)
  case object NoCoin extends Coin(0)
  case object Nickel extends Coin(5)
  case object Dime extends Coin(10)

  // Calculate expected outputs by accumulating inputs, reseting to 0 when $.20 is reached
  def getExpectedResults(inputs: Seq[Coin]): Seq[Boolean] = {
    inputs.scanLeft((0, false)) { // (coin sum, soda dispensed)
      case ((in, _), x) => if (in + x.value >= 20) (0, true) else (in + x.value, false)
    } map (_._2) // drop the current value, just get expected output
  }

}

/** This tester is parameterized with a given implementation of [[VendingMachine]]
  * We pass mod by reference so that we only invoke the Chisel construction of the Module once
  */
class VendingMachineTester(mod: => VendingMachine) extends BasicTester {
  import VendingMachineUtils._

  val (cycle, done) = Counter(true.B, 11)
  when (done) { stop() ; stop() } // Double stop because Verilator requires 2

  // Construct mod
  val dut = mod

  // Inputs and expected results
  val inputs: Seq[Coin] = Seq(Nickel, Dime, Dime, NoCoin, Nickel, Nickel, Nickel, Nickel, Dime, Dime)
  val expected: Seq[Boolean] = getExpectedResults(inputs)

  // Create the actual hardware test
  dut.io.nickel := false.B
  dut.io.dime := false.B

  for (i <- 0 until 10) {
    when (cycle === i.U) {
      inputs(i) match {
        case NoCoin => // do nothing
        case Nickel => dut.io.nickel := true.B
        case Dime => dut.io.dime := true.B
      }
      assert(dut.io.valid === expected(i).B)
    }
  }
}

class VendingMachineSpec extends ChiselFlatSpec {
  behavior of "VendingMachine"

  it should "work with an explicit FSM" in {
    assertTesterPasses { new VendingMachineTester(Module(new VendingMachineFSM)) }
  }

  it should "work without an explicit FSM" in {
    assertTesterPasses { new VendingMachineTester(Module(new VendingMachineNoFSM)) }
  }
}
