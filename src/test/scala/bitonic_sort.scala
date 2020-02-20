package bitonic_sort

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}

class ExhaustiveSorterTest[T <: Bool]( factory : () => SorterModuleIfc[T]) extends FlatSpec with Matchers {
  behavior of "SorterTest"

  it should "work" in {
    chisel3.iotesters.Driver( factory, "vcs") { c =>
      new PeekPokeTester( c) {
        def example( a:IndexedSeq[BigInt]) {
          poke( c.io.a, a)
          step(1)
          expect( c.io.z, a.sortWith( _>_))
        }

        for { i <- 0 to c.n } {
          for { on <-(0 until c.n).toIndexedSeq.combinations(i)} {
            val v = on.foldLeft( IndexedSeq.fill(c.n){ BigInt(0)}){ case (x,y) =>
              x updated (y,BigInt(1))
            }
            example( v)
          }
        }

      }
    } should be (true)
  }
}

class RandomSorterTest[T <: UInt]( val numExamples : Int, factory : () => SorterModuleIfc[T]) extends FlatSpec with Matchers {
  behavior of "SorterTest"

  it should "work" in {
    chisel3.iotesters.Driver( factory, "treadle") { c =>
      new PeekPokeTester( c) {
        def example( a:IndexedSeq[BigInt]) {
          poke( c.io.a, a)
          step(1)
          expect( c.io.z, a.sortWith( _>_))
        }

        for { i <- 0 until numExamples } {
          val a = IndexedSeq.fill(c.n)( BigInt( c.io.a(0).getWidth, rnd))
          example( a)
        }

      }
    } should be (true)
  }
}

class BoolBitonicSorterModule( n : Int) extends BitonicSorterModule( n, Bool(), (x:UInt,y:UInt)=>x<y)
class UInt8BitonicSorterModule( n : Int) extends BitonicSorterModule( n, UInt(8.W), (x:UInt,y:UInt)=>x<y)

class ExhaustiveBitonicSorterTest2  extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(2))
class ExhaustiveBitonicSorterTest3  extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(3))
class ExhaustiveBitonicSorterTest4  extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(4))
class ExhaustiveBitonicSorterTest6  extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(6))
class ExhaustiveBitonicSorterTest8  extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(8))
class ExhaustiveBitonicSorterTest12 extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(12))
class ExhaustiveBitonicSorterTest16 extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(16))
//class ExhaustiveBitonicSorterTest20 extends ExhaustiveSorterTest( () => new BoolBitonicSorterModule(20))

class RandomBitonicSorterTest64  extends RandomSorterTest( 10000, () => new UInt8BitonicSorterModule( 64))
class RandomBitonicSorterTest384 extends RandomSorterTest( 10000, () => new UInt8BitonicSorterModule( 384))
