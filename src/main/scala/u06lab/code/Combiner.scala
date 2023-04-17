package u06lab.code
import scala.annotation.tailrec

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combineElements[T](e: T, elems: T*)(using combiner: Combiner[T]): T

object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double = a.sum
  override def concat(a: Seq[String]): String =
    if a.nonEmpty then a.reduce( _ ++ _)
    else ""
  override def max(a: List[Int]): Int =
    if a.nonEmpty then a.max
    else Int.MinValue
  @tailrec
  override def combineElements [T: Combiner](elem: T, elems: T*): T =
    if elems.isEmpty then
      summon[Combiner[T]].unit
    else
      if elems.tail.nonEmpty then
        combineElements(summon[Combiner[T]].combine(elem, elems.head), elems.tail.head)
      else
        summon[Combiner[T]].combine(elem, elems.head)





/*
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 *   to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 *   the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

object GivenCombiners:
  given Combiner[Double] with
    override def combine(a: Double, b: Double): Double = a + b
    override def unit: Double = 0.0
  given Combiner[String] with
    override def combine(a: String, b: String): String = a ++ b
    override def unit: String = ""
  given Combiner[Int] with
    override def combine(a: Int, b: Int): Int = if a > b then a else b
    override def unit: Int = Int.MinValue

import GivenCombiners.given



@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648

  println(f.combineElements(10.0, 20.0, 30.1)) // 60.1
  println(f.combineElements(0.0)) // 0.0
  println(f.combineElements("a", "b", "c")) // abc
  println(f.combineElements("")) // ""
  println(f.combineElements(-10, 3, -5, 0)) // 3
  println(f.combineElements(0)) // -2147483648
