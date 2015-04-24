package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  /** Polynomial */
  
  test("computeDelta with a constant delta") {
    val deltaSignal = Polynomial.computeDelta(Var(1), Var(3), Var(2))
    assert(deltaSignal() == 1.0)
    
    val deltaSignal2 = Polynomial.computeDelta(Var(1), Var(5), Var(1))
    assert(deltaSignal2() == 21.0)
    
    val deltaSignal3 = Polynomial.computeDelta(Var(1), Var(0), Var(1))
    assert(deltaSignal3() == -4.0)
  }
  
  test("computeSolutions with 2 roots") {
    val a = Var(1.0)
    val b = Var(3.0)
    val c = Var(-4.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val compute = Polynomial.computeSolutions(a, b, c, delta)
    
    assert(compute() == Set(1.0, -4.0))
  }

  test("computeSolutions with 1 root") {
    val a = Var(1.0)
    val b = Var(2.0)
    val c = Var(1.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val compute = Polynomial.computeSolutions(a, b, c, delta)
    
    assert(compute() == Set(-1.0))
  }
  
  test("computeSolutions with no roots") {
    val a = Var(1.0)
    val b = Var(0.0)
    val c = Var(1.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val compute = Polynomial.computeSolutions(a, b, c, delta)
    
    assert(compute() == Set())
  }
  
  /** Calculator */
  
  test("eval literal") {
    val a = Literal(5.0)
    assert(Calculator.eval(a, Map()) == 5.0)
    
    val b = Literal(3.5)
    assert(Calculator.eval(b, Map()) == 3.5)
    
    val c = Literal(-0.4)
    assert(Calculator.eval(c, Map()) == -0.4)
  }
  
  test("eval simple operations") {
    val a = Literal(10.0)
    val b = Literal(4.0)
    
    val plus = Plus(a, b)
    val minus = Minus(a, b)
    val times = Times(a, b)
    val divide = Divide(a, b)
    
    assert(Calculator.eval(plus, Map()) == 14.0)
    assert(Calculator.eval(minus, Map()) == 6.0)
    assert(Calculator.eval(times, Map()) == 40.0)
    assert(Calculator.eval(divide, Map()) == 2.5)
  }
  
  test("eval div by 0 is NaN") {
    val a = Literal(1.0)
    val b = Literal(0.0)
    
    val divide = Divide(a, b)
    
    assert(Calculator.eval(divide, Map()).equals(Double.NaN))
  }
  
  test("eval simple ref") {
    val a = Literal(10.0)
    val ref = Ref("a")
    
    assert(Calculator.eval(ref, Map("a" -> Var(a))) == 10.0)
  }
  
  test("eval missing ref") {
    val a = Literal(10.0)
    val ref = Ref("b")
    
    // (NaN == NaN) is false, use .equals() instead
    assert(Calculator.eval(ref, Map("a" -> Var(a))).equals(Double.NaN))
  }
  
  test("computeValues") {
    val a = Literal(10.0)
    val b = Ref("a")
    
    val map = Map[String, Signal[Expr]]("a" -> Var(a), "b" -> Var(b))
    
    val expectedValues = Map[String, Signal[Double]]("a" -> Var(10.0), "b" -> Var(10.0))
    val observedValues = Calculator.computeValues(map);
    
    assert(expectedValues.get("a").get() == observedValues.get("a").get())
    assert(expectedValues.get("b").get() == observedValues.get("b").get())
  }
  
  test("computeValues with cyclical references") {
    val a = Times(Literal(2.0), Ref("b"))
    val b = Plus(Ref("a"), Literal(1.0))
    val map = Map[String, Signal[Expr]]("a" -> Var(a), "b" -> Var(b))
    
    val observedValues = Calculator.computeValues(map)
    
    assert(observedValues.get("a").get().equals(Double.NaN))
    assert(observedValues.get("b").get().equals(Double.NaN))
  }
  
}
