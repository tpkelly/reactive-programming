package nodescala

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("A future of any should be completed") {
    val any = Future.any(List(Future.never[Int], Future.always(3), Future.delay(5 seconds)))
    
    assert(Await.result(any, 0.5 seconds) == 3)
  }
  
  test("An empty list of any futures should not be completed") {
    val anyNever = Future.any(List())
    
    try {
      Await.result(anyNever, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  
  test("A list of all futures should return all completed futures") {
    val all = Future.all(List(Future.always(3), Future.always(5)))
    assert(Await.result(all, 1 second) == List(3, 5))
  }
  
  test("A list of all never futures should not be completed") {
    val allNever = Future.all(List(Future.never[Int], Future.never[Int]))
    
    try {
      Await.result(allNever, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  
  test("An empty list of all futures should not be completed") {
    val all = Future.all(List())
    assert(Await.result(all, 1 second) == List())
  }
  
  test("Delay does not execute immediately") {
    val delay = Future.delay(0.5 seconds)
    
    try {
      Await.result(delay, 0 nanos)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  
  test("Delay will execute eventually") {
    val delay = Future.delay(0.5 seconds)
    Await.result(delay, 1 second)
  }
  
  test("Now on unexecuted value is NoSuchElementException") {
    val never = Future.never[Int]
    Thread.sleep(100)
    
    try {
      never.now
      assert(false)
    } catch {
      case t: NoSuchElementException => // ok!
    }
  }
  
  test("Now on executed value returns that value") {
    val always = Future.always(400)
    Thread.sleep(100)
    assert(always.now == 400)
  }
  
  test("continueWith transforms value from one Future type to another") {
    val cont = (a:Future[Int]) => a.now.toString
    val original = Future.always(400)
    val transformed = original.continueWith[String](cont)
    
    Thread.sleep(100)
    assert(transformed.now == "400")
  }
  
  test("continue transforms value from result of one Future type to another") {
    val cont = (a:Try[Int]) => a.get.toString
    val original = Future.always(400)
    val transformed = original.continue[String](cont)
    
    Thread.sleep(100)
    assert(transformed.now == "400")
  }
  
  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }
}




