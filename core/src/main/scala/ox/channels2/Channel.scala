package ox.channels2

import java.util.concurrent.{ArrayBlockingQueue, ConcurrentSkipListSet, CountDownLatch, Semaphore, SynchronousQueue}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference, AtomicReferenceArray}
import java.util.concurrent.locks.LockSupport
import scala.annotation.tailrec

class Channel[T]:
  private val senders = new AtomicLong(0L)
  private val receivers = new AtomicLong(0L)
  private val buffer: AtomicReferenceArray[State] = AtomicReferenceArray[State](20_000_000) // TODO

  //

  private object Interrupted
  private class Continuation[E]():
    private val creatingThread = Thread.currentThread()
    private val data = new AtomicReference[E | Interrupted.type | Null]()

    def tryResume(e: E): Boolean =
      val result = data.compareAndSet(null, e)
      LockSupport.unpark(creatingThread)
      result

    def await(onInterrupt: () => Unit): E =
      var spinIterations = 1000
      while data.get() == null do
        if spinIterations > 0 then
          Thread.onSpinWait()
          spinIterations -= 1
        else LockSupport.park()

        if Thread.interrupted() then
          data.compareAndSet(null, Interrupted) // TODO if
          val e = new InterruptedException()

          try onInterrupt()
          catch case ee: Throwable => e.addSuppressed(ee)

          throw e

      data.get().asInstanceOf[E] // another thread has just inserted E

  //

  private enum State: // null == Empty
    case Buffered(v: T)
    case Done
    case SuspendedSend(v: T, c: Continuation[Unit])
    case SuspendedReceive(c: Continuation[T])
    case Broken
    case Interrupted

  def send(v: T): Unit =
    while
      val s = senders.getAndIncrement()
      !updateCellSend(s, v)
    do ()

  /** @return false if [[send]] should restart. */
  @tailrec private def updateCellSend(s: Long, v: T): Boolean =
    val state = buffer.get(s.toInt)
    val r = receivers.get()

    state match
      case null if s >= r =>
        val cont = Continuation[Unit]()
        val newState = State.SuspendedSend(v, cont)
        if buffer.compareAndSet(s.toInt, null, newState) then
          cont.await(() => buffer.set(s.toInt, State.Interrupted))
          true // don't restart send
        else updateCellSend(s, v) // read state again

      case null /* s < r */ =>
        if buffer.compareAndSet(s.toInt, null, State.Buffered(v)) then true
        else updateCellSend(s, v)

      case State.SuspendedReceive(c) =>
        if c.tryResume(v)
        then
          buffer.set(s.toInt, State.Done)
          true
        else false

      case State.Broken | State.Interrupted                           => false
      case State.Done | State.Buffered(_) | State.SuspendedSend(_, _) => throw new IllegalStateException(state.toString)

  private object Restart
  @tailrec final def receive(): T =
    val r = receivers.getAndIncrement()
    updateCellReceive(r) match
      case Restart => receive()
      case v: T    => v

  /** @return false if [[receive]] should restart. */
  @tailrec private def updateCellReceive(r: Long): T | Restart.type =
    val state = buffer.get(r.toInt)
    val s = senders.get()

    state match
      case null if r >= s =>
        val cont = Continuation[T]()
        if buffer.compareAndSet(r.toInt, null, State.SuspendedReceive(cont))
        then cont.await(() => buffer.set(r.toInt, State.Interrupted))
        else updateCellReceive(r)
      case null /* r < s */ => if buffer.compareAndSet(r.toInt, null, State.Broken) then Restart else updateCellReceive(r)
      case State.SuspendedSend(v, cont) =>
        if cont.tryResume(())
        then
          buffer.set(r.toInt, State.Done)
          v
        else Restart
      case State.Buffered(v)                                                         => v
      case State.Interrupted                                                         => Restart
      case State.Done | State.Buffered(_) | State.SuspendedReceive(_) | State.Broken => throw new IllegalStateException(state.toString)

//

// simple send-receive, validate results
@main def test(): Unit =
  import ox.*
  val c = Channel[Int]()
  supervised {
    fork { c.send(5) }
    fork { println(c.receive()) }
  }

// send-receive 1k elements, validate results
@main def test2(): Unit =
  import ox.*
  val c = Channel[Int]()
  val s = new ConcurrentSkipListSet[Int]()
  supervised {
    for (i <- 1 to 1000) fork(c.send(i))
    for (i <- 1 to 1000) fork(s.add(c.receive()))
  }
  println(s.size)

// create 1k sending and 1k receiving forks, validate results
@main def test3(): Unit =
  import ox.*
  val c = Channel[Int]()
  val s = new ConcurrentSkipListSet[Int]()
  supervised {
    for (i <- 1 to 1000) {
      fork(c.send(i))
      fork(s.add(c.receive()))
    }
  }
  println(s.size)
