package ox.flow

import ox.Ox
import ox.channels.ChannelClosed
import ox.channels.Sink
import ox.channels.Source
import ox.channels.StageCapacity
import ox.discard
import ox.repeatWhile

import ox.channels.forkPropagate

/** Describes an asynchronous transformation pipeline emitting elements of type `T`. */
class Flow[+T](protected val last: FlowStage[T]) extends FlowOps[T] with FlowRunOps[T]:
  def async()(using StageCapacity): Flow[T] =
    Flow(
      new FlowStage:
        override def run(sink: FlowSink[T])(using Ox): Unit =
          val ch = StageCapacity.newChannel[T]
          runLastToChannelAsync(ch)
          FlowStage.fromSource(ch).run(sink)
    )

  //

  protected def runLastToChannelAsync(ch: Sink[T])(using Ox): Unit =
    forkPropagate(ch)(last.run(FlowSink.ToChannel(ch))).discard
end Flow

object Flow extends FlowCompanionOps

//

trait FlowStage[+T]:
  def run(sink: FlowSink[T])(using Ox): Unit

object FlowStage:
  def fromSource[T](source: Source[T]): FlowStage[T] =
    new FlowStage[T]:
      def run(next: FlowSink[T])(using Ox): Unit =
        repeatWhile:
          val t = source.receiveOrClosed()
          t match
            case ChannelClosed.Done     => next.onDone(); false
            case ChannelClosed.Error(r) => next.onError(r); false
            case t: T @unchecked        => next.onNext(t); true

//

trait FlowSink[-T]:
  def onNext(t: T): Unit
  def onDone(): Unit
  def onError(e: Throwable): Unit

object FlowSink:
  /** Creates a sink which sends all elements to the given channel. Any closure events are propagated as well. */
  class ToChannel[T](ch: Sink[T]) extends FlowSink[T]:
    override def onNext(t: T): Unit = ch.send(t)
    override def onDone(): Unit = ch.done()
    override def onError(e: Throwable): Unit = ch.error(e)

  /** Creates a new sink which runs the provided callback when a new element is received. Closure events are propagated to the given sink.
    */
  inline def propagateClose[T](inline next: FlowSink[_])(inline runOnNext: T => Unit): FlowSink[T] =
    new FlowSink[T]:
      override def onNext(t: T): Unit = runOnNext(t)
      override def onDone(): Unit = next.onDone()
      override def onError(e: Throwable): Unit = next.onError(e)
