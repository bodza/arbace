package java.util.concurrent;

/**
 * A delayed result-bearing action that can be cancelled.
 * Usually a scheduled future is the result of scheduling
 * a task with a {@link ScheduledExecutorService}.
 *
 * @param <V> The result type returned by this Future
 */
public interface ScheduledFuture<V> extends Delayed, Future<V> {
}
