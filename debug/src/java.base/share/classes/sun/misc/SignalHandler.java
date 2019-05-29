package sun.misc;

/**
 * This is the signal handler interface expected in <code>Signal.handle</code>.
 */
public interface SignalHandler {
    /**
     * The default signal handler
     */
    public static final SignalHandler SIG_DFL = new Signal.SunMiscHandler(null, jdk.internal.misc.Signal.Handler.SIG_DFL);
    /**
     * Ignore the signal
     */
    public static final SignalHandler SIG_IGN = new Signal.SunMiscHandler(null, jdk.internal.misc.Signal.Handler.SIG_IGN);

    /**
     * Handle the given signal
     *
     * @param sig a signal object
     */
    public void handle(Signal sig);
}
