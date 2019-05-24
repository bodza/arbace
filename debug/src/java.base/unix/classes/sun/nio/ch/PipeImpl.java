package sun.nio.ch;

import java.io.*;
import java.nio.channels.*;
import java.nio.channels.spi.*;

class PipeImpl extends Pipe {
    // Source and sink channels
    private final SourceChannel source;
    private final SinkChannel sink;

    PipeImpl(SelectorProvider sp) throws IOException {
        long pipeFds = IOUtil.makePipe(true);
        int readFd = (int) (pipeFds >>> 32);
        int writeFd = (int) pipeFds;
        FileDescriptor sourcefd = new FileDescriptor();
        IOUtil.setfdVal(sourcefd, readFd);
        source = new SourceChannelImpl(sp, sourcefd);
        FileDescriptor sinkfd = new FileDescriptor();
        IOUtil.setfdVal(sinkfd, writeFd);
        sink = new SinkChannelImpl(sp, sinkfd);
    }

    public SourceChannel source() {
        return source;
    }

    public SinkChannel sink() {
        return sink;
    }
}
