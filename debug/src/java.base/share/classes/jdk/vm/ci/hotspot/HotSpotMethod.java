package jdk.vm.ci.hotspot;

import jdk.vm.ci.meta.JavaMethod;

abstract class HotSpotMethod implements JavaMethod {
    public JavaMethod asJavaMethod() {
        return this;
    }
}
