package jdata.core;

public interface BuildersProvider {
    
    Builders getBuilders(Iterable<Class<? extends Builder<?>>> classes);
}
