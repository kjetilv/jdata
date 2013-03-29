package jdata.core;

public interface Builders {
    
    <T extends Access, B extends Builder<T>> B getBuilder(Class<B> t);
}
