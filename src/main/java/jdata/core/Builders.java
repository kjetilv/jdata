package jdata.core;

public interface Builders {
    
    <T extends Access> Builder<T> getBuilder(Class<T> t);
}
