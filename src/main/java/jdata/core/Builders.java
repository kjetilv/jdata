package jdata.core;

public interface Builders {
    
    <T extends Access> Builder<T> get(Class<T> t);
}
