package jdata.examples;

import jdata.core.Builder;

public interface PersonBuilder extends Builder<Person> {
    
    PersonBuilder setName(Name name);
    
    PersonBuilder setAddress(Address address);
}
