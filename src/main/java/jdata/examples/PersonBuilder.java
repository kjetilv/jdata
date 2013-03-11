package jdata.examples;

public interface PersonBuilder extends Builder<Person> {
    
    PersonBuilder setName(Name name);
    
    PersonBuilder setAddress(Address address);
}
