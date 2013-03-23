package jdata.examples;

import jdata.core.Builder;

public interface NameBuilder extends Builder<Name> {
    
    NameBuilder setFirstName(String firstName);
    
    NameBuilder setMiddleName(String middleName);
    
    NameBuilder setLastName(String lastName);
}
