package jdata.examples;

import jdata.core.Access;

public interface Person extends Access {
    
    Name getName();
    
    Address getAddress();
}
