package jdata.examples;

import jdata.core.Access;

public interface Address extends Access {
    
    String getStreetName();
    
    String getStreetNo();
    
    String getZipCode();
    
    String getArea();
}
