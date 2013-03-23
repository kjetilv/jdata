package jdata.examples;

import jdata.core.Builder;

public interface AddressBuilder extends Builder<Address> {
    
    AddressBuilder getStreetName(String streetName);
    
    AddressBuilder getStreetNo(String streetNo);
    
    AddressBuilder getZipCode(String zipCode);
    
    AddressBuilder getArea(String area);
}
