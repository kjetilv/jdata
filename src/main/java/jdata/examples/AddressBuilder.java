package jdata.examples;

public interface AddressBuilder extends Builder<Address> {
    
    AddressBuilder getStreetName(String streetName);
    
    AddressBuilder getStreetNo(String streetNo);
    
    AddressBuilder getZipCode(String zipCode);
    
    AddressBuilder getArea(String area);
}
