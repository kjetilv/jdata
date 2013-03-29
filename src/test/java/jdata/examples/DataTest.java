package jdata.examples;

import jdata.core.Builder;
import jdata.core.Builders;
import jdata.core.BuildersProvider;
import org.junit.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class DataTest {
    
    @Test 
    public void newPerson() throws IOException, NoSuchFieldException {
        BuildersProvider provider = new data.ClojureBuildersProvider();
        System.out.println(provider);
        List<Class<? extends Builder<?>>> classes = Arrays.asList
                (NameBuilder.class, AddressBuilder.class, PersonBuilder.class);
        
        Builders builders = provider.getBuilders(classes);
        System.out.println("Provider: " + builders);

        Builder<Name> builder = builders.getBuilder(Name.class);
        System.out.println("NameBuilder: " + builder);
        
        Name name = ((NameBuilder)builder).setFirstName("Kjetil").setLastName("V").build();
        System.out.println("Name: " + name.get());
    }
}
