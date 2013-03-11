package jdata.examples;

import clojure.lang.Compiler;
import clojure.lang.RT;
import org.junit.Test;

import java.io.*;
import java.nio.charset.Charset;

public class DataTest {
    
    @Test
    public void newPerson() throws IOException, NoSuchFieldException {
        System.out.println(RT.baseLoader());
        File source = new File("src/main/clojure/data.clj");
        Reader rdr = new InputStreamReader(new FileInputStream(source), Charset.forName("UTF-8"));
        Object load;
        try {
            load = Compiler.load(rdr, "src/main/clojure/data.clj", "data.clj");
        } finally {
            rdr.close();
        }
        System.out.println(load);
        
        
    }
}
