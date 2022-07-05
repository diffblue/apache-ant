package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class JAXPUtilsDiffblueTest {
  /**
  * Method under test: {@link JAXPUtils#getSystemId(File)}
  */
  @Test
  public void testGetSystemId() {
    // Arrange and Act
    String actualSystemId = JAXPUtils.getSystemId(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals(String.join("", "file:", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()),
        actualSystemId);
  }
}

