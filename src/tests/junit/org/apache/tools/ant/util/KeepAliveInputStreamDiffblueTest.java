package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import org.junit.Test;

public class KeepAliveInputStreamDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link KeepAliveInputStream#KeepAliveInputStream(InputStream)}
   *   <li>{@link KeepAliveInputStream#close()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws IOException {
    // Arrange and Act
    KeepAliveInputStream actualKeepAliveInputStream = new KeepAliveInputStream(
        new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    actualKeepAliveInputStream.close();

    // Assert
    assertEquals(8, actualKeepAliveInputStream.read(new byte[8]));
  }
}
