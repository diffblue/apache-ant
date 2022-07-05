package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import org.junit.Test;

public class KeepAliveInputStreamDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link KeepAliveInputStream#KeepAliveInputStream(InputStream)}
  *   <li>{@link KeepAliveInputStream#close()}
  * </ul>
  */
  @Test
  public void testConstructor() throws IOException {
    // Arrange and Act
    KeepAliveInputStream actualKeepAliveInputStream = new KeepAliveInputStream(
        new ByteArrayInputStream("AAAAAAAA".getBytes("UTF-8")));
    actualKeepAliveInputStream.close();

    // Assert that nothing has changed
    assertEquals(8, actualKeepAliveInputStream.available());
  }

  /**
   * Method under test: {@link KeepAliveInputStream#wrapSystemIn()}
   */
  @Test
  public void testWrapSystemIn() {
    // Arrange, Act and Assert
    assertTrue(KeepAliveInputStream.wrapSystemIn() instanceof KeepAliveInputStream);
  }
}

