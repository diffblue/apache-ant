package org.apache.tools.ant.util.java15;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ProxyDiagnosticsDiffblueTest {
  /**
  * Method under test: {@link ProxyDiagnostics#ProxyDiagnostics(String)}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> new ProxyDiagnostics("42https://ant.apache.org/"));
  }

  /**
   * Method under test: {@link ProxyDiagnostics#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("Direct connection\n", (new ProxyDiagnostics()).toString());
  }
}

