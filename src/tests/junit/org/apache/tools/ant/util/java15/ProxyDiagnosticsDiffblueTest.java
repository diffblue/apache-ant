package org.apache.tools.ant.util.java15;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ProxyDiagnosticsDiffblueTest {
  /**
   * Test {@link ProxyDiagnostics#ProxyDiagnostics(String)}.
   * <ul>
   *   <li>When {@code 42https://ant.apache.org/}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProxyDiagnostics#ProxyDiagnostics(String)}
   */
  @Test
  public void testNewProxyDiagnostics_when42httpsAntApacheOrg_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> new ProxyDiagnostics("42https://ant.apache.org/"));
  }

  /**
   * Test {@link ProxyDiagnostics#toString()}.
   * <ul>
   *   <li>Given {@link ProxyDiagnostics#ProxyDiagnostics()}.</li>
   *   <li>Then return {@code Direct connection}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProxyDiagnostics#toString()}
   */
  @Test
  public void testToString_givenProxyDiagnostics_thenReturnDirectConnection() {
    // Arrange, Act and Assert
    assertEquals("Direct connection\n", (new ProxyDiagnostics()).toString());
  }
}
