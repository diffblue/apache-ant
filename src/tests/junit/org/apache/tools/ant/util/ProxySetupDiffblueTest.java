package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class ProxySetupDiffblueTest {
  /**
  * Method under test: {@link ProxySetup#getSystemProxySetting()}
  */
  @Test
  public void testGetSystemProxySetting() {
    // Arrange and Act
    String actualSystemProxySetting = ProxySetup.getSystemProxySetting();

    // Assert
    assertEquals(Boolean.TRUE.toString(), actualSystemProxySetting);
  }
}

