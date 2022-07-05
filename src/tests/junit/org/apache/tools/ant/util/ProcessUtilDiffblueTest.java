package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class ProcessUtilDiffblueTest {
  /**
  * Method under test: {@link ProcessUtil#getProcessId(String)}
  */
  @Test
  public void testGetProcessId() {
    // Arrange, Act and Assert
    assertEquals("72595", ProcessUtil.getProcessId("Fallback"));
  }
}

