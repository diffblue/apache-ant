package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class Native2AsciiUtilsDiffblueTest {
  /**
   * Test {@link Native2AsciiUtils#native2ascii(String)}.
   * <p>
   * Method under test: {@link Native2AsciiUtils#native2ascii(String)}
   */
  @Test
  public void testNative2ascii() {
    // Arrange, Act and Assert
    assertEquals("Line", Native2AsciiUtils.native2ascii("Line"));
  }

  /**
   * Test {@link Native2AsciiUtils#ascii2native(String)}.
   * <p>
   * Method under test: {@link Native2AsciiUtils#ascii2native(String)}
   */
  @Test
  public void testAscii2native() {
    // Arrange, Act and Assert
    assertEquals("Line", Native2AsciiUtils.ascii2native("Line"));
  }
}
