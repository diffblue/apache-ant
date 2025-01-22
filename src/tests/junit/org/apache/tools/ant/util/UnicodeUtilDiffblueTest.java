package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class UnicodeUtilDiffblueTest {
  /**
   * Test {@link UnicodeUtil#EscapeUnicode(char)}.
   * <p>
   * Method under test: {@link UnicodeUtil#EscapeUnicode(char)}
   */
  @Test
  public void testEscapeUnicode() {
    // Arrange, Act and Assert
    assertEquals("u0041", UnicodeUtil.EscapeUnicode('A').toString());
  }
}
