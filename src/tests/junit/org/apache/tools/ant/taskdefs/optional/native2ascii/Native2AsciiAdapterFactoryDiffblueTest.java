package org.apache.tools.ant.taskdefs.optional.native2ascii;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class Native2AsciiAdapterFactoryDiffblueTest {
  /**
   * Test {@link Native2AsciiAdapterFactory#getDefault()}.
   * <p>
   * Method under test: {@link Native2AsciiAdapterFactory#getDefault()}
   */
  @Test
  public void testGetDefault() {
    // Arrange, Act and Assert
    assertEquals("builtin", Native2AsciiAdapterFactory.getDefault());
  }
}
