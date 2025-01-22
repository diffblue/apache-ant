package org.apache.tools.ant.listener;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class AnsiColorLoggerDiffblueTest {
  /**
   * Test new {@link AnsiColorLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AnsiColorLogger}
   */
  @Test
  public void testNewAnsiColorLogger() {
    // Arrange, Act and Assert
    assertEquals(0, (new AnsiColorLogger()).getMessageOutputLevel());
  }
}
