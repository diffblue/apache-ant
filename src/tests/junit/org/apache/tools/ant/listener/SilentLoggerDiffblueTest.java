package org.apache.tools.ant.listener;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class SilentLoggerDiffblueTest {
  /**
   * Test new {@link SilentLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SilentLogger}
   */
  @Test
  public void testNewSilentLogger() {
    // Arrange, Act and Assert
    assertEquals(0, (new SilentLogger()).getMessageOutputLevel());
  }
}
