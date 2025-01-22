package org.apache.tools.ant.listener;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class TimestampedLoggerDiffblueTest {
  /**
   * Test new {@link TimestampedLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TimestampedLogger}
   */
  @Test
  public void testNewTimestampedLogger() {
    // Arrange, Act and Assert
    assertEquals(0, (new TimestampedLogger()).getMessageOutputLevel());
  }
}
