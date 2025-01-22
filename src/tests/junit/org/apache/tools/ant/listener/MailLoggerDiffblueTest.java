package org.apache.tools.ant.listener;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class MailLoggerDiffblueTest {
  /**
   * Test new {@link MailLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MailLogger}
   */
  @Test
  public void testNewMailLogger() {
    // Arrange, Act and Assert
    assertEquals(0, (new MailLogger()).getMessageOutputLevel());
  }
}
