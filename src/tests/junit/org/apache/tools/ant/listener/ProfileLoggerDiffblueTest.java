package org.apache.tools.ant.listener;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class ProfileLoggerDiffblueTest {
  /**
   * Test new {@link ProfileLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ProfileLogger}
   */
  @Test
  public void testNewProfileLogger() {
    // Arrange, Act and Assert
    assertEquals(0, (new ProfileLogger()).getMessageOutputLevel());
  }
}
