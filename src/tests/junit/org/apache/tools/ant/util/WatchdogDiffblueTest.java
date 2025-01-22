package org.apache.tools.ant.util;

import static org.junit.Assert.assertThrows;
import org.junit.Test;

public class WatchdogDiffblueTest {
  /**
   * Test {@link Watchdog#Watchdog(long)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Watchdog#Watchdog(long)}
   */
  @Test
  public void testNewWatchdog_whenZero_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new Watchdog(0L));
  }
}
