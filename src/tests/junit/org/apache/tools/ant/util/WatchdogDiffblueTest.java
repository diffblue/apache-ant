package org.apache.tools.ant.util;

import static org.junit.Assert.assertThrows;
import org.junit.Test;

public class WatchdogDiffblueTest {
  /**
  * Method under test: {@link Watchdog#Watchdog(long)}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new Watchdog(-1L));
  }
}

