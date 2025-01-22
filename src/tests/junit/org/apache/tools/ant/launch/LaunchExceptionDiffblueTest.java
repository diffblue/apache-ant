package org.apache.tools.ant.launch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class LaunchExceptionDiffblueTest {
  /**
   * Test {@link LaunchException#LaunchException(String)}.
   * <p>
   * Method under test: {@link LaunchException#LaunchException(String)}
   */
  @Test
  public void testNewLaunchException() {
    // Arrange and Act
    LaunchException actualLaunchException = new LaunchException("0123456789ABCDEF");

    // Assert
    assertEquals("0123456789ABCDEF", actualLaunchException.getMessage());
    assertNull(actualLaunchException.getCause());
    assertEquals(0, actualLaunchException.getSuppressed().length);
  }
}
