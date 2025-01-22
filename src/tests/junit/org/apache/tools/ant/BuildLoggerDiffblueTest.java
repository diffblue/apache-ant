package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class BuildLoggerDiffblueTest {
  /**
   * Test {@link BuildLogger#getMessageOutputLevel()}.
   * <p>
   * Method under test: {@link BuildLogger#getMessageOutputLevel()}
   */
  @Test
  public void testGetMessageOutputLevel() {
    // Arrange, Act and Assert
    assertEquals(0, (new DefaultLogger()).getMessageOutputLevel());
  }
}
