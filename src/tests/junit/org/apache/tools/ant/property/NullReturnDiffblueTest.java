package org.apache.tools.ant.property;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class NullReturnDiffblueTest {
  /**
   * Test {@link NullReturn#toString()}.
   * <p>
   * Method under test: {@link NullReturn#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("null", NullReturn.NULL.toString());
  }
}
