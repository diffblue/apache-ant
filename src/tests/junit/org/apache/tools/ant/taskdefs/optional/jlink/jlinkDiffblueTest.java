package org.apache.tools.ant.taskdefs.optional.jlink;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class jlinkDiffblueTest {
  /**
   * Test new {@link jlink} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link jlink}
   */
  @Test
  public void testNewJlink() {
    // Arrange, Act and Assert
    assertEquals(8192, (new jlink()).buffer.length);
  }
}
