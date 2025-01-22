package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class JvcDiffblueTest {
  /**
   * Test new {@link Jvc} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Jvc}
   */
  @Test
  public void testNewJvc() {
    // Arrange and Act
    Jvc actualJvc = new Jvc();

    // Assert
    assertNull(actualJvc.getProject());
    assertNull(actualJvc.getJavac());
  }
}
