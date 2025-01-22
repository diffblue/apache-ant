package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class GcjDiffblueTest {
  /**
   * Test new {@link Gcj} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Gcj}
   */
  @Test
  public void testNewGcj() {
    // Arrange and Act
    Gcj actualGcj = new Gcj();

    // Assert
    assertNull(actualGcj.getProject());
    assertNull(actualGcj.getJavac());
  }
}
