package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class Javac13DiffblueTest {
  /**
   * Test new {@link Javac13} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Javac13}
   */
  @Test
  public void testNewJavac13() {
    // Arrange and Act
    Javac13 actualJavac13 = new Javac13();

    // Assert
    assertNull(actualJavac13.getProject());
    assertNull(actualJavac13.getJavac());
  }
}
