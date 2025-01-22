package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class Javac12DiffblueTest {
  /**
   * Test new {@link Javac12} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Javac12}
   */
  @Test
  public void testNewJavac12() {
    // Arrange and Act
    Javac12 actualJavac12 = new Javac12();

    // Assert
    assertNull(actualJavac12.getProject());
    assertNull(actualJavac12.getJavac());
  }
}
