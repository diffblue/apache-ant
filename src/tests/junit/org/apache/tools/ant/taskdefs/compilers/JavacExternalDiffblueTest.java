package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class JavacExternalDiffblueTest {
  /**
   * Test new {@link JavacExternal} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JavacExternal}
   */
  @Test
  public void testNewJavacExternal() {
    // Arrange and Act
    JavacExternal actualJavacExternal = new JavacExternal();

    // Assert
    assertNull(actualJavacExternal.getProject());
    assertNull(actualJavacExternal.getJavac());
  }
}
