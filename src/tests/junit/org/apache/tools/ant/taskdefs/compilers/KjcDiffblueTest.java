package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class KjcDiffblueTest {
  /**
   * Test new {@link Kjc} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Kjc}
   */
  @Test
  public void testNewKjc() {
    // Arrange and Act
    Kjc actualKjc = new Kjc();

    // Assert
    assertNull(actualKjc.getProject());
    assertNull(actualKjc.getJavac());
  }
}
