package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class JikesDiffblueTest {
  /**
   * Test new {@link Jikes} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Jikes}
   */
  @Test
  public void testNewJikes() {
    // Arrange and Act
    Jikes actualJikes = new Jikes();

    // Assert
    assertNull(actualJikes.getProject());
    assertNull(actualJikes.getJavac());
  }
}
